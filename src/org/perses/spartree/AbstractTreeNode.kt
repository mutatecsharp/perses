/*
 * Copyright (C) 2018-2022 University of Waterloo.
 *
 * This file is part of Perses.
 *
 * Perses is free software; you can redistribute it and/or modify it under the
 * terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3, or (at your option) any later version.
 *
 * Perses is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * Perses; see the file LICENSE.  If not see <http://www.gnu.org/licenses/>.
 */
package org.perses.spartree

import com.google.common.collect.ImmutableList
import org.perses.util.SimpleQueue
import org.perses.util.SimpleStack
import org.perses.util.Util
import java.util.function.Predicate

abstract class AbstractTreeNode<T : AbstractTreeNode<T, Payload>, Payload>
protected constructor(val nodeId: Int) : Comparable<T> {

  protected val children = ArrayList<T>()

  /**
   * Return an immutable view of the children. The underlying children list of this node can still
   * change.
   *
   * @return an immutable view of the children.
   */
  val immutableChildView = children as List<T>
  var isPermanentlyDeleted = false
    private set

  /**
   * Get the parent information of the current node.
   *
   * @return the parent of the current Node.
   */
  var parent: T? = null
    private set(value) {
      check(value == null || field == null) { "can only set once." }
      field = value
    }

  var payload: Payload? = null
    private set(value) {
      check(value == null || field == null) { "can only set once. $payload" }
      field = value
    }

  fun hasChildren(): Boolean {
    return childCount > 0
  }

  fun hasNoChild() = !hasChildren()

  val childCount: Int
    get() = children.size

  /** Return the child at the specified index.  */
  fun getChild(index: Int): T {
    return children[index]
  }

  fun copyAndReverseChildren(): ImmutableList<T> {
    val builder: ImmutableList.Builder<T> = ImmutableList.builder()
    val length = children.size
    for (i in length - 1 downTo 0) {
      builder.add(children[i])
    }
    return builder.build()
  }

  fun copyChildren(): ImmutableList<T> {
    return ImmutableList.copyOf(children)
  }

  fun cleanDeletedImmediateChildren() {
    assert(checkNodeIntegrity() == null) { checkNodeIntegrity()!! }
    Util.removeElementsFromList(children) { _, element ->
      element.isPermanentlyDeleted
    }
    assert(checkNodeIntegrity() == null) { checkNodeIntegrity()!! }
  }

  /**
   * Remove a child from the current node.
   *
   * Note that the child's parent must be the current node.
   *
   * @param child, a tree node whose parent is the current node.
   */
  fun removeChild(child: T) {
    require(child.parent === this) {
      "The child ($child) is not a child of this node ($this)"
    }
    val index = children.indexOf(child)
    assert(index >= 0) { index }
    val removedChild = children.removeAt(index)
    assert(removedChild === child) {
      "The child is not in the child list of this node. Child:$child, This:$this"
    }
    child.resetParent()
    onChildRemoved(index, removedChild)
    assert(checkNodeIntegrity() == null) { checkNodeIntegrity()!! }
  }

  protected abstract fun onChildRemoved(index: Int, child: T)

  /**
   * Permanently delete this node, and its children recursively.
   *
   *
   * Note that this method does not remove the current node from its parent. This behavior is
   * intended, as program reduction traverses the tree from the top to the bottom.
   *
   *
   *
   *
   *
   * Package-private, so that this can only be called within this package.
   */
  fun delete() {
    if (isPermanentlyDeleted) {
      return
    }
    postOrderVisit { it.deleteCurrentNode() }
  }

  protected open fun deleteCurrentNode() {
    isPermanentlyDeleted = true
    children.clear()
  }

  open fun addChild(child: T, payload: Payload) {
    assert(child.parent == null) {
      "The parent of the parameter " + child + " is not null: " + child.parent
    }
    children.add(child)
    @Suppress("UNCHECKED_CAST")
    child.parent = this as T
    child.payload = payload

    assert(child.parent === this)
    assert(child.payload === payload)
  }

  override fun compareTo(other: T): Int {
    return nodeId.compareTo(other.nodeId)
  }

  /** Visit each child of this node.  */
  fun forEachChild(consumer: (node: T) -> Unit) {
    children.forEach(consumer)
  }

  /** Clear the parent of the current node to `null`.  */
  fun resetParent() {
    parent = null
  }

  fun resetPayload() {
    payload = null
  }

  /**
   * @return null if there is no error.
   */
  protected abstract fun checkNodeIntegrity(): ErrorMessage?

  /**
   * Replace the old child with the new child. The old child should be already in the child list,
   * and the parent of the new child should be null.
   *
   *
   *
   *
   *
   * TODO: need to check whether the newChild is assignable to `oldChild`
   *
   * @param oldChild, the existing child to be removed from the list.
   * @param newChild, the new child to be added in the position of the old child.
   */
  fun replaceChild(oldChild: T, newChild: T, payload: Payload) {
    assert(checkNodeIntegrity() == null) { checkNodeIntegrity()!! }
    assert(newChild.parent == null)
    assert(newChild.payload == null)

    val currentNode = oldChild.parent
    assert(currentNode === this) { "$currentNode, $this" }
    oldChild.resetParent()

    val index = children.indexOf(oldChild)
    assert(index >= 0) { index }
    children[index] = newChild

    newChild.parent = currentNode
    newChild.payload = payload

    assert(newChild.parent === this)
    assert(checkNodeIntegrity() == null) { checkNodeIntegrity()!! }
  }

  /**
   * Perform breadth-first search from this node. The depth is bounded by the specified parameter.
   *
   *
   * A node below this node (this node is excluded) and above/at the max depth (included) is
   * returned, if
   *  * it passes the given `nodePredicate`
   *  * and is the first node along the path from this to the node at the max depth.
   */
  fun boundedBreadthFirstSearchForFirstQualifiedNodes(
    nodePredicate: Predicate<T>,
    maxBfsDepth: Int
  ): ArrayList<T> {
    val resultNodes = ArrayList<T>()
    boundedBFSChildren { node: T, currentDepth: Int ->
      if (currentDepth > maxBfsDepth) {
        return@boundedBFSChildren TreeNodeFilterResult.STOP
      }
      val predicateResult = nodePredicate.test(node)
      if (predicateResult) {
        resultNodes.add(node)
        return@boundedBFSChildren TreeNodeFilterResult.STOP
      }
      if (currentDepth == maxBfsDepth) {
        return@boundedBFSChildren TreeNodeFilterResult.STOP
      } else {
        assert(currentDepth < maxBfsDepth)
        return@boundedBFSChildren TreeNodeFilterResult.CONTINUE
      }
    }
    return resultNodes
  }

  inline fun boundedBFSChildren(visitor: (node: T, currentDepth: Int) -> TreeNodeFilterResult) {
    val queue = SimpleQueue<T>()
    forEachChild { e: T -> queue.add(e) }
    var currentDepth = 0
    while (!queue.isEmpty()) {
      ++currentDepth
      val queueSize = queue.size // Iterate over the nodes on the same level.
      for (i in 0 until queueSize) {
        val node = queue.remove()
        val filterResult = visitor(node, currentDepth)
        if (filterResult === TreeNodeFilterResult.STOP) {
          continue
        }
        assert(filterResult === TreeNodeFilterResult.CONTINUE)
        node.forEachChild { queue.add(it) }
      }
    }
  }

  /**
   * Traverse the tree with the given visitor.
   *
   * This method is safe to manipulate the children
   */
  fun preOrderVisit(visitor: (node: T) -> List<T>) {
    val stack = SimpleStack<T>()
    @Suppress("UNCHECKED_CAST")
    stack.add(this as T)
    while (stack.isNotEmpty()) {
      val top = stack.remove()
      val childrenToVisit = visitor(top)
      val childCount = childrenToVisit.size
      for (i in childCount - 1 downTo 0) {
        val child = childrenToVisit[i]
        stack.add(child)
      }
    }
  }

  fun postOrderVisit(visitor: (node: T) -> Unit) {
    val tempStack = SimpleStack<T>()
    val postOrderStack = SimpleStack<T>()
    @Suppress("UNCHECKED_CAST")
    tempStack.add(this as T)

    // Push and remove the tempSimpleStack to set up the structure of postOrderSimpleStack
    while (tempStack.isNotEmpty()) {
      val top = tempStack.remove()
      postOrderStack.add(top)
      val childrenToVisit: List<T> = top.children
      val childCount = childrenToVisit.size
      for (i in 0 until childCount) {
        val child = childrenToVisit[i]
        tempStack.add(child)
      }
    }

    // Use postOrderSimpleStack to iterate
    while (postOrderStack.isNotEmpty()) {
      val top = postOrderStack.remove()
      visitor(top)
    }
  }

  open fun recursiveDeepCopy(): T {
    cleanDeletedImmediateChildren()
    val map = HashMap<T, T>()
    postOrderVisit { origNode ->
      val copyNode = origNode.copyCurrentNode()
      origNode.children.forEach { oldChild ->
        val newChild = map[oldChild]!!
        copyNode.addChild(newChild, oldChild.payload!!)
      }
      map[origNode] = copyNode
    }
    @Suppress("UNCHECKED_CAST")
    return map[this]!!
  }

  protected abstract fun copyCurrentNode(): T

  companion object {

    @JvmStatic
    fun <T : AbstractTreeNode<T, Payload>, Payload> findLowestAncestor(
      vararg descendants: T
    ) = findLowestAncestor(ImmutableList.copyOf(descendants))

    @JvmStatic
    fun <T : AbstractTreeNode<T, Payload>, Payload> findLowestAncestor(
      descendants: ImmutableList<T>
    ): T {
      require(descendants.isNotEmpty())
      val pathList = ArrayList<ArrayList<T>>()

      // collect pathList
      val descendantsCount = descendants.size
      for (i in 0 until descendantsCount) {
        val currentPath = ArrayList<T>()
        var curNode = descendants[i]
        while (curNode != null) {
          currentPath.add(curNode)
          curNode = curNode.parent
        }
        pathList.add(currentPath)
      }

      // initialize pathCount and maxPathCount
      val pathCount = pathList.size
      val firstPath = pathList[0]
      var maxPathCount = pathList[0].size
      for (i in 0 until pathCount) {
        val curSize = pathList[i].size
        if (maxPathCount > curSize) {
          maxPathCount = curSize
        }
        assert(curSize > 0)
      }

      // assert that all the nodes come from the same tree
      val root = pathList[0][pathList[0].size - 1]
      for (i in 0 until pathCount) {
        check(pathList[i][pathList[i].size - 1] === root)
      }

      // check mismatch and update lowestAncestor
      var check = true
      var lowestAncestor: T? = null
      for (i in 0 until maxPathCount) {
        val curNode = firstPath[firstPath.size - 1 - i]
        val curId = curNode.nodeId
        for (j in 0 until pathCount) {
          val eachNode = pathList[j][pathList[j].size - 1 - i]
          val eachId = eachNode.nodeId
          if (eachId != curId) {
            check = false
          }
        }
        lowestAncestor = if (check) {
          curNode
        } else break
      }
      return lowestAncestor!!
    }

    fun <T : AbstractTreeNode<T, Payload>, Payload> findLowestAncestorPair(
      oneNode: T,
      anotherNode: T
    ): T {
      return findLowestAncestor(ImmutableList.of(oneNode, anotherNode))
    }
  }
}
