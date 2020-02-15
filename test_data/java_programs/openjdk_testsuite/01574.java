

package org.openjdk.tests.shapegen;

import java.util.HashSet;
import java.util.Set;

import static org.openjdk.tests.shapegen.ClassCase.Kind.*;


public class RuleGroup {

    final String name;
    private final Rule[] rules;

    public RuleGroup(String name, Rule[] rules) {
        this.name = name;
        this.rules = rules;
    }

    public boolean exec(ClassCase cc) {
        boolean found = false;
        for (Rule rule : rules) {
            if (rule.guard(cc)) {
                if (found) {
                    throw new RuntimeException("Bad rules -- multiple matches " + toString() + " for " + cc);
                } else {
                    rule.eval(cc);
                    found = true;
                }
            }
        }
        return found;
    }

    @Override
    public String toString() {
        return name;
    }

    public static RuleGroup PROVENENCE = new RuleGroup("Provenence", new Rule[] {
      new Rule("P-CDeclare") {
          boolean guard(ClassCase cc) {
              return cc.isa(CCONCRETE, CABSTRACT);
          }

          void eval(ClassCase cc) {
              cc.set_mprov(cc);
              cc.set_HasClassMethod(true);
          }
      },

      new Rule("P-IDeclare") {
          boolean guard(ClassCase cc) {
              return cc.isa(IDEFAULT, IPRESENT);
          }

          void eval(ClassCase cc) {
              cc.set_mprov(cc);
          }
      },

      new Rule("P-IntfInh") {
          boolean guard(ClassCase cc) {
              return cc.isa(IVAC, CNONE) && !(cc.hasSuperclass() && cc.getSuperclass().get_HasClassMethod());
          }

          void eval(ClassCase cc) {
              Set<ClassCase> _S = new HashSet<>();
              for (ClassCase t : cc.getSupertypes()) {
                  _S.addAll(t.get_mprov());
              }
              Set<ClassCase> tops = new HashSet<>();
              for (ClassCase _W : _S) {
                  for (ClassCase _V : _S) {
                      if (_V.equals(_W) || !(_V.isSubtypeOf(_W))) {
                          tops.add(_W);
                      }
                  }
              }
              cc.set_mprov(tops);
          }
      },

      new Rule("P-ClassInh") {
          boolean guard(ClassCase cc) {
              return cc.isa(CNONE) && (cc.hasSuperclass() && cc.getSuperclass().get_HasClassMethod());
          }

          void eval(ClassCase cc) {
              cc.set_mprov(cc.getSuperclass());
              cc.set_HasClassMethod(true);
          }
      },

    });

    public static RuleGroup MARKER = new RuleGroup("Marker", new Rule[] {
      new Rule("M-Default") {
          boolean guard(ClassCase cc) {
              return cc.isa(IDEFAULT);
          }

          void eval(ClassCase cc) {
              cc.set_HasDefault(true);
          }
      },

      new Rule("M-Conc") {
          boolean guard(ClassCase cc) {
            return cc.isa(CCONCRETE);
          }

          void eval(ClassCase cc) {
              cc.set_IsConcrete(true);
          }
      },

    });

    public static RuleGroup RESOLUTION = new RuleGroup("Resolution", new Rule[] {
      new Rule("R-Resolve") {
          boolean guard(ClassCase cc) {
              if (!(cc.isClass() && cc.get_mprov().size() == 1)) {
                  return false;
              }
              ClassCase _V = cc.get_mprov().iterator().next();
              return _V.get_IsConcrete() || _V.get_HasDefault();
          }

          void eval(ClassCase cc) {
              ClassCase _V = cc.get_mprov().iterator().next();
              cc.set_mres(_V);
          }
      },

    });

    public static RuleGroup DEFENDER = new RuleGroup("Defender", new Rule[] {
      new Rule("D-Defend") {
          boolean guard(ClassCase cc) {
              ClassCase mresSuper = cc.hasSuperclass() ? cc.getSuperclass().get_mres() : null;
              boolean eq = cc.get_mres() == null ? mresSuper == null : cc.get_mres().equals(mresSuper);
              return cc.isa(CNONE) && !eq;
          }

          void eval(ClassCase cc) {
              cc.set_mdefend(cc.get_mres());
          }
      },

    });

    public static RuleGroup CHECKING = new RuleGroup("Checking", new Rule[] {
      new Rule("C-Check") {
          boolean guard(ClassCase cc) {
              for (ClassCase t : cc.getSupertypes()) {
                  if (! t.get_OK()) {
                      return false;
                  }
              }
              int defenderCount = 0;
              int provCount = 0;
              for (ClassCase prov : cc.get_mprov()) {
                  if (prov.get_HasDefault()) {
                      defenderCount++;
                  }
                  provCount++;
              }
              return provCount <= 1 || defenderCount == 0;
          }

          void eval(ClassCase cc) {
              cc.set_OK(true);
          }
      },

    });

}