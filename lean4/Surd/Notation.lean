/-
  Surd.Notation — Scoped notation macros for radical expressions.
-/
import Surd.Radical.Expr

namespace Surd

scoped notation:max "√" x => RadExpr.root 2 x
scoped notation:max "∛" x => RadExpr.root 3 x
scoped notation:max "∜" x => RadExpr.root 4 x

end Surd
