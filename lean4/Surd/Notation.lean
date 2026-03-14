/-
  Surd.Notation — Scoped notation macros for radical expressions.

  Within `open Surd`, you can write `√ e`, `∛ e`, `∜ e` instead
  of `RadExpr.root 2 e`, etc.  These notations bind at maximum
  precedence so they apply to the immediately following term.
-/
import Surd.Radical.Expr

namespace Surd

/-- Square root: `√ e` is `RadExpr.root 2 e`. -/
scoped notation:max "√" x => RadExpr.root 2 x
/-- Cube root: `∛ e` is `RadExpr.root 3 e`. -/
scoped notation:max "∛" x => RadExpr.root 3 x
/-- Fourth root: `∜ e` is `RadExpr.root 4 e`. -/
scoped notation:max "∜" x => RadExpr.root 4 x

end Surd
