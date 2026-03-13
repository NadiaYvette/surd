/-
  Surd.Types — Core AST for radical expressions.

  Re-exports the RadExpr type defined in Surd.Radical.Expr, plus
  convenience constructors and pattern aliases.

  In the Lean 4 port, RadExpr is defined directly in Radical.Expr
  (not here), so this module just provides the Haskell-style helpers.
-/
import Surd.Radical.Expr
import Surd.Rat
import Std.Internal.Rat

open Std.Internal

namespace Surd

/-- Lift a rational into a radical expression. -/
def ratE (r : Rat) : RadExpr Rat := .lit r

/-- Lift an integer into a radical expression. -/
def intE (n : Int) : RadExpr Rat := .lit (n : Rat)

/-- Subtraction as Add a (Neg b). -/
def radSub {k : Type} (a b : RadExpr k) : RadExpr k := .add a (.neg b)

/-- Division as Mul a (Inv b). -/
def radDiv {k : Type} (a b : RadExpr k) : RadExpr k := .mul a (.inv b)

/-- Square root shorthand. -/
def radSqrt {k : Type} (x : RadExpr k) : RadExpr k := .root 2 x

end Surd
