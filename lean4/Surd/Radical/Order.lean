/-
  Surd.Radical.Order — Ordering of real radical expressions.

  Uses algebraic number comparison with Sturm-based interval
  refinement for rigorous results.

  TODO: Full implementation requires AlgNum (algebraic number) module.
  Currently provides Float-based approximate ordering.
-/
import Surd.Radical.Expr
import Surd.Radical.Eval
import Surd.Rat
import Std.Internal.Rat

open Std.Internal

namespace Surd

/-- Compare two radical expressions.
    Note: Full rigorous implementation via algebraic numbers will be
    added when the Algebraic.Number module is ported. -/
def radicalCompare (a b : RadExpr Rat) : Ordering :=
  if a == b then .eq
  else
    let va := evalFloat a
    let vb := evalFloat b
    let diff := va - vb
    let fmax (a b : Float) : Float := if a ≥ b then a else b
    let scale := fmax 1.0 (fmax (Float.abs va) (Float.abs vb))
    if Float.abs diff / scale < 1.0e-12 then .eq
    else if diff < 0 then .lt
    else .gt

/-- Less than. -/
def radicalLt (a b : RadExpr Rat) : Bool :=
  radicalCompare a b == .lt

/-- Greater than. -/
def radicalGt (a b : RadExpr Rat) : Bool :=
  radicalCompare a b == .gt

/-- Less than or equal. -/
def radicalLeq (a b : RadExpr Rat) : Bool :=
  radicalCompare a b != .gt

/-- Greater than or equal. -/
def radicalGeq (a b : RadExpr Rat) : Bool :=
  radicalCompare a b != .lt

end Surd
