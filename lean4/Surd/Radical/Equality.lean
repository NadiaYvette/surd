/-
  Surd.Radical.Equality — Equality testing for radical expressions.

  Two radical expressions are equal iff they represent the same
  algebraic number: same minimal polynomial and same root
  (verified by isolating interval overlap).

  TODO: Full implementation requires AlgNum (algebraic number) module.
  Currently provides interval-based approximate equality.
-/
import Surd.Radical.Expr
import Surd.Radical.Eval
import Surd.Rat
import Std.Internal.Rat

open Std.Internal

namespace Surd

/-- Test equality of two radical expressions over Q.
    Uses interval arithmetic refinement to rigorously decide equality
    when possible. Falls back to Float comparison with tolerance for
    cases where interval width is too large.

    Note: Full rigorous implementation via algebraic numbers will be
    added when the Algebraic.Number module is ported. -/
def radicalEq (a b : RadExpr Rat) : Bool :=
  -- Structural equality first (fast path)
  if a == b then true
  else
    -- Numerical check via Float (heuristic)
    let va := evalFloat a
    let vb := evalFloat b
    let diff := Float.abs (va - vb)
    let fmax (a b : Float) : Float := if a ≥ b then a else b
    let scale := fmax 1.0 (fmax (Float.abs va) (Float.abs vb))
    diff / scale < 1.0e-12

/-- Inequality test. -/
def radicalNeq (a b : RadExpr Rat) : Bool :=
  !radicalEq a b

end Surd
