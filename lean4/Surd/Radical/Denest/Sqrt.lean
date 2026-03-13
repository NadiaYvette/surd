/-
  Surd.Radical.Denest.Sqrt — Square root denesting.

  Implements the algorithm for denesting expressions of the form
  √(a + b√r) where a, b, r ∈ Q.

  If denesting is possible, √(a + b√r) = √x + √y (or √x - √y)
  where x + y = a and x * y = b²r/4.

  Reference: Borodin, Fagin, Hopcroft, Tompa (1985)
-/
import Surd.Radical.Expr
import Surd.Rat
import Std.Internal.Rat

open Std.Internal

namespace Surd

/-- Integer square root: returns some s if n = s², none otherwise. -/
private partial def exactSquareRootInt (n : Int) : Option Int :=
  if n < 0 then none
  else if n == 0 then some 0
  else if n == 1 then some 1
  else
    -- Newton's method
    let nNat := n.toNat
    let s := go nNat nNat
    if s * s == nNat then some (Int.ofNat s)
    else none
where
  go (x n : Nat) : Nat :=
    let x' := (x + n / x) / 2
    if x' ≥ x then x
    else go x' n

/-- Check if a rational number is a perfect square.
    Returns some √q if q is a perfect square, none otherwise. -/
private def isRationalSqrt (q : Rat) : Option Rat :=
  if q < 0 then none
  else if q == 0 then some 0
  else
    match exactSquareRootInt q.num, exactSquareRootInt (Int.ofNat q.den) with
    | some sn, some sd =>
      if sd != 0 then some (sn / sd : Rat) else none
    | _, _ => none

/-- Try to denest √(a + b√r) into √x ± √y.
    Returns some (sign, x, y) where the result is √x + sign*√y,
    or none if denesting is not possible over Q. -/
def trySqrtDenest (a b r : Rat) : Option (Int × Rat × Rat) :=
  let disc := a * a - b * b * r
  if disc < 0 then none
  else
    match isRationalSqrt disc with
    | none => none
    | some sd =>
      let x := (a + sd) / 2
      let y := (a - sd) / 2
      let sign : Int := if b > 0 then 1 else -1
      if x ≥ 0 && y ≥ 0 then some (sign, x, y)
      else none

/-- Match the pattern a + b*√r in a radical expression. -/
private def matchSqrtNested : RadExpr Rat → Option (Rat × Rat × Rat)
  | .add (.lit a) (.mul (.lit b) (.root 2 (.lit r))) => some (a, b, r)
  | .add (.mul (.lit b) (.root 2 (.lit r))) (.lit a) => some (a, b, r)
  | .add (.lit a) (.root 2 (.lit r)) => some (a, 1, r)
  | .add (.root 2 (.lit r)) (.lit a) => some (a, 1, r)
  | .add (.lit a) (.neg (.mul (.lit b) (.root 2 (.lit r)))) => some (a, -b, r)
  | .add (.lit a) (.neg (.root 2 (.lit r))) => some (a, -1, r)
  | _ => none

/-- Try to denest a radical expression that is a square root.
    Looks for the pattern √(a + b*√r) and attempts denesting. -/
def denestSqrtExpr : RadExpr Rat → Option (RadExpr Rat)
  | .root 2 inner =>
    match matchSqrtNested inner with
    | some (a, b, r) => do
      let (sign, x, y) ← trySqrtDenest a b r
      let sx : RadExpr Rat := .root 2 (.lit x)
      let sy : RadExpr Rat := .root 2 (.lit y)
      if sign > 0 then some (.add sx sy)
      else some (.add sx (.neg sy))
    | none => none
  | _ => none

/-- Recursively try to denest all square roots in an expression. -/
partial def denestSqrt : RadExpr Rat → RadExpr Rat
  | e@(.root 2 _) =>
    match denestSqrtExpr e with
    | some denested => denestSqrt denested  -- try again on the result
    | none => match e with
      | .root 2 a => .root 2 (denestSqrt a)
      | _ => e  -- unreachable
  | .neg a => .neg (denestSqrt a)
  | .add a b => .add (denestSqrt a) (denestSqrt b)
  | .mul a b => .mul (denestSqrt a) (denestSqrt b)
  | .inv a => .inv (denestSqrt a)
  | .root n a => .root n (denestSqrt a)
  | .pow a n => .pow (denestSqrt a) n
  | e => e

end Surd
