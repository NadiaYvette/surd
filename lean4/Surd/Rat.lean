/-
  Surd.Rat — Utility instances and functions for Lean's built-in Rat type.
-/
import Std.Internal.Rat

namespace Surd

open Std.Internal in
/-- Ordering instance for Rat, needed for Map/sort. -/
instance : Ord Rat where
  compare (a b : Rat) :=
    let lhs := a.num * b.den
    let rhs := b.num * a.den
    if lhs < rhs then .lt
    else if lhs > rhs then .gt
    else .eq

open Std.Internal in
/-- Hashable instance for Rat, needed for HashMap. -/
instance : Hashable Rat where
  hash (r : Rat) :=
    mixHash (hash r.num) (hash r.den)

namespace Rat'

open Std.Internal

/-- Absolute value of a rational. -/
def abs (r : Rat) : Rat :=
  if r < 0 then -r else r

/-- Sign of a rational: -1, 0, or 1. -/
def sign (r : Rat) : Int :=
  if r < 0 then -1
  else if r > 0 then 1
  else 0

/-- The numerator of a rational (as Int). -/
def numerator (r : Rat) : Int := r.num

/-- The denominator of a rational (as Nat, always positive). -/
def denominator (r : Rat) : Nat := r.den

/-- Min/Max for Rat via Ord. -/
instance : Min Rat where
  min a b := if a ≤ b then a else b

instance : Max Rat where
  max a b := if a ≥ b then a else b

/-- Convert Nat to Rat. -/
def natToRat (n : Nat) : Rat := (Int.ofNat n : Int)

end Rat'

end Surd
