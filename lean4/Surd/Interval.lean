/-
  Surd.Interval — Interval arithmetic with rational endpoints.

  Closed intervals [lo, hi] for root isolation and numerical evaluation.
-/
import Surd.Rat
import Std.Internal.Rat

open Std.Internal

namespace Surd

/-- A closed interval [lo, hi] with rational endpoints. -/
structure Interval where
  lo : Rat
  hi : Rat
  deriving Repr, BEq

namespace Interval

def midpoint (iv : Interval) : Rat :=
  (iv.lo + iv.hi) / 2

def width (iv : Interval) : Rat :=
  iv.hi - iv.lo

def contains (iv : Interval) (x : Rat) : Bool :=
  iv.lo ≤ x && x ≤ iv.hi

def overlaps (iv1 iv2 : Interval) : Bool :=
  iv1.lo ≤ iv2.hi && iv2.lo ≤ iv1.hi

def bisect (iv : Interval) : Interval × Interval :=
  let m := iv.midpoint
  (⟨iv.lo, m⟩, ⟨m, iv.hi⟩)

def bisectLo (iv : Interval) : Interval :=
  ⟨iv.lo, iv.midpoint⟩

def bisectHi (iv : Interval) : Interval :=
  ⟨iv.midpoint, iv.hi⟩

def fromRat (x : Rat) : Interval :=
  ⟨x, x⟩

def strictlyPositive (iv : Interval) : Bool :=
  iv.lo > 0

def strictlyNegative (iv : Interval) : Bool :=
  iv.hi < 0

def containsZero (iv : Interval) : Bool :=
  iv.lo ≤ 0 && 0 ≤ iv.hi

/-- Interval addition. -/
def iadd (a b : Interval) : Interval :=
  ⟨a.lo + b.lo, a.hi + b.hi⟩

/-- Interval subtraction. -/
def isub (a b : Interval) : Interval :=
  ⟨a.lo - b.hi, a.hi - b.lo⟩

/-- Interval negation. -/
def ineg (a : Interval) : Interval :=
  ⟨-a.hi, -a.lo⟩

/-- Interval multiplication. -/
def imul (a b : Interval) : Interval :=
  let p1 := a.lo * b.lo
  let p2 := a.lo * b.hi
  let p3 := a.hi * b.lo
  let p4 := a.hi * b.hi
  ⟨min p1 (min p2 (min p3 p4)), max p1 (max p2 (max p3 p4))⟩

/-- Interval reciprocal. Assumes interval does not contain zero. -/
def iinv (a : Interval) : Interval :=
  if a.lo > 0 || a.hi < 0 then
    let r1 := (1 : Rat) / a.lo
    let r2 := (1 : Rat) / a.hi
    ⟨min r1 r2, max r1 r2⟩
  else
    ⟨-1000000, 1000000⟩

/-- Interval division. -/
def idiv (a b : Interval) : Interval :=
  imul a (iinv b)

/-- Interval absolute value. -/
def iabs (a : Interval) : Interval :=
  if a.lo ≥ 0 then a
  else if a.hi ≤ 0 then ⟨-a.hi, -a.lo⟩
  else ⟨0, max (-a.lo) a.hi⟩

/-- Interval integer power by squaring. -/
def ipow (a : Interval) (n : Nat) : Interval :=
  match n with
  | 0 => fromRat 1
  | 1 => a
  | n + 2 =>
    if (n + 2) % 2 == 0 then
      let half := ipow a ((n + 2) / 2)
      imul half half
    else
      imul a (ipow a (n + 1))
  termination_by n

/-- Rational Newton iteration for square root upper bound. -/
private def ratSqrtHi (r : Rat) (iters : Nat) : Rat :=
  if r ≤ 0 then 0
  else go (max r 1) iters
where
  go (x : Rat) : Nat → Rat
    | 0 => x
    | n + 1 => go ((x + r / x) / 2) n

/-- Rational Newton iteration for square root lower bound. -/
private def ratSqrtLo (r : Rat) (iters : Nat) : Rat :=
  if r ≤ 0 then 0
  else go (r / (1 + r)) iters
where
  go (x : Rat) : Nat → Rat
    | 0 => x
    | n + 1 =>
      let x' := (x + r / x) / 2
      go (min x x') n

/-- Interval square root (for non-negative intervals). -/
def isqrt (a : Interval) : Interval :=
  if a.hi ≤ 0 then fromRat 0
  else
    let a' := { lo := max a.lo 0, hi := a.hi : Interval }
    ⟨ratSqrtLo a'.lo 20, ratSqrtHi a'.hi 20⟩

/-- Interval nth root (n ≥ 2, non-negative interval). -/
def inth (n : Nat) (a : Interval) : Interval :=
  if n == 2 then isqrt a
  else
    let lo := nthRootBound a.lo n 20 false
    let hi := nthRootBound a.hi n 20 true
    ⟨lo, hi⟩
where
  ratPow (x : Rat) : Nat → Rat
    | 0 => 1
    | 1 => x
    | k + 1 => x * ratPow x k
  nthRootNewton (r : Rat) (n : Nat) (x : Rat) : Nat → Rat
    | 0 => x
    | i + 1 =>
      let xn1 := ratPow x (n - 1)
      let nRat : Rat := (Int.ofNat n : Int)
      let x' := ((nRat - 1) * x + r / xn1) / nRat
      nthRootNewton r n x' i
  nthRootBound (r : Rat) (n iters : Nat) (upper : Bool) : Rat :=
    if r ≤ 0 then 0
    else
      let x0 : Rat := if upper then max r 1 else min r 1
      nthRootNewton r n x0 iters

end Interval

/-- A complex interval: pair of real and imaginary intervals. -/
structure ComplexInterval where
  re : Interval
  im : Interval
  deriving Repr, BEq

namespace ComplexInterval

def fromReal (iv : Interval) : ComplexInterval :=
  ⟨iv, Interval.fromRat 0⟩

def fromRat (r : Rat) : ComplexInterval :=
  fromReal (Interval.fromRat r)

def ciadd (a b : ComplexInterval) : ComplexInterval :=
  ⟨a.re.iadd b.re, a.im.iadd b.im⟩

def cisub (a b : ComplexInterval) : ComplexInterval :=
  ⟨a.re.isub b.re, a.im.isub b.im⟩

def cimul (a b : ComplexInterval) : ComplexInterval :=
  ⟨(a.re.imul b.re).isub (a.im.imul b.im),
   (a.re.imul b.im).iadd (a.im.imul b.re)⟩

def cineg (a : ComplexInterval) : ComplexInterval :=
  ⟨a.re.ineg, a.im.ineg⟩

def ciMagnitudeSq (a : ComplexInterval) : Interval :=
  (a.re.imul a.re).iadd (a.im.imul a.im)

end ComplexInterval

end Surd
