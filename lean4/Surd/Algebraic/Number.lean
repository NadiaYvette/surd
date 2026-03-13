/-
  Surd.Algebraic.Number — Canonical representation of real algebraic numbers.

  An algebraic number is represented as a pair (minimal polynomial, isolating interval).
  Two algebraic numbers are equal iff they have the same minimal polynomial
  and their isolating intervals overlap (after refinement).

  Arithmetic is done via resultant-based composed polynomials:
    α + β → composedSum(minpoly α, minpoly β)
    α · β → composedProduct(minpoly α, minpoly β)
  which are then factored to find the true minimal polynomial.
-/
import Surd.Poly.Univariate
import Surd.Poly.Resultant
import Surd.Interval
import Surd.Algebraic.RootIsolation
import Surd.Radical.Eval
import Surd.Rat
import Std.Internal.Rat

open Std.Internal
open Surd.Poly

namespace Surd

/-- A real algebraic number: its minimal polynomial over Q
    and an isolating interval pinpointing which root it is. -/
structure AlgNum where
  anMinPoly : Poly Rat
  anInterval : Interval
  deriving Repr

/-- Stub for square-free factorization (same as in MinimalPoly).
    TODO: share implementation when full factoring is ported. -/
private def factorSquareFreePoly (p : Poly Rat) : List (Poly Rat) :=
  let sf := Poly.monic (Poly.squareFree p)
  if sf.isZero then [] else [sf]

/-- Embed a rational number as an algebraic number. -/
def algFromRational (r : Rat) : AlgNum :=
  ⟨Poly.mkPoly #[-r, 1], ⟨r, r⟩⟩

/-- Approximate the algebraic number as a rational to within epsilon. -/
def algApprox (eps : Rat) (a : AlgNum) : Rat :=
  let ii := refineRoot eps ⟨a.anMinPoly, a.anInterval⟩
  (ii.iiInterval.lo + ii.iiInterval.hi) / 2

/-- Approximate as a Float. -/
def algApproxFloat (a : AlgNum) : Float :=
  let r := algApprox (1 / 1000000000000000) a
  ratToFloat r

/-- Pick the irreducible factor with a root closest to the target. -/
private partial def pickBestRoot (factors : List (Poly Rat)) (approx : Float)
    : Option AlgNum :=
  let candidates : List (Poly Rat × Interval) :=
    factors.flatMap fun f =>
      (isolateRealRoots f).map fun ii => (f, ii.iiInterval)
  let scored : List (Poly Rat × Interval × Float) :=
    candidates.map fun (f, iv) =>
      let mid := ratToFloat ((iv.lo + iv.hi) / 2)
      (f, iv, Float.abs (mid - approx))
  match scored with
  | [] => none
  | s :: ss =>
    let (f, iv, _) := ss.foldl (fun best cur =>
      if cur.2.2 < best.2.2 then cur else best) s
    some ⟨f, iv⟩

/-- Construct an AlgNum from an annihilating polynomial and approximate value.
    Factors the polynomial, picks the irreducible factor with a root nearest
    to the approximation, and isolates the root. -/
private partial def makeAlgNumFromAnn (ann : Poly Rat) (approx : Float) : AlgNum :=
  let factors := factorSquareFreePoly (Poly.monic ann)
  match pickBestRoot factors approx with
  | some a => a
  | none => ⟨Poly.monic ann, ⟨-1000, 1000⟩⟩

/-- From an annihilating polynomial and interval,
    find the minimal polynomial factor and refine the interval. -/
private partial def makeAlgNum (ann : Poly Rat) (iv : Interval) : AlgNum :=
  let factors := factorSquareFreePoly (Poly.monic ann)
  let mid := ratToFloat ((iv.lo + iv.hi) / 2)
  match pickBestRoot factors mid with
  | some a => a
  | none => ⟨Poly.monic ann, iv⟩

/-- Construct an algebraic number from a polynomial and an approximate
    real value (used to pick the right root). -/
partial def algFromPoly (p : Poly Rat) (approx : Float) : Option AlgNum :=
  let mp := Poly.monic p
  let factors := factorSquareFreePoly mp
  pickBestRoot factors approx

/-- Negation: if p(α) = 0 then p(-x) annihilates -α. -/
partial def algNeg (a : AlgNum) : AlgNum :=
  let p' := Poly.negateVar a.anMinPoly
  makeAlgNum p' ⟨-a.anInterval.hi, -a.anInterval.lo⟩

/-- Addition via composed sum. -/
partial def algAdd (a b : AlgNum) : AlgNum :=
  let ann := Poly.composedSum a.anMinPoly b.anMinPoly
  let approx := algApproxFloat a + algApproxFloat b
  makeAlgNumFromAnn ann approx

/-- Subtraction. -/
partial def algSub (a b : AlgNum) : AlgNum :=
  algAdd a (algNeg b)

/-- Multiplication via composed product. -/
partial def algMul (a b : AlgNum) : AlgNum :=
  let ann := Poly.composedProduct a.anMinPoly b.anMinPoly
  let approx := algApproxFloat a * algApproxFloat b
  makeAlgNumFromAnn ann approx

/-- Multiplicative inverse: x^deg(p) · p(1/x) annihilates 1/α. -/
partial def algInv (a : AlgNum) : AlgNum :=
  let p' := Poly.reciprocalPoly a.anMinPoly
  let iv := a.anInterval
  let iv' :=
    if iv.lo > 0 then ⟨(1 : Rat) / iv.hi, (1 : Rat) / iv.lo⟩
    else if iv.hi < 0 then ⟨(1 : Rat) / iv.hi, (1 : Rat) / iv.lo⟩
    else ⟨-1000, 1000⟩  -- zero in interval, degenerate
  makeAlgNum p' iv'

/-- Division. -/
partial def algDiv (a b : AlgNum) : AlgNum :=
  algMul a (algInv b)

/-- Integer power. -/
partial def algPow (a : AlgNum) (n : Int) : AlgNum :=
  if n == 0 then algFromRational 1
  else if n == 1 then a
  else if n < 0 then algPow (algInv a) (-n)
  else if n % 2 == 0 then
    let half := algPow a (n / 2)
    algMul half half
  else algMul a (algPow a (n - 1))

/-- nth root of a positive algebraic number.
    The annihilating polynomial is p(x^n). -/
partial def algRoot (n : Int) (a : AlgNum) : AlgNum :=
  if n ≤ 0 then a
  else
    let ann := Poly.substituteXN n.toNat a.anMinPoly
    let v := algApproxFloat a
    let approx := (v.log / n.toNat.toFloat).exp
    makeAlgNumFromAnn ann approx

/-- Check if two intervals identify the same root, by refining until
    they either overlap or separate. -/
private partial def intervalsOverlap (iv1 iv2 : Interval) (p : Poly Rat) : Bool :=
  go 50 iv1 iv2
where
  go : Nat → Interval → Interval → Bool
    | 0, i1, i2 =>
      let m1 := (i1.lo + i1.hi) / 2
      let m2 := (i2.lo + i2.hi) / 2
      Rat'.abs (m1 - m2) < (i1.hi - i1.lo + i2.hi - i2.lo) / 2
    | n + 1, i1, i2 =>
      if i1.hi < i2.lo || i2.hi < i1.lo then false  -- disjoint
      else if i1.lo == i1.hi && i2.lo == i2.hi then i1.lo == i2.lo  -- both exact
      else if i1.lo == i1.hi then
        Poly.eval p i1.lo == 0 && i2.lo ≤ i1.lo && i1.lo ≤ i2.hi
      else if i2.lo == i2.hi then
        Poly.eval p i2.lo == 0 && i1.lo ≤ i2.lo && i2.lo ≤ i1.hi
      else
        let i1' := (refineRoot ((i1.hi - i1.lo) / 4) ⟨p, i1⟩).iiInterval
        let i2' := (refineRoot ((i2.hi - i2.lo) / 4) ⟨p, i2⟩).iiInterval
        go n i1' i2'

/-- Equality: same minimal polynomial and overlapping intervals
    (after sufficient refinement). -/
def algEq (a b : AlgNum) : Bool :=
  a.anMinPoly == b.anMinPoly &&
  intervalsOverlap a.anInterval b.anInterval a.anMinPoly

/-- Refine intervals of two distinct algebraic numbers until disjoint. -/
private partial def separateAndCompare (a b : AlgNum) : Ordering :=
  go 200 ⟨a.anMinPoly, a.anInterval⟩ ⟨b.anMinPoly, b.anInterval⟩
where
  go : Nat → IsolatingInterval → IsolatingInterval → Ordering
    | 0, ii1, ii2 =>
      let m1 := (ii1.iiInterval.lo + ii1.iiInterval.hi) / 2
      let m2 := (ii2.iiInterval.lo + ii2.iiInterval.hi) / 2
      if m1 < m2 then .lt else if m1 > m2 then .gt else .eq
    | n + 1, ii1, ii2 =>
      if ii1.iiInterval.hi < ii2.iiInterval.lo then .lt
      else if ii2.iiInterval.hi < ii1.iiInterval.lo then .gt
      else
        let w1 := ii1.iiInterval.hi - ii1.iiInterval.lo
        let w2 := ii2.iiInterval.hi - ii2.iiInterval.lo
        let ii1' := refineRoot (w1 / 4) ii1
        let ii2' := refineRoot (w2 / 4) ii2
        go n ii1' ii2'

/-- Comparison via rigorous interval separation. -/
def algCompare (a b : AlgNum) : Ordering :=
  if algEq a b then .eq
  else separateAndCompare a b

/-- Display as "AlgNum(<poly> ≈ <approx>)". -/
def algShow (a : AlgNum) : String :=
  let approx := ratToFloat (algApprox (1 / 1000) a)
  s!"AlgNum({repr a.anMinPoly} ≈ {approx})"

instance : BEq AlgNum where
  beq := algEq

instance : Ord AlgNum where
  compare := algCompare

instance : ToString AlgNum where
  toString := algShow

instance : Inhabited AlgNum where
  default := algFromRational 0

end Surd
