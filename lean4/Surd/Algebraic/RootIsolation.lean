/-
  Surd.Algebraic.RootIsolation — Real root isolation for polynomials
  with rational coefficients.

  Uses Sturm's theorem with bisection to separate and refine roots
  of square-free polynomials.
-/
import Surd.Poly.Univariate
import Surd.Interval
import Surd.Rat
import Std.Internal.Rat

open Std.Internal
open Surd.Poly

namespace Surd

/-- An isolating interval for a real root of a polynomial.
    The polynomial has exactly one root in the open interval (lo, hi),
    unless lo == hi in which case it's an exact rational root. -/
structure IsolatingInterval where
  iiPoly : Poly Rat
  iiInterval : Interval
  deriving Repr

/-- Cauchy bound: all roots satisfy |r| ≤ bound. -/
private def rootBound (p : Poly Rat) : Rat :=
  if p.coeffs.isEmpty then 0
  else
    let lc := p.coeffs.back!
    if lc == 0 then 0
    else
      let ratios := (p.coeffs.pop.map fun c => Rat'.abs (c / lc))
      1 + ratios.foldl (fun (acc : Rat) (r : Rat) => if r > acc then r else acc) 0

/-- Build the Sturm chain: p₀ = p, p₁ = p', pₖ₊₁ = -rem(pₖ₋₁, pₖ). -/
private partial def sturmChain (p : Poly Rat) : List (Poly Rat) :=
  let p' := diff p
  p :: p' :: go p p'
where
  go (a b : Poly Rat) : List (Poly Rat) :=
    if b.isZero then []
    else
      let (_, r) := divMod a b
      let nr := scale (-1 : Rat) r
      if nr.isZero then [] else nr :: go b nr

/-- Count sign changes in the Sturm chain evaluated at x. -/
private def signChangesAt (chain : List (Poly Rat)) (x : Rat) : Nat :=
  let vals : List Rat := chain.map (fun p => Poly.eval p x) |>.filter (· != 0)
  countChanges vals
where
  countChanges : List Rat → Nat
    | [] => 0
    | [_] => 0
    | a :: b :: rest =>
      let change := if (decide (a < 0)) != (decide (b < 0)) then 1 else 0
      change + countChanges (b :: rest)

/-- Count real roots in (a, b] using Sturm's theorem. -/
def sturmCount (p : Poly Rat) (a b : Rat) : Int :=
  let chain := sturmChain p
  let va := signChangesAt chain a
  let vb := signChangesAt chain b
  (Int.ofNat va) - (Int.ofNat vb)

/-- Isolate roots within a given interval using bisection. -/
private partial def isolateIn (p : Poly Rat) (iv : Interval) : List Interval :=
  let sc := sturmCount p iv.lo iv.hi
  if sc ≤ 0 then []
  else if sc == 1 then [iv]
  else
    let m := (iv.lo + iv.hi) / 2
    if Poly.eval p m == 0 then
      ⟨m, m⟩ :: isolateIn p ⟨iv.lo, m⟩ ++ isolateIn p ⟨m, iv.hi⟩
    else
      isolateIn p ⟨iv.lo, m⟩ ++ isolateIn p ⟨m, iv.hi⟩

/-- Isolate all real roots of a polynomial.
    Returns isolating intervals ordered by increasing root value. -/
def isolateRealRoots (p : Poly Rat) : List IsolatingInterval :=
  if p.isZero then []
  else
    let p' := monic p
    let bound := rootBound p'
    let iv : Interval := ⟨-bound, bound⟩
    (isolateIn p' iv).map fun iv' => ⟨p', iv'⟩

/-- Refine an isolating interval by bisection until width < eps. -/
partial def refineRoot (eps : Rat) (ii : IsolatingInterval) : IsolatingInterval :=
  ⟨ii.iiPoly, refineInterval ii.iiPoly eps ii.iiInterval⟩
where
  refineInterval (p : Poly Rat) (eps : Rat) (iv : Interval) : Interval :=
    if iv.lo == iv.hi then iv
    else if iv.hi - iv.lo < eps then iv
    else
      let m := (iv.lo + iv.hi) / 2
      let fm := Poly.eval p m
      if fm == 0 then ⟨m, m⟩
      else
        let fl := Poly.eval p iv.lo
        let flSign := decide (fl < 0)
        let fmSign := decide (fm < 0)
        if flSign != fmSign then refineInterval p eps ⟨iv.lo, m⟩
        else refineInterval p eps ⟨m, iv.hi⟩

/-- Check if a specific rational is a root within the interval. -/
def rootInInterval (ii : IsolatingInterval) : Option Rat :=
  if ii.iiInterval.lo == ii.iiInterval.hi &&
     Poly.eval ii.iiPoly ii.iiInterval.lo == 0
  then some ii.iiInterval.lo
  else none

end Surd
