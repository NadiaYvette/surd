/-
  Surd.Poly.Factoring — Polynomial factoring over Q.

  Implements Kronecker's method for small-degree polynomials
  and rational root testing. Sufficient for radical denesting and
  trig evaluation where we factor cyclotomic polynomials and
  low-degree minimal polynomials.
-/
import Surd.Poly.Univariate
import Surd.PrimeFactors
import Surd.Positive
import Surd.Rat
import Std.Internal.Rat

open Std.Internal

namespace Surd

-- ---------------------------------------------------------------------------
-- Divisor helpers
-- ---------------------------------------------------------------------------

/-- All positive divisors of a positive integer. -/
private def divisorsNat (n : Nat) : List Nat :=
  if n == 0 then [0]
  else
    let fs := factoriseNat n
    go fs
where
  go : List (Nat × Nat) → List Nat
    | [] => [1]
    | (p, e) :: rest =>
      let ds := go rest
      (List.range (e + 1)).flatMap fun k =>
        ds.map fun d => p ^ k * d

/-- Rational divisors: positive divisors of |num|/|den|. -/
private def rationalDivisors (r : Rat) : List Rat :=
  let n := r.num.natAbs
  let d := if r.den == 0 then 1 else r.den
  let nDivs := divisorsNat n
  let dDivs := divisorsNat d
  let pos := nDivs.flatMap fun a =>
    dDivs.filterMap fun b =>
      if b == 0 then none
      else some ((Int.ofNat a : Rat) / (Int.ofNat b : Rat))
  pos.flatMap fun x => [x, -x]

-- ---------------------------------------------------------------------------
-- Rational roots
-- ---------------------------------------------------------------------------

/-- Find all rational roots of a polynomial using the rational root theorem. -/
partial def rationalRoots (p : Poly Rat) : List Rat :=
  if p.coeffs.size ≤ 1 then []
  else
    let cs := p.coeffs
    let a0 := cs[0]!
    let an := cs[cs.size - 1]!
    if an == 0 then []
    else
      let numDivs := divisorsNat a0.num.natAbs
      let denDivs := divisorsNat an.num.natAbs
      let candidates := numDivs.flatMap fun pn =>
        denDivs.filterMap fun qn =>
          if qn == 0 then none
          else some ((Int.ofNat pn : Rat) / (Int.ofNat qn : Rat))
      let withNegs := candidates.flatMap fun x => [x, -x]
      -- Also try 0
      let all := if withNegs.contains 0 then withNegs else 0 :: withNegs
      all.filter fun r => Poly.eval p r == 0

-- ---------------------------------------------------------------------------
-- Factor helpers
-- ---------------------------------------------------------------------------

/-- Find a linear factor (rational root). -/
private partial def findLinearFactor (p : Poly Rat) : Option (Rat × Poly Rat) :=
  match rationalRoots p with
  | [] => none
  | r :: _ =>
    let divisor := Poly.mkPoly #[-r, 1]
    let (q, _) := Poly.divMod p divisor
    some (r, q)

/-- Try to divide p by f exactly. -/
private def tryDivide (p f : Poly Rat) : Option (Poly Rat × Poly Rat) :=
  if f.coeffs.size ≤ 1 then none
  else
    let (q, r) := Poly.divMod p f
    if r.coeffs.size == 0 then some (f, q) else none

/-- Try to find a quadratic factor via Kronecker's method. -/
private partial def findQuadraticFactor (p : Poly Rat) : Option (Poly Rat × Poly Rat) :=
  if p.coeffs.size < 5 then none  -- degree < 4
  else
    let v0 := Poly.eval p 0
    let v1 := Poly.eval p 1
    let vm1 := Poly.eval p (-1)
    let d0 := if v0 == 0 then [(0 : Rat)] else rationalDivisors v0
    let d1 := if v1 == 0 then [(0 : Rat)] else rationalDivisors v1
    let dm1 := if vm1 == 0 then [(0 : Rat)] else rationalDivisors vm1
    let candidates := d0.flatMap fun a =>
      d1.flatMap fun f1 =>
        dm1.filterMap fun fm1 =>
          let b := (f1 - fm1) / 2
          let c := (f1 + fm1) / 2 - a
          if c != (0 : Rat) then some (Poly.mkPoly #[a, b, c]) else none
    match candidates.filterMap (tryDivide p) with
    | [] => none
    | (f, q) :: _ => some (Poly.monic f, q)

-- ---------------------------------------------------------------------------
-- Square-free factorization
-- ---------------------------------------------------------------------------

/-- Square-free decomposition: returns list of (square-free factor, multiplicity). -/
partial def squareFreeDecomp (p : Poly Rat) : List (Poly Rat × Int) :=
  if p.coeffs.size ≤ 1 then []
  else go (Poly.monic p) 1
where
  go (f : Poly Rat) (m : Int) : List (Poly Rat × Int) :=
    let g := Poly.gcd f (Poly.diff f)
    if g.coeffs.size ≤ 1 then
      -- f is square-free
      if f.coeffs.size ≤ 1 then [] else [(f, m)]
    else
      let sfPart := (Poly.divMod f g).1
      let rest := go (Poly.monic g) (m + 1)
      if sfPart.coeffs.size ≤ 1 then rest
      else (Poly.monic sfPart, m) :: rest

-- ---------------------------------------------------------------------------
-- Main factoring
-- ---------------------------------------------------------------------------

/-- Factor a square-free polynomial over Q into irreducible factors. -/
partial def factorSquareFree (p : Poly Rat) : List (Poly Rat) :=
  if p.coeffs.size ≤ 1 then []
  else if p.coeffs.size == 2 then [Poly.monic p]
  else go (Poly.monic p)
where
  go (f : Poly Rat) : List (Poly Rat) :=
    if f.coeffs.size ≤ 2 then [f]
    else
      match findLinearFactor f with
      | some (root, quotient) =>
        Poly.mkPoly #[-root, 1] :: go quotient
      | none =>
        match findQuadraticFactor f with
        | some (fac, quotient) =>
          fac :: go quotient
        | none => [f]

/-- Factor a polynomial over Q into irreducible factors with multiplicities. -/
partial def factorPoly (p : Poly Rat) : List (Poly Rat × Int) :=
  if p.coeffs.size ≤ 1 then []
  else
    let sfFactors := squareFreeDecomp p
    sfFactors.flatMap fun (f, m) =>
      (factorSquareFree f).map fun g => (g, m)

/-- Check if a polynomial is irreducible over Q. -/
partial def isIrreducible (p : Poly Rat) : Bool :=
  if p.coeffs.size ≤ 2 then p.coeffs.size == 2
  else (factorSquareFree p).length == 1

end Surd
