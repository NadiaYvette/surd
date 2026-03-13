/-
  Surd.Poly.Resultant — Polynomial resultant, interpolation, and composed polynomials.

  - polyResultant: resultant via pseudo-remainder Euclidean algorithm
  - lagrangeInterpolate: polynomial through given points
  - composedSum/composedProduct: polynomials whose roots are sums/products of roots
-/
import Surd.Poly.Univariate
import Std.Internal.Rat

open Std.Internal

namespace Surd

namespace Poly

variable {k : Type} [BEq k] [Add k] [Sub k] [Mul k] [Neg k] [Div k]
  [OfNat k 0] [OfNat k 1] [Inhabited k]

/-- Polynomial resultant via Euclidean algorithm.
    Res(p, q) = 0 iff p and q share a common root. -/
partial def resultant (p q : Poly k) : k :=
  if q.isZero then 0
  else if q.degreeInt == 0 then
    match q.leadCoeff with
    | some c => powK c (degIntNat p.degreeInt)
    | none => 0
  else
    let (_, r) := divMod p q
    if r.isZero then 0
    else
      let dp := degIntNat p.degreeInt
      let dq := degIntNat q.degreeInt
      let dr := degIntNat r.degreeInt
      match q.leadCoeff with
      | some lq =>
        let sign : k := if (dp * dq) % 2 == 0 then 1 else -(1 : k)
        let lqPow := powK lq (dp - dr)
        sign * resultant q r / lqPow
      | none => 0
where
  degIntNat (d : Int) : Nat := if d < 0 then 0 else d.toNat
  powK (x : k) : Nat → k
    | 0 => 1
    | 1 => x
    | n + 1 => x * powK x n

/-- Negate the variable: p(-x). Negates odd-degree coefficients. -/
def negateVar (p : Poly k) : Poly k :=
  let cs := p.coeffs.mapIdx fun i c =>
    if i % 2 == 1 then -(c : k) else c
  ⟨cs⟩

/-- Reciprocal polynomial: x^n · p(1/x). Reverses coefficients. -/
def reciprocalPoly (p : Poly k) : Poly k :=
  mkPoly p.coeffs.reverse

/-- Substitute x^n into polynomial: p(x^n). Spaces out coefficients. -/
def substituteXN (n : Nat) (p : Poly k) : Poly k :=
  if p.isZero || n == 0 then p
  else
    let sz := (p.coeffs.size - 1) * n + 1
    let cs := Array.mkArray sz (0 : k)
    let cs := p.coeffs.foldl (init := (cs, 0)) fun (arr, idx) c =>
      (arr.set! idx c, idx + n)
    mkPoly cs.1

/-- One pass of synthetic division: returns (quotient coeffs, remainder). -/
private def syntheticDiv (cs : Array k) (a : k) : Array k × k :=
  let n := cs.size
  if n == 0 then (#[], 0)
  else Id.run do
    let mut acc := cs.back!
    let mut result := Array.mkEmpty (n - 1)
    for i in [1:n] do
      let idx := n - 1 - i
      let next := cs[idx]! + acc * a
      result := result.push acc
      acc := next
    return (result.reverse, acc)

/-- Evaluate p(x + a): shift polynomial by constant a.
    Uses iterated synthetic division. -/
def shiftPoly (p : Poly k) (a : k) : Poly k :=
  if p.isZero then p
  else
    let n := p.coeffs.size
    go p.coeffs a n #[]
where
  go (cs : Array k) (a : k) : (n : Nat) → Array k → Poly k
    | 0, result => mkPoly result
    | n + 1, result =>
      let (cs', rem) := syntheticDiv cs a
      go cs' a n (result.push rem)

/-- Lagrange interpolation: find the unique polynomial of degree < n
    passing through the given points. -/
def lagrangeInterpolate (points : List (k × k)) : Poly k :=
  points.foldl (fun acc (xi, yi) =>
    let basis := lagrangeBasis xi points
    Poly.add acc (Poly.scale yi basis)
  ) Poly.zero
where
  lagrangeBasis (xi : k) (pts : List (k × k)) : Poly k :=
    pts.foldl (fun acc (xj, _) =>
      if xi == xj then acc
      else
        let denom := xi - xj
        let factor := mkPoly #[-(xj / denom), (1 : k) / denom]
        Poly.mul acc factor
    ) (Poly.const 1)

/-- Convert Nat to field element via repeated addition of 1. -/
private def natToK [Add k] [OfNat k 0] [OfNat k 1] : Nat → k
  | 0 => 0
  | n + 1 => natToK n + 1

/-- Composed sum polynomial: roots are α_i + β_j where α_i are roots of p
    and β_j are roots of q. Computed via resultant + interpolation. -/
partial def composedSum (p q : Poly k) : Poly k :=
  let dp := degIntNat p.degreeInt
  let dq := degIntNat q.degreeInt
  let n := dp * dq
  let points := (List.range (n + 1)).map fun i =>
    let x0 : k := natToK i
    let qShifted := substituteNeg q x0
    let r := resultant p qShifted
    (x0, r)
  lagrangeInterpolate points
where
  degIntNat (d : Int) : Nat := if d < 0 then 0 else d.toNat
  substituteNeg (q : Poly k) (x0 : k) : Poly k :=
    let shifted := shiftPoly q x0
    negateVar shifted

/-- Composed product polynomial: roots are α_i · β_j. -/
partial def composedProduct (p q : Poly k) : Poly k :=
  let dp := degIntNat p.degreeInt
  let dq := degIntNat q.degreeInt
  let n := dp * dq
  let points := (List.range (n + 1)).map fun i =>
    let x0 : k := natToK i
    let qScaled := scaledReciprocal q x0 dq
    let r := resultant p qScaled
    (x0, r)
  lagrangeInterpolate points
where
  degIntNat (d : Int) : Nat := if d < 0 then 0 else d.toNat
  scaledReciprocal (q : Poly k) (x0 : k) (_dq : Nat) : Poly k :=
    let qRev := reciprocalPoly q
    shiftPoly (substituteXN 1 qRev) x0

end Poly

end Surd
