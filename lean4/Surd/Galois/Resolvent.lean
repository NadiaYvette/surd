/-
  Surd.Galois.Resolvent — Resolvent polynomials for Galois group computation.

  Given an irreducible f(x) of degree n with roots α₁,...,αₙ, an invariant
  θ(x₁,...,xₙ), and coset representatives σ₁,...,σₘ, the resolvent is:

    R_θ(x) = ∏ᵢ (x - θ(α_{σᵢ(1)}, ..., α_{σᵢ(n)}))

  Roots approximated numerically via Aberth–Ehrlich, coefficients
  recovered via bounded-denominator rational approximation.
-/
import Surd.Poly.Univariate
import Surd.Poly.Factoring
import Surd.Poly.Resultant
import Surd.Radical.Eval
import Surd.Rat
import Std.Internal.Rat

open Std.Internal

namespace Surd

-- ---------------------------------------------------------------------------
-- Complex number helpers (using Float pairs)
-- ---------------------------------------------------------------------------

private structure CFloat where
  re : Float
  im : Float
  deriving Inhabited

private def cfAdd (a b : CFloat) : CFloat := ⟨a.re + b.re, a.im + b.im⟩
private def cfSub (a b : CFloat) : CFloat := ⟨a.re - b.re, a.im - b.im⟩
private def cfMul (a b : CFloat) : CFloat :=
  ⟨a.re * b.re - a.im * b.im, a.re * b.im + a.im * b.re⟩
private def cfDiv (a b : CFloat) : CFloat :=
  let d := b.re * b.re + b.im * b.im
  if d == 0 then ⟨0, 0⟩
  else ⟨(a.re * b.re + a.im * b.im) / d, (a.im * b.re - a.re * b.im) / d⟩
private def cfNeg (a : CFloat) : CFloat := ⟨-a.re, -a.im⟩
private def cfMag (a : CFloat) : Float := Float.sqrt (a.re * a.re + a.im * a.im)
private def cfOfFloat (r : Float) : CFloat := ⟨r, 0⟩
private def cfZero : CFloat := ⟨0, 0⟩
private def cfOne : CFloat := ⟨1, 0⟩

-- ---------------------------------------------------------------------------
-- Aberth–Ehrlich root finding
-- ---------------------------------------------------------------------------

/-- Cauchy's upper bound on absolute values of roots. -/
private def cauchyBound (p : Poly Rat) : Float :=
  let cs := p.coeffs
  if cs.size ≤ 1 then 1.0
  else
    let n := cs.size - 1
    let an := Float.abs (ratToFloat cs[n]!)
    if an < 1e-300 then 1.0
    else
      let maxRatio := (List.range n).foldl (fun acc i =>
        let r := Float.abs (ratToFloat cs[i]!) / an
        if r > acc then r else acc
      ) 0
      1.0 + maxRatio

/-- Evaluate a rational polynomial at a complex point (Horner's method). -/
private def evalC (p : Poly Rat) (z : CFloat) : CFloat :=
  let cs := p.coeffs.toList.reverse
  cs.foldl (fun acc c => cfAdd (cfOfFloat (ratToFloat c)) (cfMul acc z)) cfZero

/-- Aberth–Ehrlich simultaneous iteration for all roots. -/
private partial def aberthEhrlich (poly : Poly Rat) : List CFloat :=
  let n := poly.coeffs.size - 1
  if n == 0 then []
  else
    let r := cauchyBound poly
    let piF : Float := 3.141592653589793
    let z0 : List CFloat := (List.range n).map fun k =>
      let angle := 2 * piF * k.toFloat / n.toFloat + 0.37
      ⟨r * Float.cos angle, r * Float.sin angle⟩
    let poly' := Poly.diff poly
    go z0 500 poly poly'
where
  go (zs : List CFloat) (iter : Nat) (poly poly' : Poly Rat) : List CFloat :=
    match iter with
    | 0 => zs
    | fuel + 1 =>
      let zs' := step zs poly poly'
      let maxShift := zs.zip zs' |>.foldl (fun acc (a, b) =>
        let d := cfMag (cfSub a b)
        if d > acc then d else acc
      ) (0 : Float)
      if maxShift < 1e-15 then zs' else go zs' fuel poly poly'
  step (zs : List CFloat) (poly poly' : Poly Rat) : List CFloat :=
    zs.enum.map fun (i, z) =>
      let fz := evalC poly z
      let f'z := evalC poly' z
      if cfMag f'z < 1e-300 then z
      else
        let w := cfDiv fz f'z
        let s := zs.enum.foldl (fun acc (j, zj) =>
          if j == i then acc
          else cfAdd acc (cfDiv cfOne (cfSub z zj))
        ) cfZero
        let denom := cfSub cfOne (cfMul w s)
        if cfMag denom < 1e-300 then cfSub z w
        else cfSub z (cfDiv w denom)

/-- Approximate all complex roots of a polynomial over Q. -/
partial def complexRootsOf (p : Poly Rat) : List CFloat :=
  if p.coeffs.size ≤ 1 then []
  else if p.coeffs.size == 2 then
    let c0 := ratToFloat p.coeffs[0]!
    let c1 := ratToFloat p.coeffs[1]!
    [⟨-c0 / c1, 0⟩]
  else aberthEhrlich p

-- ---------------------------------------------------------------------------
-- Resolvent construction
-- ---------------------------------------------------------------------------

/-- Build ∏ᵢ (x - rᵢ) from complex values. -/
private def polyFromRootsC (roots : List CFloat) : List CFloat :=
  roots.foldl (fun cs r =>
    let shifted := cfZero :: cs
    let scaled := cs.map (cfMul (cfNeg r)) ++ [cfZero]
    shifted.zip scaled |>.map fun (a, b) => cfAdd a b
  ) [cfOne]

/-- Bounded-denominator rational approximation. -/
private def bestRational (x : Float) : Rat :=
  let sign : Float := if x < 0 then -1 else 1
  let ax := Float.abs x
  let (_, bestN, bestD) := (List.range 10000).foldl (fun (bestErr, bestN, bestD) d' =>
    let d := d' + 1
    let n := Float.round (ax * d.toFloat)
    let approx := n / d.toFloat
    let err := Float.abs (approx - ax)
    if err < bestErr then (err, n, d) else (bestErr, bestN, bestD)
  ) (1e20, 0.0, 1)
  let nInt : Int := (sign * bestN).toUInt64.toNat |> Int.ofNat
  let nFinal : Int := if x < 0 then -((Float.abs (sign * bestN)).toUInt64.toNat |> Int.ofNat) else nInt
  (nFinal : Rat) / (Int.ofNat bestD : Rat)

/-- Round complex polynomial coefficients to rationals. -/
private def roundPolyC (cs : List CFloat) : Option (Poly Rat) :=
  let results := cs.map fun c =>
    let threshold := if Float.abs c.re > 1 then Float.abs c.re else 1
    if Float.abs c.im > 1e-4 * threshold then none
    else some (bestRational c.re)
  if results.any (·.isNone) then none
  else some (Poly.mkPoly (results.filterMap id |>.toArray))

/-- Compute a resolvent polynomial from numerical roots and an invariant.
    Returns None if coefficients are not rational. -/
def resolventFromRoots
    (roots : List CFloat)
    (theta : List CFloat → CFloat)
    (perms : List (List Nat))
    : Option (Poly Rat) :=
  let values := perms.map fun sigma =>
    theta (sigma.map fun j => roots.get! j)
  let polyC := polyFromRootsC values
  roundPolyC polyC

-- ---------------------------------------------------------------------------
-- Discriminant and rational tests
-- ---------------------------------------------------------------------------

/-- Discriminant of a polynomial over Q. -/
def discriminantOf (f : Poly Rat) : Rat :=
  if f.coeffs.size ≤ 1 then 0
  else
    let n := f.coeffs.size - 1
    let lc := f.coeffs[f.coeffs.size - 1]!
    let f' := Poly.diff f
    let res := Poly.resultant f f'
    let sign : Rat := if (n * (n - 1) / 2) % 2 == 0 then 1 else -1
    sign * res / lc

/-- Integer square root via Newton's method. -/
private partial def integerSquareRoot (n : Int) : Int :=
  if n ≤ 0 then 0
  else go (Int.ofNat (Nat.max 1 (Float.sqrt (intToFloat n) |>.toUInt64.toNat))) n
where
  go (x n : Int) : Int :=
    let x' := (x + n / x) / 2
    if x' ≥ x then x else go x' n

/-- Test whether a non-negative integer is a perfect square. -/
private def isSquareInteger (n : Int) : Bool :=
  if n < 0 then false
  else if n == 0 then true
  else
    let s := integerSquareRoot n
    s * s == n

/-- Test whether a rational number is a perfect square in Q. -/
def isSquareRational (r : Rat) : Bool :=
  if r < 0 then false
  else if r == 0 then true
  else
    let n := if r.num < 0 then -r.num else r.num
    let d := Int.ofNat r.den
    isSquareInteger n && isSquareInteger d

/-- Check whether a polynomial over Q has at least one rational root. -/
def hasRationalRoot (p : Poly Rat) : Bool :=
  !(rationalRoots p).isEmpty

end Surd
