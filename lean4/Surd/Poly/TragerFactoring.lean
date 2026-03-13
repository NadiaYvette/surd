/-
  Surd.Poly.TragerFactoring — Factoring polynomials over Q(α) via
  Trager's algorithm (1976).

  Given f(x) ∈ Q(α)[x] where α is a root of an irreducible m(t) ∈ Q[t],
  compute the norm N(x) = Res_t(m(t), f(x - s·t)) ∈ Q[x] for suitable s,
  factor N over Q, and lift factors back via GCD.
-/
import Surd.Poly.Univariate
import Surd.Poly.Factoring
import Surd.Poly.Resultant
import Surd.Field.Extension
import Surd.Rat
import Std.Internal.Rat

open Std.Internal

namespace Surd

-- ---------------------------------------------------------------------------
-- Helpers for Poly (ExtElem Rat)
-- ---------------------------------------------------------------------------

/-- Check if a polynomial over Q(α) is zero. -/
private def isZeroPolyExt (p : Poly (ExtElem Rat)) : Bool :=
  p.coeffs.all (· == (0 : ExtElem Rat))

/-- GCD of two polynomials over Q(α). -/
private partial def gcdPolyExt (a b : Poly (ExtElem Rat))
    : Poly (ExtElem Rat) :=
  if isZeroPolyExt b then Poly.monic a
  else gcdPolyExt b (Poly.divMod a b).2

/-- Lift a Q[x] polynomial to Q(α)[x] by embedding coefficients. -/
private def liftQPoly (field : ExtField Rat) (p : Poly Rat) : Poly (ExtElem Rat) :=
  ⟨p.coeffs.map (embed field)⟩

/-- Check if a rational polynomial is square-free. -/
private def isSquareFreeQ (p : Poly Rat) : Bool :=
  let p' := Poly.diff p
  let g := Poly.gcd p p'
  g.coeffs.size ≤ 1

/-- Integer to Rat helper. -/
private def intRat (n : Int) : Rat := n

-- ---------------------------------------------------------------------------
-- Norm computation
-- ---------------------------------------------------------------------------

/-- Evaluate the norm at a specific rational point x₀. -/
private def normAtPoint (field : ExtField Rat) (f : Poly (ExtElem Rat)) (x0 : Rat)
    : Rat :=
  let m := field.genMinPoly
  let coeffs := f.coeffs.toList.map (·.elemPoly)
  let fAtX0 : Poly Rat := coeffs.enum.foldl (fun acc (i, ci) =>
    let x0i := (List.range i).foldl (fun p _ => p * x0) (1 : Rat)
    Poly.add acc (Poly.scale x0i ci)
  ) (Poly.mkPoly #[])
  Poly.resultant m fAtX0

/-- Compute the norm of f(x) ∈ Q(α)[x] via evaluation + interpolation. -/
partial def normPoly (field : ExtField Rat) (f : Poly (ExtElem Rat)) : Poly Rat :=
  let df := f.coeffs.size - 1
  let dm := (field.extDegree).toNat
  let resultDeg := df * dm
  let points : List (Rat × Rat) := (List.range (resultDeg + 1)).map fun i =>
    let x0 : Rat := intRat (Int.ofNat i)
    (x0, normAtPoint field f x0)
  Poly.lagrangeInterpolate points

-- ---------------------------------------------------------------------------
-- Lift factors back
-- ---------------------------------------------------------------------------

/-- Lift Q-factors back to Q(α)-factors via GCD. -/
private partial def liftFactors (field : ExtField Rat)
    (f : Poly (ExtElem Rat))
    (sAlpha : ExtElem Rat)
    (qFactors : List (Poly Rat))
    : List (Poly (ExtElem Rat)) :=
  go f qFactors
where
  go (remaining : Poly (ExtElem Rat)) : List (Poly Rat) → List (Poly (ExtElem Rat))
    | [] => []
    | g :: gs =>
      if remaining.coeffs.size ≤ 1 then []
      else
        let gLifted := liftQPoly field g
        let gUnshifted := Poly.shiftPoly gLifted (-sAlpha)
        let h := gcdPolyExt remaining gUnshifted
        if h.coeffs.size > 1 then
          let (q, _) := Poly.divMod remaining h
          Poly.monic h :: go q gs
        else go remaining gs

-- ---------------------------------------------------------------------------
-- Trager's algorithm
-- ---------------------------------------------------------------------------

/-- Core Trager factoring loop. -/
private partial def tragerFactor (field : ExtField Rat)
    (f : Poly (ExtElem Rat)) (s : Int)
    : List (Poly (ExtElem Rat)) :=
  if s > 20 then [f]
  else
    let alpha := generator field
    let sAlpha := extMul (embed field (intRat s)) alpha
    let fShifted := Poly.shiftPoly f sAlpha
    let n := normPoly field fShifted
    if !isSquareFreeQ n then tragerFactor field f (s + 1)
    else
      let nFactors := factorSquareFree (Poly.monic n)
      if nFactors.length ≤ 1 then [Poly.monic f]
      else liftFactors field f sAlpha nFactors

/-- Factor a square-free polynomial over Q(α) into irreducible factors. -/
partial def factorSFOverExtension (field : ExtField Rat)
    (f : Poly (ExtElem Rat)) : List (Poly (ExtElem Rat)) :=
  if f.coeffs.size ≤ 1 then []
  else if f.coeffs.size == 2 then [Poly.monic f]
  else tragerFactor field f 0

/-- Factor a polynomial over Q(α) into irreducible factors.
    Returns a list of (factor, multiplicity) pairs. -/
partial def factorOverExtension (field : ExtField Rat)
    (f : Poly (ExtElem Rat)) : List (Poly (ExtElem Rat) × Int) :=
  -- Square-free decomposition for ExtElem Rat
  let sfFactors : List (Poly (ExtElem Rat) × Int) :=
    let sf := Poly.squareFree f
    if sf.coeffs.size == 0 then []
    else [(sf, 1)]
  sfFactors.flatMap fun (g, m) =>
    (factorSFOverExtension field g).map fun h => (h, m)

-- ---------------------------------------------------------------------------
-- Generalized Trager (specialized to Rat base)
-- ---------------------------------------------------------------------------

/-- Evaluate f(x₀) where f has ExtElem Rat coefficients and x₀ ∈ Rat.
    Result is a Poly Rat (the polynomial in t representing the sum). -/
private def evalPolyExtK (f : Poly (ExtElem Rat)) (x0 : Rat) : Poly Rat :=
  f.coeffs.toList.enum.foldl (fun acc (i, ee) =>
    let x0i := (List.range i).foldl (fun p _ => p * x0) (1 : Rat)
    Poly.add acc (Poly.scale x0i ee.elemPoly)
  ) (Poly.mkPoly #[])

/-- Compute the norm over Rat base field. -/
partial def normPolyK (field : ExtField Rat) (f : Poly (ExtElem Rat)) : Poly Rat :=
  let df := f.coeffs.size - 1
  let dm := field.extDegree.toNat
  let resultDeg := df * dm
  let m := field.genMinPoly
  let points : List (Rat × Rat) := (List.range (resultDeg + 1)).map fun i =>
    let x0 : Rat := intRat (Int.ofNat i)
    (x0, Poly.resultant m (evalPolyExtK f x0))
  Poly.lagrangeInterpolate points

/-- GCD over Rat. -/
private partial def gcdPolyRat (a b : Poly Rat) : Poly Rat :=
  if b.coeffs.all (· == (0 : Rat)) then Poly.monic a
  else gcdPolyRat b (Poly.divMod a b).2

/-- Check if a Rat polynomial is square-free. -/
private def isSquareFreeK (p : Poly Rat) : Bool :=
  let p' := Poly.diff p
  let g := gcdPolyRat p p'
  g.coeffs.size ≤ 1

/-- Factor a square-free polynomial over K(α) given a factoring function for K.
    Generalized Trager. -/
partial def factorSFOverExtensionK
    (factorBaseK : Poly Rat → List (Poly Rat))
    (field : ExtField Rat)
    (f : Poly (ExtElem Rat)) : List (Poly (ExtElem Rat)) :=
  if f.coeffs.size ≤ 1 then []
  else if f.coeffs.size == 2 then [Poly.monic f]
  else tragerFactorK factorBaseK field f 0
where
  tragerFactorK (factorBaseK : Poly Rat → List (Poly Rat))
      (field : ExtField Rat) (f : Poly (ExtElem Rat)) (s : Int)
      : List (Poly (ExtElem Rat)) :=
    if s > 20 then [f]
    else
      let alpha := generator field
      let sAlpha := extMul (embed field (intRat s)) alpha
      let fShifted := Poly.shiftPoly f sAlpha
      let n := normPolyK field fShifted
      if !isSquareFreeK n then tragerFactorK factorBaseK field f (s + 1)
      else
        let nFactors := factorBaseK (Poly.monic n)
        if nFactors.length ≤ 1 then [Poly.monic f]
        else liftFactors field f sAlpha nFactors

end Surd
