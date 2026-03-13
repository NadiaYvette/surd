/-
  Surd.Field.Extension — Arithmetic in simple algebraic extension fields K(α).

  Elements of K(α) are represented as polynomials in α of degree
  less than the degree of the minimal polynomial.

  ExtElem has Add/Mul/Neg/Div instances enabling natural arithmetic.
-/
import Surd.Poly.Univariate
import Surd.Rat
import Std.Internal.Rat

open Std.Internal

namespace Surd

-- ---------------------------------------------------------------------------
-- Extension field
-- ---------------------------------------------------------------------------

/-- A simple algebraic extension K(α)/K defined by the minimal
    polynomial of α over K. -/
structure ExtField (k : Type) where
  genMinPoly : Poly k    -- Minimal polynomial (monic, irreducible)
  extDegree : Int         -- Degree of the extension
  extName : String        -- Display name for the generator
  deriving Inhabited

/-- An element of an extension field, represented as a polynomial
    in the generator of degree < extDegree. -/
structure ExtElem (k : Type) where
  elemPoly : Poly k
  elemField : ExtField k
  deriving Inhabited

/-- Construct an extension field from an irreducible polynomial. -/
def mkExtField (p : Poly Rat) (name : String) : ExtField Rat :=
  let mp := Poly.monic p
  { genMinPoly := mp
    extDegree := Int.ofNat mp.coeffs.size - 1
    extName := name }

/-- A sentinel field used by literal constructors. -/
def sentinelField {k : Type} : ExtField k := ⟨⟨#[]⟩, 0, "<literal>"⟩

/-- Check whether a field is the sentinel. -/
def isSentinelField {k : Type} (f : ExtField k) : Bool :=
  f.extDegree == 0

/-- Pick the "real" field from two operands. -/
private def pickField {k : Type} (f g : ExtField k) : ExtField k :=
  if isSentinelField f then g else f

-- ---------------------------------------------------------------------------
-- Core operations
-- ---------------------------------------------------------------------------

section ExtOps
variable {k : Type}
variable [BEq k] [Add k] [Sub k] [Mul k] [Neg k] [Div k]
  [OfNat k 0] [OfNat k 1] [Inhabited k]

/-- Reduce a polynomial modulo the minimal polynomial. -/
private def reduce (field : ExtField k) (p : Poly k) : Poly k :=
  if isSentinelField field then p
  else (Poly.divMod p field.genMinPoly).2

/-- Embed a base field element into the extension. -/
def embed (field : ExtField k) (c : k) : ExtElem k :=
  ⟨Poly.mkPoly #[c], field⟩

/-- The generator α of the extension. -/
def generator (field : ExtField k) : ExtElem k :=
  ⟨Poly.mkPoly #[0, 1], field⟩

/-- Add extension elements. -/
def extAdd (a b : ExtElem k) : ExtElem k :=
  let f := pickField a.elemField b.elemField
  ⟨reduce f (Poly.add a.elemPoly b.elemPoly), f⟩

/-- Subtract extension elements. -/
def extSub (a b : ExtElem k) : ExtElem k :=
  let f := pickField a.elemField b.elemField
  ⟨reduce f (Poly.sub a.elemPoly b.elemPoly), f⟩

/-- Negate an extension element. -/
def extNeg (a : ExtElem k) : ExtElem k :=
  ⟨Poly.scale (-1 : k) a.elemPoly, a.elemField⟩

/-- Multiply extension elements. -/
def extMul (a b : ExtElem k) : ExtElem k :=
  let f := pickField a.elemField b.elemField
  ⟨reduce f (Poly.mul a.elemPoly b.elemPoly), f⟩

/-- Extended Euclidean algorithm for polynomials.
    Returns (g, s, t) such that g = s·a + t·b, with g monic. -/
partial def extGcd (a b : Poly k) : Poly k × Poly k × Poly k :=
  go a b (Poly.mkPoly #[1]) (Poly.mkPoly #[]) (Poly.mkPoly #[]) (Poly.mkPoly #[1])
where
  go (r0 r1 s0 s1 t0 t1 : Poly k) : Poly k × Poly k × Poly k :=
    if r1.coeffs.size == 0 then
      match Poly.leadCoeff r0 with
      | some lc =>
        let g := Poly.scale (1 / lc) r0
        let s := Poly.scale (1 / lc) s0
        let t := Poly.scale (1 / lc) t0
        (g, s, t)
      | none => (r0, s0, t0)
    else
      let (q, r) := Poly.divMod r0 r1
      let s2 := Poly.sub s0 (Poly.mul q s1)
      let t2 := Poly.sub t0 (Poly.mul q t1)
      go r1 r s1 s2 t1 t2

/-- Multiplicative inverse via extended Euclidean algorithm. -/
partial def extInv (a : ExtElem k) : ExtElem k :=
  if a.elemPoly.coeffs.size == 0 then a  -- division by zero, return as-is
  else
    let (_, s, _) := extGcd a.elemPoly a.elemField.genMinPoly
    ⟨reduce a.elemField s, a.elemField⟩

/-- Division. -/
partial def extDiv (a b : ExtElem k) : ExtElem k :=
  extMul a (extInv b)

/-- Exponentiation. -/
partial def extPow (e : ExtElem k) (n : Int) : ExtElem k :=
  if n == 0 then embed e.elemField 1
  else if n < 0 then extPow (extInv e) (-n)
  else if n % 2 == 0 then
    let half := extPow e (n / 2)
    extMul half half
  else extMul e (extPow e (n - 1))

/-- Equality check. -/
def extEq (a b : ExtElem k) : Bool :=
  a.elemPoly == b.elemPoly

end ExtOps

-- ---------------------------------------------------------------------------
-- Instances for ExtElem Rat
-- ---------------------------------------------------------------------------

instance : BEq (ExtElem Rat) where
  beq := extEq

instance : Add (ExtElem Rat) where
  add := extAdd

instance : Sub (ExtElem Rat) where
  sub := extSub

instance : Neg (ExtElem Rat) where
  neg := extNeg

instance : Mul (ExtElem Rat) where
  mul := extMul

instance : Div (ExtElem Rat) where
  div := extDiv

instance : OfNat (ExtElem Rat) 0 where
  ofNat := ⟨Poly.mkPoly #[], sentinelField⟩

instance : OfNat (ExtElem Rat) 1 where
  ofNat := ⟨Poly.mkPoly #[1], sentinelField⟩

end Surd
