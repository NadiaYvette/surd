||| Algebraic field extensions K(alpha) where alpha is a root of an
||| irreducible polynomial over K.
|||
||| Elements of K(alpha) are represented as polynomials in alpha of
||| degree < deg(minPoly), with arithmetic modulo the minimal polynomial.
||| Multiplicative inverse is computed via the extended Euclidean algorithm.
module Surd.Extension

import Surd.Rational
import Surd.Poly
import Surd.Cyclotomic  -- for Ring/Field Rational instances

import Data.List

%default covering

------------------------------------------------------------------------
-- Field extension K(alpha) where alpha is a root of minPoly
------------------------------------------------------------------------

||| A field extension K(alpha), defined by a minimal polynomial.
export
record ExtField k where
  constructor MkExtField
  efMinPoly : Poly k   -- minimal polynomial of alpha
  efName    : String    -- display name for the generator

||| An element of K(alpha), represented as a polynomial in alpha
||| of degree < deg(minPoly).
export
record ExtElem k where
  constructor MkExtElem
  eeField : ExtField k
  eePoly  : Poly k       -- representation as polynomial in alpha

||| Create an extension field from a minimal polynomial.
export
mkExtField : Poly k -> String -> ExtField k
mkExtField = MkExtField

||| The generator alpha of the extension K(alpha).
export
generator : Ring k => ExtField k -> ExtElem k
generator ef = MkExtElem ef monoX

||| Embed a scalar from K into K(alpha).
export
embedScalar : Ring k => ExtField k -> k -> ExtElem k
embedScalar ef c = MkExtElem ef (constPoly c)

||| Reduce a polynomial modulo the minimal polynomial.
reduce : Field k => ExtField k -> Poly k -> Poly k
reduce ef p = snd (divModPoly p (efMinPoly ef))

||| Add two elements of the extension field.
export
extAdd : Field k => ExtElem k -> ExtElem k -> ExtElem k
extAdd (MkExtElem ef a) (MkExtElem _ b) =
  MkExtElem ef (reduce ef (addPoly a b))

||| Negate an element.
export
extNeg : Field k => ExtElem k -> ExtElem k
extNeg (MkExtElem ef a) = MkExtElem ef (negPoly a)

||| Subtract two elements.
export
extSub : Field k => ExtElem k -> ExtElem k -> ExtElem k
extSub a b = extAdd a (extNeg b)

||| Multiply two elements.
export
extMul : Field k => ExtElem k -> ExtElem k -> ExtElem k
extMul (MkExtElem ef a) (MkExtElem _ b) =
  MkExtElem ef (reduce ef (mulPoly a b))

------------------------------------------------------------------------
-- Extended GCD for inverse computation
------------------------------------------------------------------------

||| Extended GCD: returns (g, s, t) such that s*a + t*b = g.
extGcdPoly : Field k => Poly k -> Poly k -> (Poly k, Poly k, Poly k)
extGcdPoly a (MkPoly []) = (monicPoly a, constPoly rOne, zeroPoly)
extGcdPoly a b =
  let (q, r) = divModPoly a b
      (g, s, t) = extGcdPoly b r
      -- g = s*b + t*r = s*b + t*(a - q*b) = t*a + (s - t*q)*b
  in (g, t, subPoly s (mulPoly t q))

||| Inverse of an element in the extension field.
||| Uses extended GCD: if gcd(a, minpoly) = 1, then s*a + t*minpoly = 1,
||| so a^(-1) = s mod minpoly.
export
extInv : Field k => ExtElem k -> ExtElem k
extInv (MkExtElem ef a) =
  let (g, s, _) = extGcdPoly a (efMinPoly ef)
      -- Normalise so g is monic (should be 1 for irreducible minpoly)
      lc = case leadCoeff g of Just c => c; Nothing => rOne
      sinv = scalePoly (rInv lc) s
  in MkExtElem ef (reduce ef sinv)

||| Divide two elements.
export
extDiv : Field k => ExtElem k -> ExtElem k -> ExtElem k
extDiv a b = extMul a (extInv b)

||| Power of an element (n >= 1).
export
extPow : Field k => ExtElem k -> Nat -> ExtElem k
extPow e Z = MkExtElem (eeField e) (constPoly rOne)
extPow e (S Z) = e
extPow e (S k) = extMul e (extPow e k)

------------------------------------------------------------------------
-- Adjoin a root
------------------------------------------------------------------------

||| Adjoin an nth root to a field: given radicand r in K and degree n,
||| construct K(alpha) where alpha^n = r.
||| The minimal polynomial is x^n - r.
export
adjoinRoot : (Ring k, Field k) => Int -> k -> (ExtField k, ExtElem k)
adjoinRoot n r =
  let minPoly = mkPoly (rNeg r :: replicate (cast (minus (cast n) 1)) (the k rZero) ++ [rOne])
      field = mkExtField minPoly ("root" ++ show n)
  in (field, generator field)

------------------------------------------------------------------------
-- Show
------------------------------------------------------------------------

export
(Show k) => Show (ExtField k) where
  show ef = "ExtField(" ++ efName ef ++ ", " ++ show (efMinPoly ef) ++ ")"

export
(Show k) => Show (ExtElem k) where
  show (MkExtElem ef p) = "ExtElem(" ++ show p ++ " in " ++ efName ef ++ ")"

export
(Eq k) => Eq (ExtElem k) where
  (MkExtElem _ a) == (MkExtElem _ b) = a == b
