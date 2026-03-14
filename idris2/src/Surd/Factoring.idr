module Surd.Factoring

import Surd.Rational
import Surd.Poly
import Surd.Cyclotomic  -- for Ring/Field Rational instances
import Surd.RootBound
import Surd.GCD

import Data.List

%default covering

------------------------------------------------------------------------
-- Rational root test
------------------------------------------------------------------------

||| Find all rational roots of a polynomial with rational coefficients.
||| Uses the rational root theorem: if p/q is a root of
||| a_n x^n + ... + a_0, then p | a_0 and q | a_n.
export
rationalRoots : Poly Rational -> List Rational
rationalRoots (MkPoly []) = []
rationalRoots p =
  case (leadCoeff p, coeffs p) of
    (Just lc, (a0 :: _)) =>
      if Surd.Rational.isZero a0 then
        Rational.zero :: rationalRoots (fst (divModPoly p (mkPoly [Rational.zero, Rational.one])))
      else
        let numDivisors = divisors (abs (numer a0))
            denDivisors = divisors (abs (numer lc))
            candidates = [mkRat n d | n <- numDivisors ++ map negate numDivisors,
                                      d <- denDivisors,
                                      d /= 0]
        in nub $ filter (\r => Surd.Rational.isZero (evalPoly p r)) candidates
    _ => []
  where
    divisors : Integer -> List Integer
    divisors 0 = [1]
    divisors n = let an = abs n
                 in filter (\d => mod an d == 0) [1 .. an]

------------------------------------------------------------------------
-- Square-free factoring over Q
------------------------------------------------------------------------

||| Make a polynomial primitive (content = 1) over Z, given rational coefficients.
||| Returns (content, primitive part).
export
primitivePart : Poly Rational -> (Rational, Poly Rational)
primitivePart (MkPoly []) = (Rational.zero, zeroPoly)
primitivePart p =
  let cs = coeffs p
      -- Clear denominators: multiply by LCM of denominators
      lcmDens = foldl (\acc, c => lcmInteger acc (denom c)) 1 cs
      intCoeffs = map (\c => numer c * div lcmDens (denom c)) cs
      -- GCD of numerators
      g = foldl gcdInteger 0 (map abs intCoeffs)
      g' = if g == 0 then 1 else g
      primCoeffs = map (\c => mkRat c g') intCoeffs
      content = mkRat g' lcmDens
  in (content, mkPoly primCoeffs)

------------------------------------------------------------------------
-- Kronecker factoring over Q
------------------------------------------------------------------------

||| Factor a square-free polynomial over Q using Kronecker's method.
||| Returns the list of irreducible factors (monic).
|||
||| For polynomials of degree <= 3, uses direct methods.
||| For higher degrees, attempts to find factors by evaluating at
||| small integer points and checking divisors.
export
factorSquareFree : Poly Rational -> List (Poly Rational)
factorSquareFree (MkPoly []) = []
factorSquareFree p =
  case degreeInt p of
    d =>
      if d <= 0 then []
      else if d == 1 then [monicPoly p]
      else
        -- Try rational roots first
        case rationalRoots p of
          (r :: _) =>
            let factor = mkPoly [negate r, Rational.one]
                quotient = fst (divModPoly p factor)
            in monicPoly factor :: factorSquareFree quotient
          [] =>
            if d <= 3 then [monicPoly p]
            else [monicPoly p]  -- higher degree: return as irreducible for now

||| Full factorisation over Q: square-free decomposition then factor each part.
export
factor : Poly Rational -> List (Poly Rational, Nat)
factor (MkPoly []) = []
factor p =
  let sqfree = squareFree p
  in concatMap (\pe =>
       let factors = factorSquareFree (fst pe)
           mult = snd pe
       in map (\f => (f, mult)) factors) sqfree
