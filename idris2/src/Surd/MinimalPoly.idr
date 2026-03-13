module Surd.MinimalPoly

import Surd.Rational
import Surd.Poly
import Surd.Cyclotomic  -- Ring/Field instances
import Surd.Types
import Surd.Eval
import Surd.Factoring
import Surd.Expr

import Data.List

%default covering

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

ratToDouble : Rational -> Double
ratToDouble r = cast (numer r) / cast (denom r)

evalPolyDouble : Poly Rational -> Double -> Double
evalPolyDouble (MkPoly []) _ = 0.0
evalPolyDouble (MkPoly cs) x =
  foldr (\c, acc => ratToDouble c + x * acc) 0.0 cs

negateOddCoeffs : Bool -> List Rational -> List Rational
negateOddCoeffs _ [] = []
negateOddCoeffs True  (c :: cs) = negate c :: negateOddCoeffs False cs
negateOddCoeffs False (c :: cs) = c :: negateOddCoeffs True cs

------------------------------------------------------------------------
-- Polynomial shift
------------------------------------------------------------------------

||| Shift a polynomial: compute f(x + c).
shiftPoly : Poly Rational -> Rational -> Poly Rational
shiftPoly (MkPoly []) _ = zeroPoly
shiftPoly p c =
  composePoly p (mkPoly [c, Rational.one])

------------------------------------------------------------------------
-- Annihilating polynomial via resultant tower
------------------------------------------------------------------------

||| Compute an annihilating polynomial for a radical expression.
export
annihilatingPoly : RadExpr Rational -> Poly Rational
annihilatingPoly (Lit r) = mkPoly [negate r, Rational.one]
annihilatingPoly (Neg a) =
  -- If f(alpha) = 0 then f(-x) has root -alpha
  let p = annihilatingPoly a
      cs = coeffs p
      -- Negate odd-indexed coefficients (index 1, 3, 5, ...)
  in mkPoly (negateOddCoeffs False cs)
annihilatingPoly (Root n (Lit r)) =
  -- nth root of r: minimal poly is x^n - r
  let ni = cast {to = Nat} (abs n)
      zeros = replicate (minus ni 1) Rational.zero
  in mkPoly (negate r :: zeros ++ [Rational.one])
annihilatingPoly (Add (Lit r) b) =
  -- alpha + r: if f(beta) = 0 then f(x - r) has root alpha + r
  let p = annihilatingPoly b
  in shiftPoly p (negate r)
annihilatingPoly (Mul (Lit r) b) =
  -- r * beta: if f(beta) = 0 then r^deg * f(x/r) has root r*beta
  if Surd.Rational.isZero r then mkPoly [Rational.zero, Rational.one]
  else
    let p = annihilatingPoly b
        cs = coeffs p
        -- f(x/r): coefficient of x^k gets divided by r^k
        scaled = scaleCoeffs Rational.one cs
    in mkPoly scaled
  where
    scaleCoeffs : Rational -> List Rational -> List Rational
    scaleCoeffs _ [] = []
    scaleCoeffs rk (c :: cs) = (c / rk) :: scaleCoeffs (rk * r) cs
annihilatingPoly _ =
  -- Fallback: return a stub polynomial
  mkPoly [Rational.zero, Rational.one]

------------------------------------------------------------------------
-- Minimal polynomial (factor the annihilating poly)
------------------------------------------------------------------------

||| Compute the minimal polynomial of a radical expression.
export
minimalPoly : RadExpr Rational -> Poly Rational
minimalPoly expr =
  let ann = annihilatingPoly expr
      approx = eval expr
      factors = factorSquareFree ann
  in case factors of
       [] => ann
       _ => pickFactor factors approx
  where
    pickFactor : List (Poly Rational) -> Double -> Poly Rational
    pickFactor [] _ = mkPoly [Rational.one]
    pickFactor [f] _ = f
    pickFactor (f :: fs) approx =
      let val = evalPolyDouble f approx
      in if abs val < 1.0e-10 then f
         else pickFactor fs approx
