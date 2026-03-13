--- Resolvent polynomials for Galois group computation.
---
--- Resolvent polynomials are the primary tool for computing Galois groups.
--- Given an irreducible polynomial f(x) of degree n with roots alpha_i,
--- and an invariant theta, the resolvent R_theta(x) has rational coefficients
--- and determines which subgroup the Galois group lies in.
module Resolvent
  ( discriminant
  , sexticResolvent
  , hasRationalRoot
  , isDiscSquare
  ) where

import Rational
import Poly
import Factoring (rationalRoots)

--- Local aliases.
rZero :: Rational
rZero = Rational.fromInt 0

rOne :: Rational
rOne = Rational.fromInt 1

--- Compute the discriminant of a polynomial.
---
--- disc(f) = (-1)^(n(n-1)/2) * Res(f, f') / lc(f)
--- where Res is the resultant and f' is the derivative.
discriminant :: Poly -> Rational
discriminant p =
  let p' = diffPoly p
      n  = degree p
      -- Use resultant via Euclidean algorithm
      res = polyResultantLocal p p'
      lc = case leadCoeff p of
             Just c  -> c
             Nothing -> rOne
      sign = if (n * (n - 1) `div` 2) `mod` 2 == 0 then rOne
             else Rational.fromInt (negate 1)
  in ratMul sign (ratDiv res lc)

--- Simple resultant (duplicated to avoid circular imports).
polyResultantLocal :: Poly -> Poly -> Rational
polyResultantLocal f g
  | degree f < 0 = rZero
  | degree g < 0 = rZero
  | degree g == 0 =
      case leadCoeff g of
        Just c  -> ratPow c (degree f)
        Nothing -> rZero
  | otherwise =
      let (_, r) = divModPoly f g
          df = degree f
          dg = degree g
          lg = case leadCoeff g of
                 Just c  -> c
                 Nothing -> rOne
          dr = degree r
          s = if odd (df * dg) then Rational.fromInt (negate 1) else rOne
      in if degree r < 0
         then rZero
         else ratMul s (ratMul (ratPow lg (df - dr))
                               (polyResultantLocal g r))

--- Check if discriminant is a perfect square (over Q).
isDiscSquare :: Poly -> Bool
isDiscSquare p =
  let d = discriminant p
  in isRatPerfectSquare d

--- Check if a rational is a perfect square.
isRatPerfectSquare :: Rational -> Bool
isRatPerfectSquare r
  | r == rZero = True
  | ratLt r rZero = False
  | otherwise =
      let n = numerator r
          d = denominator r
      in isIntPerfectSquare (absInt n) && isIntPerfectSquare d

isIntPerfectSquare :: Int -> Bool
isIntPerfectSquare n =
  let s = isqrt n
  in s * s == n

isqrt :: Int -> Int
isqrt n
  | n <= 0    = 0
  | otherwise = go n
  where
    go x =
      let x' = (x + n `div` x) `div` 2
      in if x' >= x then x else go x'

absInt :: Int -> Int
absInt x = if x < 0 then negate x else x

--- Compute the sextic resolvent for degree-5 Galois group identification.
--- The F20-invariant: theta = sum_i x_i^2 * (x_{i+1}*x_{i+4} + x_{i+2}*x_{i+3})
--- has stabiliser F20 in S5, giving a degree-6 resolvent.
---
--- Since this requires numerical root approximation, this is a stub
--- that returns the zero polynomial. Proper implementation would use
--- Aberth-Ehrlich iteration for complex root approximation.
sexticResolvent :: Poly -> Poly
sexticResolvent _ = zeroPoly  -- Stub: proper implementation needs complex roots

--- Check if a polynomial has a rational root.
hasRationalRoot :: Poly -> Bool
hasRationalRoot p = not (null (rationalRoots p))

