module Surd.Cyclotomic

import Surd.Rational
import Surd.Poly
import Surd.PrimeFactors
import Surd.Positive

import Data.List

%default covering

------------------------------------------------------------------------
-- Ring/Field instances for Rational (needed for Poly operations)
------------------------------------------------------------------------

export
Ring Rational where
  rZero = Rational.zero
  rOne  = Rational.one
  rAdd  = (+)
  rMul  = (*)
  rNeg  = negate
  rSub  = (-)
  rFromInteger = Rational.fromInteger

export
Field Rational where
  rDiv = (/)
  rInv = recip

------------------------------------------------------------------------
-- Cyclotomic polynomials
------------------------------------------------------------------------

||| Compute the nth cyclotomic polynomial Phi_n(x) over Q.
|||
||| Uses the recursive formula:
|||   x^n - 1 = product of Phi_d(x) for d | n
||| So Phi_n(x) = (x^n - 1) / product of Phi_d(x) for d | n, d /= n.
export
cyclotomic : Int -> Poly Rational
cyclotomic n =
  if n <= 0 then zeroPoly
  else if n == 1 then
    -- Phi_1(x) = x - 1
    mkPoly [negate Rational.one, Rational.one]
  else
    let ni : Integer = cast n
        -- x^n - 1
        xnMinus1 : Poly Rational
        xnMinus1 = mkPoly (negate Rational.one :: replicate (cast (minus (cast n) 1)) Rational.zero ++ [Rational.one])
        -- Proper divisors of n
        divisors : List Int
        divisors = filter (\d => d > 0 && d < n && mod n d == 0)
                          (map cast [1 .. ni])
        -- Product of Phi_d for d | n, d /= n
        prodDivisors : Poly Rational
        prodDivisors = foldl mulPoly (constPoly Rational.one) (map cyclotomic divisors)
    in fst (divModPoly xnMinus1 prodDivisors)

||| Euler's totient function.
export
eulerTotient : Integer -> Integer
eulerTotient n =
  if n <= 0 then 0
  else case positive (cast n) of
    Nothing => 0
    Just pos =>
      let fs = factorise pos
      in foldl (\acc, pf =>
            let p = fst pf
                e = cast {to = Integer} (snd pf)
                pe = powInteger p (e - 1)
            in acc * (p - 1) * pe) 1 fs
  where
    powInteger : Integer -> Integer -> Integer
    powInteger _ 0 = 1
    powInteger b k = if k < 0 then 1 else b * powInteger b (k - 1)

||| Check if n is a prime power. Returns Just (p, k) if n = p^k.
export
primePowerDecomp : Int -> Maybe (Integer, Int)
primePowerDecomp n =
  if n <= 1 then Nothing
  else case positive (cast n) of
    Nothing => Nothing
    Just pos =>
      let fs = factorise pos
      in case fs of
        [(p, k)] => Just (p, cast k)
        _ => Nothing

||| Check if n is a power of 2.
export
isPowerOf2 : Int -> Bool
isPowerOf2 n =
  if n <= 0 then False
  else case primePowerDecomp n of
    Just (2, _) => True
    _ => n == 1
