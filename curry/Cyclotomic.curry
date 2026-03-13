--- Cyclotomic polynomials and related number-theoretic utilities.
---
--- The nth cyclotomic polynomial Phi_n(x) is the minimal polynomial
--- of primitive nth roots of unity over Q.  It has degree phi(n).
module Cyclotomic
  ( cyclotomic
  , eulerTotient
  , moebiusMu
  ) where

import Rational
import Poly
import Positive (unsafePositive)
import PrimeFactors (factorise)

--- Compute the nth cyclotomic polynomial Phi_n(x) over Q.
---
--- Uses the identity: x^n - 1 = prod_{d|n} Phi_d(x)
--- So Phi_n(x) = (x^n - 1) / prod_{d|n, d<n} Phi_d(x)
cyclotomic :: Int -> Poly
cyclotomic n
  | n <= 0    = error "cyclotomic: non-positive argument"
  | n == 1    = mkPoly [Rational.fromInt (negate 1), Rational.fromInt 1]
  | otherwise =
      let xnm1  = xToTheNMinus1 n
          divs  = properDivisors n
          denom = foldl mulPoly (constPoly (Rational.fromInt 1))
                        (map cyclotomic divs)
          (q, _) = divModPoly xnm1 denom
      in q

--- x^n - 1
xToTheNMinus1 :: Int -> Poly
xToTheNMinus1 n =
  mkPoly (Rational.fromInt (negate 1) : replicate (n - 1) (Rational.fromInt 0)
          ++ [Rational.fromInt 1])

--- Proper divisors of n (all divisors except n itself).
properDivisors :: Int -> [Int]
properDivisors n = [d | d <- [1 .. n - 1], n `mod` d == 0]

--- Euler's totient function phi(n).
eulerTotient :: Int -> Int
eulerTotient n
  | n <= 0    = error "eulerTotient: non-positive argument"
  | n == 1    = 1
  | otherwise =
      let fs = factorise (unsafePositive n)
      in foldl (\acc (p, e) -> acc * (p - 1) * intPow p (e - 1)) 1 fs

--- Moebius function mu(n).
moebiusMu :: Int -> Int
moebiusMu n
  | n <= 0    = error "moebiusMu: non-positive argument"
  | n == 1    = 1
  | otherwise =
      let fs = factorise (unsafePositive n)
      in if any (\(_, e) -> e > 1) fs
         then 0
         else if even (length fs) then 1 else negate 1

--- Integer power.
intPow :: Int -> Int -> Int
intPow b e
  | e == 0    = 1
  | otherwise = b * intPow b (e - 1)
