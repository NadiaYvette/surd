-- |
-- Module      : Math.Polynomial.Cyclotomic
-- Description : Cyclotomic polynomials and related number-theoretic functions
-- Stability   : experimental
--
-- Computation of cyclotomic polynomials \(\Phi_n(x)\), Euler's totient
-- function \(\varphi(n)\), and the Mobius function \(\mu(n)\).
--
-- The \(n\)th cyclotomic polynomial \(\Phi_n(x)\) is the minimal polynomial
-- of primitive \(n\)th roots of unity over \(\mathbb{Q}\). It has degree
-- \(\varphi(n)\) and integer coefficients.
module Math.Polynomial.Cyclotomic
  ( cyclotomic
  , euler'sTotient
  , moebiusMu
  , allCyclotomic
  ) where

import Math.Polynomial.Univariate
import Math.Internal.Positive (Positive)
import Math.Internal.PrimeFactors (factorise)

-- | Compute the \(n\)th cyclotomic polynomial \(\Phi_n(x)\) over \(\mathbb{Q}\).
--
-- Uses the identity \(x^n - 1 = \prod_{d \mid n} \Phi_d(x)\),
-- so \(\Phi_n(x) = (x^n - 1) / \prod_{d \mid n, d < n} \Phi_d(x)\).
--
-- __Precondition:__ @n > 0@.
--
-- >>> unPoly (cyclotomic 1)
-- [-1 % 1,1 % 1]
-- >>> unPoly (cyclotomic 2)
-- [1 % 1,1 % 1]
-- >>> unPoly (cyclotomic 6)
-- [1 % 1,-1 % 1,1 % 1]
cyclotomic :: Int -> Poly Rational
cyclotomic n
  | n <= 0    = error "cyclotomic: non-positive argument"
  | n == 1    = mkPoly [-1, 1]  -- x - 1
  | otherwise =
      let xnm1 = xToTheNMinus1 n
          divs = properDivisors n
          denom = foldl mulPoly (constPoly 1) [cyclotomic d | d <- divs]
          (q, _) = divModPoly xnm1 denom
      in q

-- | @x^n - 1@ as a polynomial.
xToTheNMinus1 :: Int -> Poly Rational
xToTheNMinus1 n = mkPoly $ (-1) : replicate (n - 1) 0 ++ [1]

-- | Proper divisors of @n@ (all divisors except @n@ itself).
properDivisors :: Int -> [Int]
properDivisors n = [d | d <- [1..n-1], n `mod` d == 0]

-- | Euler's totient function \(\varphi(n)\): the number of integers in
-- \(\{1, \ldots, n\}\) that are coprime to \(n\).
--
-- Computed via the formula \(\varphi(n) = n \prod_{p \mid n} (1 - 1/p)\).
--
-- __Precondition:__ @n > 0@.
--
-- >>> euler'sTotient 12
-- 4
euler'sTotient :: Int -> Int
euler'sTotient n
  | n <= 0    = error "euler'sTotient: non-positive argument"
  | n == 1    = 1
  | otherwise =
      let fs = factorise (fromIntegral n :: Positive)
      in product [fromIntegral (p - 1) * fromIntegral p ^ (e - 1) | (p, e) <- fs]

-- | Mobius function \(\mu(n)\).
--
-- * \(\mu(1) = 1\)
-- * \(\mu(n) = 0\) if \(n\) has a squared prime factor
-- * \(\mu(n) = (-1)^k\) if \(n\) is a product of \(k\) distinct primes
--
-- __Precondition:__ @n > 0@.
--
-- >>> moebiusMu 6
-- 1
-- >>> moebiusMu 4
-- 0
moebiusMu :: Int -> Int
moebiusMu n
  | n <= 0    = error "moebiusMu: non-positive argument"
  | n == 1    = 1
  | otherwise =
      let fs = factorise (fromIntegral n :: Positive)
      in if any (\(_, e) -> e > 1) fs
         then 0
         else if even (length fs) then 1 else -1

-- | Compute all cyclotomic polynomials \(\Phi_1, \Phi_2, \ldots, \Phi_n\).
--
-- Slightly more efficient than calling 'cyclotomic' individually for each
-- index, since intermediate results can be reused.
allCyclotomic :: Int -> [Poly Rational]
allCyclotomic n = map snd $ scanl step (1, cyclotomic 1) [2..n]
  where
    step _ k = (k, cyclotomic k)
