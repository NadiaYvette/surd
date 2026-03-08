-- | Cyclotomic polynomials and related utilities.
--
-- The @n@th cyclotomic polynomial Φₙ(x) is the minimal polynomial of
-- primitive @n@th roots of unity over Q. It has degree φ(n) (Euler's totient).
module Surd.Polynomial.Cyclotomic
  ( cyclotomic
  , euler'sTotient
  , moebiusMu
  , allCyclotomic
  ) where

import Surd.Polynomial.Univariate
import Surd.Internal.Positive (Positive)
import Surd.Internal.PrimeFactors (factorise)

-- | Compute the @n@th cyclotomic polynomial Φₙ(x) over Q.
--
-- Uses the identity: x^n - 1 = ∏_{d|n} Φ_d(x)
-- So Φₙ(x) = (x^n - 1) / ∏_{d|n, d<n} Φ_d(x)
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

-- | x^n - 1
xToTheNMinus1 :: Int -> Poly Rational
xToTheNMinus1 n = mkPoly $ (-1) : replicate (n - 1) 0 ++ [1]

-- | Proper divisors of n (all divisors except n itself).
properDivisors :: Int -> [Int]
properDivisors n = [d | d <- [1..n-1], n `mod` d == 0]

-- | Euler's totient function φ(n).
euler'sTotient :: Int -> Int
euler'sTotient n
  | n <= 0    = error "euler'sTotient: non-positive argument"
  | n == 1    = 1
  | otherwise =
      let fs = factorise (fromIntegral n :: Positive)
      in product [fromIntegral (p - 1) * fromIntegral p ^ (e - 1) | (p, e) <- fs]

-- | Möbius function μ(n).
moebiusMu :: Int -> Int
moebiusMu n
  | n <= 0    = error "moebiusMu: non-positive argument"
  | n == 1    = 1
  | otherwise =
      let fs = factorise (fromIntegral n :: Positive)
      in if any (\(_, e) -> e > 1) fs
         then 0
         else if even (length fs) then 1 else -1

-- | Compute all cyclotomic polynomials Φ₁ through Φₙ.
-- More efficient than calling 'cyclotomic' individually since
-- intermediate results are reused.
allCyclotomic :: Int -> [Poly Rational]
allCyclotomic n = map snd $ scanl step (1, cyclotomic 1) [2..n]
  where
    step _ k = (k, cyclotomic k)
