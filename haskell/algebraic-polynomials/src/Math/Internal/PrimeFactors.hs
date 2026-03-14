-- |
-- Module      : Math.Internal.PrimeFactors
-- Description : Small prime factorisation and related utilities
-- Stability   : experimental
--
-- Trial-division based prime factorisation and primality testing.
-- Adequate for the integer sizes encountered in polynomial and algebraic
-- number computations (typically up to a few thousand digits at most).
module Math.Internal.PrimeFactors
  ( factorise
  , primeFactors
  , isPrime
  , primes
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Math.Internal.Positive (Positive, unPositive)

-- | Factorise a positive integer into @(prime, exponent)@ pairs,
-- in ascending order of prime.
--
-- Returns an empty list for the input @1@.
--
-- >>> factorise 360
-- [(2,3),(3,2),(5,1)]
-- >>> factorise 1
-- []
factorise :: Positive -> [(Integer, Int)]
factorise pos =
  let n = toInteger (unPositive pos)
  in if n == 1
     then []
     else map (\(x :| xs) -> (x, 1 + length xs)) . NE.group $ go n primes
  where
    go 1 _      = []
    go m (p:ps)
      | p * p > m = [m]
      | r == 0    = p : go q (p:ps)
      | otherwise = go m ps
      where (q, r) = m `divMod` p
    go _ []     = error "primes exhausted"  -- unreachable

-- | Distinct prime factors of a positive integer, in ascending order.
--
-- >>> primeFactors 360
-- [2,3,5]
primeFactors :: Positive -> [Integer]
primeFactors = map fst . factorise

-- | Trial-division primality test.
--
-- Checks divisibility by all primes up to \(\sqrt{n}\).
-- Adequate for the sizes encountered in this library.
--
-- >>> isPrime 7
-- True
-- >>> isPrime 6
-- False
isPrime :: Integer -> Bool
isPrime n
  | n < 2     = False
  | n < 4     = True
  | otherwise = all (\p -> n `mod` p /= 0) $ takeWhile (\p -> p * p <= n) primes

-- | Infinite list of prime numbers via trial division.
--
-- >>> take 5 primes
-- [2,3,5,7,11]
primes :: [Integer]
primes = 2 : filter isPrime [3, 5 ..]
