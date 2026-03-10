-- | Small prime factorization and related utilities.
module Surd.Internal.PrimeFactors
  ( factorise
  , primeFactors
  , isPrime
  , primes
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Surd.Internal.Positive (Positive, unPositive)

-- | Factorise a positive integer into (prime, exponent) pairs.
--
-- >>> factorise 360
-- [(2,3),(3,2),(5,1)]
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

-- | Trial-division primality test (adequate for the sizes we encounter).
isPrime :: Integer -> Bool
isPrime n
  | n < 2     = False
  | n < 4     = True
  | otherwise = all (\p -> n `mod` p /= 0) $ takeWhile (\p -> p * p <= n) primes

-- | Infinite list of primes via trial division.
primes :: [Integer]
primes = 2 : filter isPrime [3, 5 ..]

