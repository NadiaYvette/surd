--- Small prime factorization and related utilities.
module PrimeFactors
  ( factorise
  , primeFactors
  , isPrime
  , primes
  ) where

import Positive (Positive, unPositive)

--- Factorise a positive integer into (prime, exponent) pairs.
---
--- >>> factorise (unsafePositive 360)
--- [(2,3),(3,2),(5,1)]
factorise :: Positive -> [(Int, Int)]
factorise pos =
  let n = unPositive pos
  in if n == 1
     then []
     else groupFactors (trialDivide n primes)

--- Trial division: produce the list of prime factors with repetition.
trialDivide :: Int -> [Int] -> [Int]
trialDivide m ps
  | m == 1    = []
  | otherwise =
      case ps of
        []    -> error "primes exhausted"
        (p:rest) ->
          let (q, r) = divMod m p
          in if p * p > m then [m]
             else if r == 0 then p : trialDivide q (p:rest)
             else trialDivide m rest

--- Group a sorted list of factors into (factor, multiplicity) pairs.
groupFactors :: [Int] -> [(Int, Int)]
groupFactors [] = []
groupFactors (x:xs) =
  let (same, rest) = span (== x) xs
  in (x, 1 + length same) : groupFactors rest

--- Distinct prime factors in ascending order.
primeFactors :: Positive -> [Int]
primeFactors = map fst . factorise

--- Trial-division primality test.
isPrime :: Int -> Bool
isPrime n
  | n < 2     = False
  | n < 4     = True
  | otherwise = all (\p -> n `mod` p /= 0) (takeWhile (\p -> p * p <= n) primes)

--- Infinite list of primes via trial division.
primes :: [Int]
primes = 2 : filter isPrime [3, 5 ..]
