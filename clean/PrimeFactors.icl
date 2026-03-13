implementation module PrimeFactors

import StdEnv
import Positive

factorise :: !Positive -> [(Int, Int)]
factorise pos
    | n == 1 = []
    = groupFactors (trialDivide n primes)
where
    n = unPositive pos

// Trial division: produce the list of prime factors (with repeats).
trialDivide :: !Int [Int] -> [Int]
trialDivide 1 _ = []
trialDivide m [p:ps]
    | p * p > m = [m]
    | m rem p == 0 = [p : trialDivide (m / p) [p:ps]]
    = trialDivide m ps
trialDivide _ [] = abort "primes exhausted"

// Group a sorted list of factors into (prime, exponent) pairs.
groupFactors :: [Int] -> [(Int, Int)]
groupFactors [] = []
groupFactors [x:xs] = [(x, 1 + length same) : groupFactors rest]
where
    (same, rest) = span (\y -> y == x) xs

primeFactors :: !Positive -> [Int]
primeFactors pos = map fst (factorise pos)

isPrime :: !Int -> Bool
isPrime n
    | n < 2 = False
    | n < 4 = True
    = all (\p -> n rem p <> 0) (takeWhile (\p -> p * p <= n) primes)

primes :: [Int]
primes = [2 : filter isPrime [3, 5..]]
