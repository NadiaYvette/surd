definition module PrimeFactors

// Trial-division factorization and primality testing.

from Positive import :: Positive

// Factorise a positive integer into (prime, exponent) pairs.
// factorise 360 = [(2,3),(3,2),(5,1)]
factorise :: !Positive -> [(Int, Int)]

// Distinct prime factors in ascending order.
primeFactors :: !Positive -> [Int]

// Trial-division primality test.
isPrime :: !Int -> Bool

// Infinite list of primes via trial division.
primes :: [Int]
