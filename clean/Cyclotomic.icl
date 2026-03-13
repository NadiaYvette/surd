implementation module Cyclotomic

import StdEnv
import Poly
import Rational
import Positive
import PrimeFactors

// Euler's totient function
eulerTotient :: !Int -> Int
eulerTotient n
    | n <= 0 = abort "eulerTotient: non-positive"
    | n == 1 = 1
    | otherwise
        # fs = factorise (unsafePositive n)
        = prodList [(p - 1) * intPow p (e - 1) \\ (p, e) <- fs]

intPow :: !Int !Int -> Int
intPow _ 0 = 1
intPow b e
    | isOdd e = b * intPow b (e - 1)
    = let half = intPow b (e / 2) in half * half

prodList :: ![Int] -> Int
prodList [] = 1
prodList [x:xs] = x * prodList xs

// Cyclotomic polynomial via Mobius inversion:
// Phi_n(x) = Product_{d|n} (x^d - 1)^{mu(n/d)}
// Implemented by dividing x^n - 1 by Phi_d for all proper divisors d.
cyclotomic :: !Int -> Poly Rational
cyclotomic n
    | n <= 0 = abort "cyclotomic: non-positive"
    | n == 1 = mkPoly [~ one, one]  // x - 1
    | otherwise
        // x^n - 1
        # xn_minus_1 = xPowMinusOne n
        // Divide by cyclotomic(d) for all proper divisors d of n
        # divs = properDivisors n
        = foldl divByPhiD xn_minus_1 divs

divByPhiD :: !(Poly Rational) !Int -> Poly Rational
divByPhiD p d = fst (divModPoly p (cyclotomic d))

// Build the polynomial x^n - 1
xPowMinusOne :: !Int -> Poly Rational
xPowMinusOne n = mkPoly ([~ one] ++ repeatn (n - 1) zero ++ [one])

// All divisors of n excluding n itself, in ascending order
properDivisors :: !Int -> [Int]
properDivisors n = [d \\ d <- [1 .. n - 1] | n rem d == 0]
