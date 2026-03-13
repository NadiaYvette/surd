implementation module Factoring

import StdEnv
import Poly
import Rational
import Positive
import PrimeFactors
import Data.Integer

// Rational root theorem: if p/q is a root of a_n x^n + ... + a_0,
// then p | a_0 and q | a_n.
rationalRoots :: !(Poly Rational) -> [Rational]
rationalRoots (Poly []) = []
rationalRoots (Poly cs)
    // Clear denominators to get integer polynomial
    # lcmDen = foldl lcmRat one [denom r \\ r <- cs]
    # intCs = [numer (r * ratFromInteger lcmDen) \\ r <- cs]
    # a0 = hd intCs
    # an = last intCs
    | a0 == toInteger 0 = [zero : rationalRoots (removeZeroRoot (Poly cs))]
    # p_divs = intDivisors a0
    # q_divs = intDivisors an
    # candidates = removeDup [mkRational p q \\ p <- p_divs, q <- q_divs | not (q == toInteger 0)]
    = [r \\ r <- candidates | evalPoly (Poly cs) r == zero]

// Remove a factor of x from a polynomial
removeZeroRoot :: !(Poly Rational) -> Poly Rational
removeZeroRoot (Poly []) = Poly []
removeZeroRoot (Poly [_:cs]) = mkPoly cs

hasRationalRoot :: !(Poly Rational) -> Bool
hasRationalRoot p = not (isEmpty (rationalRoots p))

// Simple factoring: extract rational roots, then square-free decomposition
factor :: !(Poly Rational) -> [(Poly Rational, Int)]
factor p
    | degree p <= 0 = [(p, 1)]
    = squareFree p

// LCM for Integer (positive denominator guaranteed by Rational invariant)
lcmRat :: !Integer !Integer -> Integer
lcmRat a b
    | a == toInteger 0 || b == toInteger 0 = toInteger 1
    = abs (a * b / gcdI a b)

gcdI :: !Integer !Integer -> Integer
gcdI a b
    | b == toInteger 0 = abs a
    = gcdI b (a - (a / b) * b)

// All divisors of an Integer (positive and negative)
intDivisors :: !Integer -> [Integer]
intDivisors n
    | n == toInteger 0 = [toInteger 1]
    # absN = toInt (abs n)
    # sqrtN = toInt (sqrt (toReal absN))
    # smallDivs = [d \\ d <- [1 .. sqrtN] | absN rem d == 0]
    # posDivs = removeDup (smallDivs ++ [absN / d \\ d <- smallDivs])
    # intDivs = [toInteger d \\ d <- posDivs]
    = intDivs ++ [~ d \\ d <- intDivs]
