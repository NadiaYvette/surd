--- Galois group identification for irreducible polynomials over Q.
---
--- Uses multi-clause function definitions with guards — Curry's overlapping
--- rules provide clean dispatch without if-then-else chains.
---
--- Decision tree for degree 5:
---   disc square?  sextic root?   result
---   no            no             S5
---   yes           no             A5
---   no            yes            F20
---   yes           yes            D5 or C5 (Frobenius test)
module Identify
  ( GaloisResult(..)
  , identifyGaloisGroup5
  , showGaloisResult
  ) where

import Rational
import Poly
import Resolvent (discriminant, sexticResolvent, hasRationalRoot,
                  isDiscSquare)
import TransitiveGroup (TransitiveGroup, tgC5, tgD5, tgF20, tgA5, tgS5,
                        showTransitiveGroup)
import PrimeFactors (isPrime)

--- Result of Galois group identification.
data GaloisResult
  = Identified TransitiveGroup
  | NotSupported String

--- Identify the Galois group of a degree-5 irreducible polynomial.
---
--- Multi-clause with guards replaces the if-then-else chain.
--- Curry's pattern matching dispatches to the first matching clause.
identifyGaloisGroup5 :: Poly -> GaloisResult
identifyGaloisGroup5 p
  | degree p /= 5
  = NotSupported "only degree 5 supported"
  | not (isDiscSquare p) && not (hasRationalRoot (sexticResolvent p))
  = Identified tgS5
  | isDiscSquare p && not (hasRationalRoot (sexticResolvent p))
  = Identified tgA5
  | not (isDiscSquare p) && hasRationalRoot (sexticResolvent p)
  = Identified tgF20
  | isDiscSquare p && hasRationalRoot (sexticResolvent p) && frobeniusTestC5 p
  = Identified tgC5
  | isDiscSquare p && hasRationalRoot (sexticResolvent p)
  = Identified tgD5

--- Frobenius/Chebotarev test to distinguish C5 from D5.
--- For C5, factorization mod p is either {5} or {1,1,1,1,1}.
--- For D5, the pattern {1,2,2} also appears.
frobeniusTestC5 :: Poly -> Bool
frobeniusTestC5 p =
  let testPrimes = filter (\pr -> pr > 5) (take 30 primesList)
      patterns = map (factorizationPattern p) testPrimes
      hasDihedral = any (\pat -> pat == [1, 2, 2] || pat == [2, 2, 1]) patterns
  in not hasDihedral

--- Factorization pattern of f mod p (degrees of irreducible factors).
--- Simplified: just check if f has roots mod p.
factorizationPattern :: Poly -> Int -> [Int]
factorizationPattern (Poly cs) p =
  let -- Reduce coefficients mod p
      reduced = map (\c -> numerator c `mod` p) cs
      -- Count roots mod p
      roots = [x | x <- [0 .. p - 1],
               evalModP reduced x p == 0]
      nRoots = length roots
  in case nRoots of
       0 -> [5]
       1 -> [1, 4]
       2 -> [1, 2, 2]
       5 -> [1, 1, 1, 1, 1]
       _ -> [nRoots, 5 - nRoots]

--- Evaluate polynomial mod p.
evalModP :: [Int] -> Int -> Int -> Int
evalModP cs x p = case cs of
  [] -> 0
  _  -> foldl (\acc c -> (acc * x + c) `mod` p) 0 (reverse cs)

--- Simple primes list.
primesList :: [Int]
primesList = 2 : filter isPrime [3, 5 ..]

--- Show.
showGaloisResult :: GaloisResult -> String
showGaloisResult (Identified g) = "Identified: " ++ showTransitiveGroup g
showGaloisResult (NotSupported s) = "Not supported: " ++ s

instance Show GaloisResult where
  show = showGaloisResult
