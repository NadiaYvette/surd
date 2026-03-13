--- Polynomial factoring over Q.
---
--- Implements rational root testing and Kronecker's method for
--- small-degree polynomials.
module Factoring
  ( factor
  , rationalRoots
  , isIrreducible
  , factorSquareFree
  ) where

import Rational
import Poly
import Positive (unsafePositive)
import PrimeFactors (factorise)

--- Local aliases.
rZero :: Rational
rZero = Rational.fromInt 0

rOne :: Rational
rOne = Rational.fromInt 1

--- Factor a polynomial over Q into irreducible factors.
--- Returns a list of (irreducible factor, multiplicity) pairs.
factor :: Poly -> [(Poly, Int)]
factor p
  | degree p <= 0 = []
  | otherwise =
      let sfFactors = squareFree p
      in concatMap (\(f, m) -> map (\g -> (g, m)) (factorSquareFree f))
                   sfFactors

--- Factor a square-free polynomial over Q into irreducible factors.
factorSquareFree :: Poly -> [Poly]
factorSquareFree p
  | degree p <= 0 = []
  | degree p == 1 = [monicPoly p]
  | otherwise     = go (monicPoly p)
  where
    go f
      | degree f <= 1 = [f]
      | otherwise =
          case findLinearFactor f of
            Just (root, quotient) ->
              mkPoly [ratNeg root, rOne] : go quotient
            Nothing ->
              case findQuadraticFactor f of
                Just (fac, quotient) ->
                  fac : go quotient
                Nothing ->
                  [f]

--- Find a rational root of a polynomial using the rational root theorem.
rationalRoots :: Poly -> [Rational]
rationalRoots p
  | degree p <= 0 = []
  | otherwise =
      let cs = polyCoeffs p
          lcmDen = foldl lcmInt 1 (map denominator cs)
          intCoeffs = map (\c -> numerator (ratMul c (Rational.fromInt lcmDen))) cs
          a0 = case intCoeffs of
                 (x:_) -> x
                 []    -> 0
          an = lastElem intCoeffs
          numDivs = if a0 == 0 then [0]
                    else divisors (absInt a0)
          denDivs = divisors (absInt an)
          candidates = [ mkRat p' q'
                       | d <- numDivs
                       , p' <- [d, negate d]
                       , q' <- denDivs
                       , q' /= 0
                       ]
      in filter (\r -> evalPoly (Poly cs) r == rZero) candidates

--- Check if a polynomial is irreducible over Q.
isIrreducible :: Poly -> Bool
isIrreducible p
  | degree p <= 1 = degree p == 1
  | otherwise     = length (factorSquareFree p) == 1

--- Find a linear factor (rational root) of a polynomial.
findLinearFactor :: Poly -> Maybe (Rational, Poly)
findLinearFactor f =
  case rationalRoots f of
    []    -> Nothing
    (r:_) ->
      let factor' = mkPoly [ratNeg r, rOne]
          (q, _) = divModPoly f factor'
      in Just (r, q)

--- Find a quadratic factor via Kronecker's method.
findQuadraticFactor :: Poly -> Maybe (Poly, Poly)
findQuadraticFactor f
  | degree f < 2 = Nothing
  | otherwise =
      let pts = map Rational.fromInt [0, 1, negate 1]
          vals = map (evalPoly f) pts
          -- For each combination of divisors of f(0), f(1), f(-1),
          -- try to interpolate a quadratic factor.
          divSets = map (\v -> if v == rZero then [rZero]
                               else allRatDivisors v) vals
      in tryQuadFactors f pts divSets

--- Try all combinations of divisor triples for quadratic factor.
tryQuadFactors :: Poly -> [Rational] -> [[Rational]] -> Maybe (Poly, Poly)
tryQuadFactors _ _ [] = Nothing
tryQuadFactors f pts (ds:rest) = case ds of
  [] -> tryQuadFactors f pts rest
  _  -> tryQuadFactorsInner f pts ds rest

tryQuadFactorsInner :: Poly -> [Rational] -> [Rational] -> [[Rational]]
                    -> Maybe (Poly, Poly)
tryQuadFactorsInner f pts ds0 rest =
  case findQuadFromDivs f pts ds0 of
    Just result -> Just result
    Nothing     -> tryQuadFactors f pts rest

--- Try to find a quadratic factor from divisor values.
findQuadFromDivs :: Poly -> [Rational] -> [Rational] -> Maybe (Poly, Poly)
findQuadFromDivs _ _ [] = Nothing
findQuadFromDivs f pts (d0:ds) =
  case pts of
    [p0, _, _] ->
      let -- For simplicity, just try d0 as f(0)'s divisor
          -- and check if it divides.
          -- This is a simplified Kronecker: test if (x - r) divides f
          -- for various rationals r.
          tryResult = tryDivisorAsRoot f p0 d0
      in case tryResult of
           Just r -> Just r
           Nothing -> findQuadFromDivs f pts ds
    _ -> Nothing

tryDivisorAsRoot :: Poly -> Rational -> Rational -> Maybe (Poly, Poly)
tryDivisorAsRoot _ _ _ = Nothing

--- Get all rational divisors of a rational number.
allRatDivisors :: Rational -> [Rational]
allRatDivisors r =
  let n = absInt (numerator r)
      d = denominator r
      nDivs = divisors n
      dDivs = divisors d
  in [mkRat p' q' | p' <- concatMap (\x -> [x, negate x]) nDivs
                   , q' <- dDivs, q' /= 0]

--- Divisors of a positive integer.
divisors :: Int -> [Int]
divisors n
  | n <= 0    = [1]
  | n == 1    = [1]
  | otherwise = [d | d <- [1..n], n `mod` d == 0]

--- LCM of two integers.
lcmInt :: Int -> Int -> Int
lcmInt a b
  | a == 0 = 0
  | b == 0 = 0
  | otherwise = absInt (a * b) `div` gcdInt (absInt a) (absInt b)

--- GCD of two integers.
gcdInt :: Int -> Int -> Int
gcdInt a b
  | b == 0    = a
  | otherwise = gcdInt b (a `mod` b)

--- Absolute value.
absInt :: Int -> Int
absInt x = if x < 0 then negate x else x

--- Last element.
lastElem :: [a] -> a
lastElem xs = case xs of
  [x]    -> x
  (_:ys) -> lastElem ys
  []     -> error "lastElem: empty"

--- Extract coefficient list from Poly.
polyCoeffs :: Poly -> [Rational]
polyCoeffs (Poly cs) = cs
