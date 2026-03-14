-- |
-- Module      : Math.Polynomial.Factoring
-- Description : Polynomial factoring over the rationals
-- Stability   : experimental
--
-- Factoring of univariate polynomials over \(\mathbb{Q}\) using the
-- rational root theorem and Kronecker's method (for quadratic factors).
-- Sufficient for the radical denesting and trig evaluation use cases,
-- where the polynomials to be factored are typically cyclotomic or
-- low-degree minimal polynomials.
module Math.Polynomial.Factoring
  ( factor
  , rationalRoots
  , isIrreducible
  , factorSquareFree
  ) where

import Data.Ratio (numerator, denominator)
import Data.Maybe (mapMaybe)
import Math.Polynomial.Univariate
import Math.Internal.Positive (Positive)
import Math.Internal.PrimeFactors (factorise)

-- | Factor a polynomial over \(\mathbb{Q}\) into irreducible factors.
--
-- Returns a list of @(irreducible factor, multiplicity)@ pairs.
-- The factors are monic; any leading rational coefficient is separated out.
--
-- Uses 'squareFree' (Yun's algorithm) to separate multiplicities, then
-- 'factorSquareFree' on each square-free component.
--
-- >>> factor (mkPoly [1, 2, 1] :: Poly Rational)
-- [(Poly [1 % 1,1 % 1],2)]
factor :: Poly Rational -> [(Poly Rational, Int)]
factor p
  | degree p <= 0 = []
  | otherwise =
      let sfFactors = squareFree p
      in concatMap (\(f, m) -> map (\g -> (g, m)) (factorSquareFree f)) sfFactors

-- | Factor a square-free polynomial over \(\mathbb{Q}\) into irreducible factors.
--
-- Strategy:
--
-- 1. Extract linear factors via the rational root theorem.
-- 2. Attempt to find quadratic factors via Kronecker's method
--    (evaluation at three points and interpolation).
-- 3. If no factors are found, the polynomial is assumed irreducible.
--
-- Returns a list of monic irreducible factors.
factorSquareFree :: Poly Rational -> [Poly Rational]
factorSquareFree p
  | degree p <= 0 = []
  | degree p == 1 = [monicPoly p]
  | otherwise = go (monicPoly p)
  where
    go f
      | degree f <= 1 = [f]
      | otherwise =
          case findLinearFactor f of
            Just (root, quotient) ->
              mkPoly [-root, 1] : go quotient
            Nothing ->
              case findQuadraticFactor f of
                Just (fac, quotient) ->
                  fac : go quotient
                Nothing ->
                  -- Cannot factor further with current methods;
                  -- assume irreducible
                  [f]

-- | Find all rational roots of a polynomial using the rational root theorem.
--
-- If \(p(x) = a_n x^n + \cdots + a_0\) has integer coefficients (after
-- clearing denominators), then any rational root \(p/q\) (in lowest terms)
-- satisfies: \(p \mid a_0\) and \(q \mid a_n\).
--
-- >>> rationalRoots (mkPoly [-6, 11, -6, 1] :: Poly Rational)
-- [1 % 1,2 % 1,3 % 1]
rationalRoots :: Poly Rational -> [Rational]
rationalRoots p
  | degree p <= 0 = []
  | otherwise =
      let cs = unPoly p
          -- Clear denominators to get integer coefficients
          lcmDenom = foldl lcm 1 (map denominator cs)
          intCoeffs = map (\c -> numerator (c * fromInteger lcmDenom)) cs
          a0 = case intCoeffs of { (x:_) -> x; [] -> 0 }
          an = last intCoeffs
          -- Candidate numerators: divisors of a0
          -- Candidate denominators: divisors of an
          numDivs = if a0 == 0 then [0] else divisors (abs a0)
          denDivs = divisors (abs an)
          candidates = [fromInteger p' / fromInteger q'
                       | p' <- concatMap (\d -> [d, -d]) numDivs
                       , q' <- denDivs
                       , q' /= 0
                       ]
      in filter (\r -> evalPoly p r == 0) candidates

-- | Test whether a polynomial is irreducible over \(\mathbb{Q}\).
--
-- A polynomial is irreducible if it has degree \(\geq 1\) and cannot be
-- written as a product of two polynomials of lower degree over \(\mathbb{Q}\).
isIrreducible :: Poly Rational -> Bool
isIrreducible p
  | degree p <= 1 = degree p == 1
  | otherwise     = length (factorSquareFree p) == 1

-- Internal helpers

findLinearFactor :: Poly Rational -> Maybe (Rational, Poly Rational)
findLinearFactor p =
  case rationalRoots p of
    []    -> Nothing
    (r:_) -> Just (r, fst $ divModPoly p (mkPoly [-r, 1]))

-- | Try to find a quadratic factor using Kronecker's method.
--
-- Evaluates the polynomial at 0, 1, and -1 to obtain values, then
-- enumerates divisor triples and reconstructs candidate quadratic
-- factors via interpolation.
findQuadraticFactor :: Poly Rational -> Maybe (Poly Rational, Poly Rational)
findQuadraticFactor p
  | degree p < 4 = Nothing  -- degree 2 or 3 without linear factors is irreducible over Q...
                             -- well, degree 3 without linear factors is irreducible, degree 2 also
  | otherwise =
      -- Evaluate at 0, 1, -1 and try to find a quadratic factor
      -- whose values divide p's values at those points
      let v0 = evalPoly p 0
          v1 = evalPoly p 1
          vm1 = evalPoly p (-1)
          d0 = if v0 == 0 then [0] else rationalDivisors v0
          d1 = if v1 == 0 then [0] else rationalDivisors v1
          dm1 = if vm1 == 0 then [0] else rationalDivisors vm1
          -- For each triple (f(0), f(1), f(-1)), reconstruct the quadratic
          -- a + bx + cx^2 via interpolation:
          -- f(0) = a, f(1) = a+b+c, f(-1) = a-b+c
          -- => c = (f(1) + f(-1))/2 - a, b = (f(1) - f(-1))/2
          candidates = [ mkPoly [a, b, c]
                       | a <- d0
                       , f1 <- d1
                       , fm1 <- dm1
                       , let b = (f1 - fm1) / 2
                       , let c = (f1 + fm1) / 2 - a
                       , c /= 0
                       ]
      in case mapMaybe (tryDivide p) candidates of
           []          -> Nothing
           ((f,q):_)   -> Just (monicPoly f, q)

tryDivide :: Poly Rational -> Poly Rational -> Maybe (Poly Rational, Poly Rational)
tryDivide p f
  | degree f < 1 || degree f > degree p = Nothing
  | otherwise =
      let (q, r) = divModPoly p f
      in if unPoly r == [] then Just (f, q) else Nothing

-- | All positive divisors of a positive integer.
divisors :: Integer -> [Integer]
divisors 0 = [0]
divisors n =
  let fs = factorise (fromInteger n :: Positive)
  in go fs
  where
    go [] = [1]
    go ((p, e) : rest) =
      let ds = go rest
      in [p^k * d | k <- [0..e], d <- ds]

-- | Rational divisors: all rational numbers whose numerator divides the
-- given value's numerator and whose denominator divides its denominator,
-- with both signs.
rationalDivisors :: Rational -> [Rational]
rationalDivisors r =
  let n = abs (numerator r)
      d = abs (denominator r)
      nDivs = divisors n
      dDivs = divisors d
  in [fromInteger a / fromInteger b | a <- concatMap (\x -> [x, -x]) nDivs, b <- dDivs, b /= 0]
