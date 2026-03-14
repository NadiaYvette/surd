-- |
-- Module      : Math.Polynomial.RootBound
-- Description : Root bounding, approximate root finding, and factor selection
-- Stability   : experimental
--
-- Utilities for bounding polynomial roots, finding approximate real roots
-- via bisection, and selecting the factor whose root is closest to a
-- target value. These are used during algebraic number computations to
-- pick the correct factor after polynomial factorisation.
module Math.Polynomial.RootBound
  ( rootBound
  , approxRoots
  , bisectRoot
  , pickClosest
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Ord (comparing)
import Math.Polynomial.Univariate

-- | Cauchy's root bound: all roots \(r\) of \(p(x) = c_n x^n + \cdots + c_0\)
-- satisfy \(|r| \leq 1 + \max_i |c_i / c_n|\).
--
-- Returns @0@ for the zero polynomial.
rootBound :: Poly Rational -> Rational
rootBound (Poly []) = 0
rootBound (Poly cs) =
  let lc = NE.last (NE.fromList cs)
      ratios = map (\c -> abs (c / lc)) (init cs)
  in 1 + maximum (0 : ratios)

-- | Find approximate real roots of a polynomial by sign-change scanning
-- and bisection.
--
-- Scans the interval \([-B, B]\) (where \(B\) is the Cauchy bound) at
-- increments of 0.1, identifies sign changes, and refines each by
-- 50 iterations of bisection.
--
-- This is a simple heuristic root finder, sufficient for selecting the
-- correct factor in 'pickClosest'. It is not guaranteed to find all roots.
approxRoots :: Poly Rational -> [Rational]
approxRoots p
  | degree p <= 0 = []
  | degree p == 1 =
      case unPoly p of
        [a, b] -> [-a / b]
        _      -> []
  | otherwise =
      -- Scan a range and find sign changes
      let bound = rootBound p
          pts = [fromIntegral i / 10 | i <- [floor (-bound * 10) :: Integer .. ceiling (bound * 10)]]
          signs = [(x, signum (evalPoly p x)) | x <- pts]
          changes = [(x1, x2) | ((x1, s1), (x2, s2)) <- zip signs (drop 1 signs), s1 /= s2, s1 /= 0, s2 /= 0]
          roots = [bisectRoot p lo hi 50 | (lo, hi) <- changes]
      in if null roots then [0] else roots  -- fallback

-- | Bisection root finding: narrow a bracket \([lo, hi]\) containing a
-- root by repeatedly halving the interval.
--
-- The @n@ parameter controls the number of bisection steps (each step
-- halves the interval width).
--
-- __Precondition:__ @p(lo)@ and @p(hi)@ have opposite signs (a root
-- exists in the interval by the intermediate value theorem).
bisectRoot :: Poly Rational
           -> Rational    -- ^ Lower bound of bracket
           -> Rational    -- ^ Upper bound of bracket
           -> Int         -- ^ Number of bisection iterations
           -> Rational    -- ^ Approximate root
bisectRoot _ lo _ 0 = lo
bisectRoot p lo hi n =
  let mid = (lo + hi) / 2
      fmid = evalPoly p mid
      flo = evalPoly p lo
  in if fmid == 0 then mid
     else if signum fmid == signum flo
          then bisectRoot p mid hi (n - 1)
          else bisectRoot p lo mid (n - 1)

-- | Pick the polynomial factor whose approximate real root is closest
-- to a given 'Double' target value.
--
-- This is used after factoring a minimal polynomial to select the
-- irreducible factor corresponding to a specific algebraic number
-- (identified by its numerical approximation).
--
-- __Precondition:__ the input list is non-empty ('NonEmpty').
pickClosest :: Double -> NonEmpty (Poly Rational) -> Poly Rational
pickClosest target factors =
  let scored = fmap (\f -> (f, minimum $ map (\r -> abs (fromRational r - target)) (approxRoots f))) factors
  in fst $ NE.head $ NE.sortBy (comparing snd) scored
