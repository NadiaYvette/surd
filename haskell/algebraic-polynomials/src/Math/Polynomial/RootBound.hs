-- | Root bounding, approximate root finding, and factor selection
-- for rational polynomials.
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

-- | Cauchy's root bound: all roots have |r| <= 1 + max|c_i/c_n|
rootBound :: Poly Rational -> Rational
rootBound (Poly []) = 0
rootBound (Poly cs) =
  let lc = NE.last (NE.fromList cs)
      ratios = map (\c -> abs (c / lc)) (init cs)
  in 1 + maximum (0 : ratios)

-- | Find approximate real roots of a polynomial by scanning.
-- Simple bisection-based root finder, sufficient for picking the right factor.
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

-- | Bisection root finding.
bisectRoot :: Poly Rational -> Rational -> Rational -> Int -> Rational
bisectRoot _ lo _ 0 = lo
bisectRoot p lo hi n =
  let mid = (lo + hi) / 2
      fmid = evalPoly p mid
      flo = evalPoly p lo
  in if fmid == 0 then mid
     else if signum fmid == signum flo
          then bisectRoot p mid hi (n - 1)
          else bisectRoot p lo mid (n - 1)

-- | Pick the factor whose numerical root is closest to the target value.
pickClosest :: Double -> NonEmpty (Poly Rational) -> Poly Rational
pickClosest target factors =
  let scored = fmap (\f -> (f, minimum $ map (\r -> abs (fromRational r - target)) (approxRoots f))) factors
  in fst $ NE.head $ NE.sortBy (comparing snd) scored
