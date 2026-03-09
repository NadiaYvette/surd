-- | Transcendental extension field Q(x₁,...,xₙ).
--
-- Elements are rational functions (ratios of multivariate polynomials).
-- Equality uses cross-multiplication (no multivariate GCD required).
module Surd.Field.Transcendental
  ( RatFunc(..)
  , mkRatFunc
  , constRF
  , varRF
  , isConstRF
  , evalRF
  ) where

import Data.Ratio (numerator, denominator)
import Surd.Polynomial.Multivariate

-- | An element of Q(x₁,...,xₙ): a ratio of multivariate polynomials.
--
-- Invariant: denominator is non-zero.
-- Fractions are NOT reduced to lowest terms (no multivariate GCD).
-- Equality uses cross-multiplication.
data RatFunc k = RatFunc
  { rfNum :: !(MPoly k)
  , rfDen :: !(MPoly k)
  } deriving (Show)

-- | Smart constructor ensuring non-zero denominator.
mkRatFunc :: MPoly k -> MPoly k -> RatFunc k
mkRatFunc _ d | isZero d = error "RatFunc: zero denominator"
mkRatFunc n d = RatFunc n d

-- | Constant rational function.
constRF :: (Eq k, Num k) => k -> RatFunc k
constRF c = RatFunc (constPoly c) onePoly

-- | A transcendental variable as a rational function.
varRF :: (Eq k, Num k) => Var -> RatFunc k
varRF v = RatFunc (varPoly v) onePoly

-- | Test if a rational function is a constant (denominator is constant
-- and numerator is constant or zero).
isConstRF :: RatFunc k -> Bool
isConstRF (RatFunc n d) = numTerms n <= 1 && numTerms d == 1
    && totalDegree n == 0 && totalDegree d == 0

-- | Evaluate a rational function by substituting values for variables.
evalRF :: (Eq k, Fractional k) => (Var -> k) -> RatFunc k -> k
evalRF env (RatFunc n d) = evalPoly env n / evalPoly env d

-- Eq: cross-multiplication
-- p1/q1 == p2/q2  ⟺  p1*q2 == p2*q1
instance (Eq k, Num k) => Eq (RatFunc k) where
  RatFunc n1 d1 == RatFunc n2 d2 = mulPoly n1 d2 == mulPoly n2 d1

-- Ord: structural ordering for use in Map keys.
-- NOT a mathematically meaningful ordering.
instance (Eq k, Num k, Ord k) => Ord (RatFunc k) where
  compare (RatFunc n1 d1) (RatFunc n2 d2) =
    compare (unMPoly (mulPoly n1 d2)) (unMPoly (mulPoly n2 d1))

-- Num instance
instance (Eq k, Num k) => Num (RatFunc k) where
  -- p1/q1 + p2/q2 = (p1*q2 + p2*q1) / (q1*q2)
  RatFunc n1 d1 + RatFunc n2 d2 =
    RatFunc (addPoly (mulPoly n1 d2) (mulPoly n2 d1))
            (mulPoly d1 d2)

  -- p1/q1 * p2/q2 = (p1*p2) / (q1*q2)
  RatFunc n1 d1 * RatFunc n2 d2 =
    RatFunc (mulPoly n1 n2) (mulPoly d1 d2)

  negate (RatFunc n d) = RatFunc (negatePoly n) d

  abs    = error "RatFunc: abs not meaningful"
  signum = error "RatFunc: signum not meaningful"

  fromInteger = constRF . fromInteger

-- Fractional instance
instance (Eq k, Num k) => Fractional (RatFunc k) where
  recip (RatFunc n d)
    | isZero n  = error "RatFunc: division by zero"
    | otherwise = RatFunc d n

  fromRational r =
    -- p/q as a RatFunc: numerator polynomial / denominator polynomial
    RatFunc (constPoly (fromInteger (numerator r)))
            (constPoly (fromInteger (denominator r)))
