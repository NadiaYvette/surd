-- |
-- Module      : Math.Field.Transcendental
-- Description : Transcendental extension fields Q(x1, ..., xn) via rational functions
-- Stability   : experimental
--
-- Elements of transcendental extension fields \(\mathbb{Q}(x_1, \ldots, x_n)\)
-- are represented as rational functions (ratios of multivariate polynomials).
--
-- When the coefficient type is 'Rational', fractions can be reduced to
-- lowest terms using 'reduceFrac' (which computes the multivariate GCD
-- of numerator and denominator).
--
-- Has 'Num', 'Fractional', 'Eq', and 'Ord' instances. The 'Ord' instance
-- provides a structural ordering for use in 'Map' keys and is /not/
-- mathematically meaningful.
module Math.Field.Transcendental
  ( RatFunc(..)
  , mkRatFunc
  , reduceFrac
  , constRF
  , varRF
  , isConstRF
  , evalRF
  ) where

import Data.Ratio (numerator, denominator)
import Math.Polynomial.Multivariate

-- | An element of \(\mathbb{Q}(x_1, \ldots, x_n)\): a ratio of
-- multivariate polynomials.
--
-- __Invariant:__ the denominator is non-zero.
--
-- For 'Rational' coefficients, use 'reduceFrac' to reduce to lowest terms.
data RatFunc k = RatFunc
  { rfNum :: !(MPoly k)  -- ^ Numerator polynomial.
  , rfDen :: !(MPoly k)  -- ^ Denominator polynomial (non-zero).
  } deriving (Show)

-- | Smart constructor ensuring a non-zero denominator.
--
-- Throws an error if the denominator is zero.
mkRatFunc :: MPoly k -> MPoly k -> RatFunc k
mkRatFunc _ d | isZero d = error "RatFunc: zero denominator"
mkRatFunc n d = RatFunc n d

-- | Reduce a rational function over 'Rational' to lowest terms by
-- dividing out the GCD of numerator and denominator.
--
-- Uses 'gcdMPoly' for the multivariate polynomial GCD.
reduceFrac :: RatFunc Rational -> RatFunc Rational
reduceFrac (RatFunc n d)
  | isZero n  = RatFunc zeroPoly (constPoly 1)
  | otherwise =
    let g = gcdMPoly n d
    in if isZero g || g == constPoly 1
       then RatFunc n d
       else RatFunc (exactDivMPoly n g) (exactDivMPoly d g)

-- | Construct a constant rational function from a scalar value.
constRF :: (Eq k, Num k) => k -> RatFunc k
constRF c = RatFunc (constPoly c) onePoly

-- | Construct a rational function representing a single transcendental
-- variable \(x_i\).
varRF :: (Eq k, Num k) => Var -> RatFunc k
varRF v = RatFunc (varPoly v) onePoly

-- | Test if a rational function is a constant (both numerator and
-- denominator are degree-0 polynomials with at most one term each).
isConstRF :: RatFunc k -> Bool
isConstRF (RatFunc n d) = numTerms n <= 1 && numTerms d == 1
    && totalDegree n == 0 && totalDegree d == 0

-- | Evaluate a rational function by substituting values for all variables.
--
-- The environment function maps each 'Var' to a value in @k@.
--
-- __Precondition:__ the denominator must not evaluate to zero.
evalRF :: (Eq k, Fractional k) => (Var -> k) -> RatFunc k -> k
evalRF env (RatFunc n d) = evalPoly env n / evalPoly env d

-- | Equality via cross-multiplication:
-- \(p_1/q_1 = p_2/q_2 \iff p_1 q_2 = p_2 q_1\).
instance (Eq k, Num k) => Eq (RatFunc k) where
  RatFunc n1 d1 == RatFunc n2 d2 = mulPoly n1 d2 == mulPoly n2 d1

-- | Structural ordering for use in 'Data.Map.Map' keys.
--
-- __Warning:__ this is /not/ a mathematically meaningful ordering.
instance (Eq k, Num k, Ord k) => Ord (RatFunc k) where
  compare (RatFunc n1 d1) (RatFunc n2 d2) =
    compare (unMPoly (mulPoly n1 d2)) (unMPoly (mulPoly n2 d1))

-- | Arithmetic on rational functions via 'Num'.
--
-- 'abs' and 'signum' are not meaningful and will throw errors.
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

-- | 'Fractional' instance: 'recip' swaps numerator and denominator.
instance (Eq k, Num k) => Fractional (RatFunc k) where
  recip (RatFunc n d)
    | isZero n  = error "RatFunc: division by zero"
    | otherwise = RatFunc d n

  fromRational r =
    -- p/q as a RatFunc: numerator polynomial / denominator polynomial
    RatFunc (constPoly (fromInteger (numerator r)))
            (constPoly (fromInteger (denominator r)))
