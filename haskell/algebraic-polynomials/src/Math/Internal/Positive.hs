-- |
-- Module      : Math.Internal.Positive
-- Description : Strictly positive integers with type-level safety
-- Stability   : experimental
--
-- A newtype wrapper for strictly positive integers (\(> 0\)), encoding the
-- positivity invariant in the type system. This eliminates runtime errors
-- for functions that require positive input (e.g., 'Math.Internal.PrimeFactors.factorise',
-- 'Math.Polynomial.Cyclotomic.cyclotomic').
--
-- Construct values via 'positive' (checked) or numeric literals (via the
-- 'Num' instance, which errors at runtime for non-positive values -- but
-- literals are known at write time).
module Math.Internal.Positive
  ( Positive
  , unPositive
  , positive
  , unsafePositive
  ) where

import Numeric.Natural (Natural)

-- | A strictly positive integer (\(> 0\)).
--
-- The internal representation is 'Natural', which is guaranteed to be
-- non-negative; the 'Positive' wrapper further guarantees it is nonzero.
--
-- Use 'positive' for safe construction, or 'unsafePositive' when
-- positivity is guaranteed by context.
newtype Positive = Positive { unPositive :: Natural }
  deriving (Eq, Ord, Show)

-- | Safe constructor: returns @'Just' p@ for positive input, @'Nothing'@
-- for zero.
--
-- >>> positive 5
-- Just (Positive {unPositive = 5})
-- >>> positive 0
-- Nothing
positive :: Natural -> Maybe Positive
positive 0 = Nothing
positive n = Just (Positive n)

-- | Unsafe constructor for cases where positivity is guaranteed by
-- context (e.g., the result of @a + 1@ where @a@ is a 'Natural').
--
-- Throws an error if given zero.
unsafePositive :: Natural -> Positive
unsafePositive 0 = error "unsafePositive: zero"
unsafePositive n = Positive n

-- | 'Num' instance for 'Positive'.
--
-- 'fromInteger' errors on non-positive values, making it safe for
-- numeric literals (which are known at write time).
--
-- Addition and multiplication preserve positivity.
-- 'negate' always errors since the result would be non-positive.
instance Num Positive where
  Positive a + Positive b = Positive (a + b)
  Positive a * Positive b = Positive (a * b)
  abs = id
  signum _ = Positive 1
  negate _ = error "Positive.negate: result would be non-positive"
  fromInteger n
    | n > 0     = Positive (fromInteger n)
    | otherwise = error $ "Positive.fromInteger: non-positive value " ++ show n
