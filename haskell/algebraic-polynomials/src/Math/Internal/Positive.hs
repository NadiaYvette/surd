-- | A type for strictly positive integers (> 0).
--
-- This eliminates runtime errors for functions that require positive input
-- (factorise, cyclotomic, euler's totient, etc.) by encoding the constraint
-- in the type system.
module Math.Internal.Positive
  ( Positive
  , unPositive
  , positive
  , unsafePositive
  ) where

import Numeric.Natural (Natural)

-- | A strictly positive integer (> 0).
--
-- Construct via 'positive' (checked) or numeric literals (via 'Num' instance,
-- which errors at runtime for non-positive values — but literals are known
-- at write time).
newtype Positive = Positive { unPositive :: Natural }
  deriving (Eq, Ord, Show)

-- | Smart constructor: returns 'Nothing' for zero.
positive :: Natural -> Maybe Positive
positive 0 = Nothing
positive n = Just (Positive n)

-- | Unsafe constructor for cases where positivity is guaranteed by context.
unsafePositive :: Natural -> Positive
unsafePositive 0 = error "unsafePositive: zero"
unsafePositive n = Positive n

-- | @fromInteger@ errors on non-positive values. Safe for numeric literals.
instance Num Positive where
  Positive a + Positive b = Positive (a + b)
  Positive a * Positive b = Positive (a * b)
  abs = id
  signum _ = Positive 1
  negate _ = error "Positive.negate: result would be non-positive"
  fromInteger n
    | n > 0     = Positive (fromInteger n)
    | otherwise = error $ "Positive.fromInteger: non-positive value " ++ show n
