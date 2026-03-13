--- Strictly positive integers (> 0).
--- Encodes the positivity constraint so callers of factorise etc.
--- cannot pass non-positive values.
module Positive
  ( Positive
  , unPositive
  , positive
  , unsafePositive
  , showPositive
  ) where

--- A strictly positive integer.
data Positive = Pos Int

--- Extract the underlying integer.
unPositive :: Positive -> Int
unPositive (Pos n) = n

--- Smart constructor: returns Nothing for non-positive input.
positive :: Int -> Maybe Positive
positive n
  | n > 0     = Just (Pos n)
  | otherwise = Nothing

--- Unsafe constructor for when positivity is known.
--- Errors at runtime for non-positive input.
unsafePositive :: Int -> Positive
unsafePositive n = if n > 0 then Pos n
                   else error "unsafePositive: non-positive value"

--- Show instance.
showPositive :: Positive -> String
showPositive (Pos n) = show n

instance Eq Positive where
  (Pos a) == (Pos b) = a == b

instance Ord Positive where
  compare (Pos a) (Pos b) = compare a b
  (Pos a) <= (Pos b) = a <= b

instance Show Positive where
  show = showPositive
