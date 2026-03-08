-- | Interval arithmetic with rational endpoints for root isolation
-- and numerical evaluation.
module Surd.Internal.Interval
  ( Interval(..)
  , midpoint
  , width
  , contains
  , overlaps
  , bisect
  , refine
  , fromRational'
  -- * Interval arithmetic
  , iadd
  , isub
  , imul
  , iinv
  , idiv
  , ipow
  , isqrt
  ) where

-- | A closed interval [lo, hi] with rational endpoints.
data Interval = Interval
  { lo :: !Rational
  , hi :: !Rational
  } deriving (Eq, Ord, Show)

midpoint :: Interval -> Rational
midpoint (Interval l h) = (l + h) / 2

width :: Interval -> Rational
width (Interval l h) = h - l

contains :: Interval -> Rational -> Bool
contains (Interval l h) x = l <= x && x <= h

overlaps :: Interval -> Interval -> Bool
overlaps (Interval l1 h1) (Interval l2 h2) = l1 <= h2 && l2 <= h1

bisect :: Interval -> (Interval, Interval)
bisect iv@(Interval l h) =
  let m = midpoint iv
  in (Interval l m, Interval m h)

-- | Narrow an interval by bisection, keeping the half that satisfies
-- the given predicate on the midpoint.
refine :: (Rational -> Bool) -> Interval -> Interval
refine p iv =
  let (left, right) = bisect iv
      m = midpoint iv
  in if p m then left else right

fromRational' :: Rational -> Interval
fromRational' x = Interval x x

-- Interval arithmetic operations

iadd :: Interval -> Interval -> Interval
iadd (Interval l1 h1) (Interval l2 h2) = Interval (l1 + l2) (h1 + h2)

isub :: Interval -> Interval -> Interval
isub (Interval l1 h1) (Interval l2 h2) = Interval (l1 - h2) (h1 - l2)

imul :: Interval -> Interval -> Interval
imul (Interval l1 h1) (Interval l2 h2) =
  let ps = [l1*l2, l1*h2, h1*l2, h1*h2]
  in Interval (minimum ps) (maximum ps)

-- | Inverse of an interval that does not contain zero.
iinv :: Interval -> Interval
iinv (Interval l h)
  | l > 0 || h < 0 = Interval (1/h) (1/l)
  | otherwise       = error "iinv: interval contains zero"

idiv :: Interval -> Interval -> Interval
idiv a b = imul a (iinv b)

ipow :: Interval -> Int -> Interval
ipow _  0 = Interval 1 1
ipow iv n
  | n < 0     = iinv (ipow iv (-n))
  | odd n     = foldr1 imul (replicate n iv)
  | otherwise =
      -- Even power: result is non-negative
      let Interval l h = iv
          ps = [l^n, h^n]
      in if l <= 0 && 0 <= h
         then Interval 0 (maximum ps)
         else Interval (minimum ps) (maximum ps)

-- | Interval enclosure of the square root of a non-negative interval.
-- Uses Newton refinement.
isqrt :: Interval -> Interval
isqrt (Interval l h)
  | l < 0     = error "isqrt: negative interval"
  | h == 0    = Interval 0 0
  | otherwise = Interval (sqrtLower l) (sqrtUpper h)
  where
    -- Lower bound: largest rational whose square <= x
    sqrtLower x = converge x (x / 2 + 1)
      where
        converge _ s | s * s <= x = s
        converge prev s
          | s == prev = s - 1  -- ensure it's a lower bound
          | otherwise = converge s ((s + x / s) / 2)

    -- Upper bound: smallest rational whose square >= x
    sqrtUpper x = converge x (x / 2 + 1)
      where
        converge _ s | s * s >= x = s
        converge prev s
          | s == prev = s + 1  -- ensure it's an upper bound
          | otherwise = converge s ((s + x / s) / 2)
