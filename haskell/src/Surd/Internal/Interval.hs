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
  , inth
  , iabs
  , strictlyPositive
  , strictlyNegative
  , containsZero
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

-- | Interval enclosure of the nth root of an interval.
--
-- For odd n, handles negative values. For even n, requires non-negative.
-- Uses bisection on rational intervals to find bounds [lo, hi] such that
-- lo^n ≤ x ≤ hi^n.
inth :: Int -> Interval -> Interval
inth n (Interval l h)
  | n <= 0    = error "inth: non-positive root index"
  | n == 1    = Interval l h
  | n == 2    = isqrt (Interval l h)
  | even n && l < 0 = error "inth: even root of negative interval"
  | h == 0 && l == 0 = Interval 0 0
  | odd n && h < 0  =
      -- Odd root of entirely negative interval: negate, root, negate
      let Interval l' h' = inth n (Interval (negate h) (negate l))
      in Interval (negate h') (negate l')
  | odd n && l < 0  =
      -- Odd root spanning zero: [-a, b] → [-ⁿ√a, ⁿ√b]
      let negPart = nthRootUpper n (negate l)
          posPart = nthRootUpper n h
      in Interval (negate negPart) posPart
  | otherwise = Interval (nthRootLower n l) (nthRootUpper n h)

-- | Find a rational lower bound r such that r^n ≤ a, via bisection.
nthRootLower :: Int -> Rational -> Rational
nthRootLower _ 0 = 0
nthRootLower n a =
  -- Bisect in [0, max(a,1)] to find largest r with r^n ≤ a
  let upper = max a 1
  in bisectDown n a 0 upper 60

-- | Find a rational upper bound r such that r^n ≥ a, via bisection.
nthRootUpper :: Int -> Rational -> Rational
nthRootUpper _ 0 = 0
nthRootUpper n a =
  let upper = max a 1 + 1
  in bisectUp n a 0 upper 60

-- | Bisect to find largest r in [lo, hi] with r^n ≤ a.
bisectDown :: Int -> Rational -> Rational -> Rational -> Int -> Rational
bisectDown _ _ lo _ 0 = lo
bisectDown n a lo hi iters =
  let mid = (lo + hi) / 2
  in if mid ^^ n <= a
     then bisectDown n a mid hi (iters - 1)
     else bisectDown n a lo mid (iters - 1)

-- | Bisect to find smallest r in [lo, hi] with r^n ≥ a.
bisectUp :: Int -> Rational -> Rational -> Rational -> Int -> Rational
bisectUp _ _ _ hi 0 = hi
bisectUp n a lo hi iters =
  let mid = (lo + hi) / 2
  in if mid ^^ n >= a
     then bisectUp n a lo mid (iters - 1)
     else bisectUp n a mid hi (iters - 1)

-- | Absolute value of an interval.
iabs :: Interval -> Interval
iabs (Interval l h)
  | l >= 0    = Interval l h
  | h <= 0    = Interval (negate h) (negate l)
  | otherwise = Interval 0 (max (negate l) h)

-- | Test if an interval is strictly positive.
strictlyPositive :: Interval -> Bool
strictlyPositive (Interval l _) = l > 0

-- | Test if an interval is strictly negative.
strictlyNegative :: Interval -> Bool
strictlyNegative (Interval _ h) = h < 0

-- | Test if an interval contains zero.
containsZero :: Interval -> Bool
containsZero (Interval l h) = l <= 0 && h >= 0
