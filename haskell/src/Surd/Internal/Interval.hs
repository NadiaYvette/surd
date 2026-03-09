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
  -- * Complex intervals
  , ComplexInterval(..)
  , ciFromRational
  , ciFromReal
  , ciadd
  , cisub
  , cimul
  , ciinv
  , cineg
  , cipow
  , ciMagnitudeSq
  , ciRealPart
  , ciImagPart
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
-- Uses bisection for guaranteed bounds (avoids rational blowup from Newton).
isqrt :: Interval -> Interval
isqrt (Interval l h)
  | l < 0     = error "isqrt: negative interval"
  | h == 0    = Interval 0 0
  | otherwise = Interval (nthRootLower 2 l) (nthRootUpper 2 h)

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

-- -------------------------------------------------------------------
-- Complex intervals (rectangular: real part × imaginary part)
-- -------------------------------------------------------------------

-- | A complex interval [re_lo, re_hi] + i·[im_lo, im_hi].
data ComplexInterval = ComplexInterval
  { ciReal :: !Interval
  , ciImag :: !Interval
  } deriving (Eq, Show)

ciFromRational :: Rational -> ComplexInterval
ciFromRational r = ComplexInterval (fromRational' r) (fromRational' 0)

ciFromReal :: Interval -> ComplexInterval
ciFromReal r = ComplexInterval r (fromRational' 0)

ciadd :: ComplexInterval -> ComplexInterval -> ComplexInterval
ciadd (ComplexInterval r1 i1) (ComplexInterval r2 i2) =
  ComplexInterval (iadd r1 r2) (iadd i1 i2)

cisub :: ComplexInterval -> ComplexInterval -> ComplexInterval
cisub (ComplexInterval r1 i1) (ComplexInterval r2 i2) =
  ComplexInterval (isub r1 r2) (isub i1 i2)

cineg :: ComplexInterval -> ComplexInterval
cineg (ComplexInterval r i) =
  ComplexInterval (Interval (negate (hi r)) (negate (lo r)))
                  (Interval (negate (hi i)) (negate (lo i)))

-- | (a + bi)(c + di) = (ac - bd) + (ad + bc)i
cimul :: ComplexInterval -> ComplexInterval -> ComplexInterval
cimul (ComplexInterval r1 i1) (ComplexInterval r2 i2) =
  ComplexInterval (isub (imul r1 r2) (imul i1 i2))
                  (iadd (imul r1 i2) (imul i1 r2))

-- | 1/(a + bi) = (a - bi)/(a² + b²)
ciinv :: ComplexInterval -> ComplexInterval
ciinv (ComplexInterval r i) =
  let magSq = iadd (imul r r) (imul i i)
  in ComplexInterval (idiv r magSq) (idiv (Interval (negate (hi i)) (negate (lo i))) magSq)

cipow :: ComplexInterval -> Int -> ComplexInterval
cipow _ 0 = ciFromRational 1
cipow z 1 = z
cipow z n
  | n < 0     = cipow (ciinv z) (negate n)
  | even n    = let half = cipow z (n `div` 2) in cimul half half
  | otherwise = cimul z (cipow z (n - 1))

-- | |z|² = re² + im² as an interval (avoids square root).
ciMagnitudeSq :: ComplexInterval -> Interval
ciMagnitudeSq (ComplexInterval r i) = iadd (imul r r) (imul i i)

ciRealPart :: ComplexInterval -> Interval
ciRealPart = ciReal

ciImagPart :: ComplexInterval -> Interval
ciImagPart = ciImag
