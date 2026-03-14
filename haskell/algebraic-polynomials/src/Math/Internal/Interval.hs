-- |
-- Module      : Math.Internal.Interval
-- Description : Interval arithmetic with rational endpoints
-- Stability   : experimental
--
-- Rigorous interval arithmetic over 'Rational' endpoints for root isolation,
-- sign determination, and numerical evaluation with guaranteed error bounds.
--
-- Includes both real intervals ('Interval') and complex intervals
-- ('ComplexInterval', rectangular representation), along with interval
-- versions of trigonometric functions and complex nth roots.
--
-- All operations produce enclosures: the true value is guaranteed to lie
-- within the returned interval. This is achieved by using exact rational
-- arithmetic (no floating-point rounding) and adding rigorous remainder
-- bounds for Taylor series.
module Math.Internal.Interval
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
  -- * Trigonometric interval functions
  , iatan
  , icos
  , isin
  , iatan2
  , piInterval
  -- * Complex nth root
  , cinthroot
  ) where

-- | A closed interval \([lo, hi]\) with rational endpoints.
--
-- __Invariant:__ @lo <= hi@.
data Interval = Interval
  { lo :: !Rational  -- ^ Lower bound of the interval.
  , hi :: !Rational  -- ^ Upper bound of the interval.
  } deriving (Eq, Ord, Show)

-- | Midpoint of the interval: \((lo + hi) / 2\).
midpoint :: Interval -> Rational
midpoint (Interval l h) = (l + h) / 2

-- | Width of the interval: \(hi - lo\).
--
-- A width of zero indicates a point interval.
width :: Interval -> Rational
width (Interval l h) = h - l

-- | Test whether a rational value lies within the interval.
contains :: Interval -> Rational -> Bool
contains (Interval l h) x = l <= x && x <= h

-- | Test whether two intervals overlap (share at least one point).
overlaps :: Interval -> Interval -> Bool
overlaps (Interval l1 h1) (Interval l2 h2) = l1 <= h2 && l2 <= h1

-- | Split an interval at its midpoint into two equal halves.
bisect :: Interval -> (Interval, Interval)
bisect iv@(Interval l h) =
  let m = midpoint iv
  in (Interval l m, Interval m h)

-- | Narrow an interval by bisection: keeps the half where the midpoint
-- satisfies the given predicate (placed in the left half).
--
-- Useful for iterative root isolation with Sturm sequences.
refine :: (Rational -> Bool) -> Interval -> Interval
refine p iv =
  let (left, right) = bisect iv
      m = midpoint iv
  in if p m then left else right

-- | Construct a point interval \([x, x]\) from a rational value.
fromRational' :: Rational -> Interval
fromRational' x = Interval x x

-- Interval arithmetic operations

-- | Interval addition: \([a, b] + [c, d] = [a+c, b+d]\).
iadd :: Interval -> Interval -> Interval
iadd (Interval l1 h1) (Interval l2 h2) = Interval (l1 + l2) (h1 + h2)

-- | Interval subtraction: \([a, b] - [c, d] = [a-d, b-c]\).
isub :: Interval -> Interval -> Interval
isub (Interval l1 h1) (Interval l2 h2) = Interval (l1 - h2) (h1 - l2)

-- | Interval multiplication: takes the min and max of all four products
-- of the endpoints.
imul :: Interval -> Interval -> Interval
imul (Interval l1 h1) (Interval l2 h2) =
  let ps = [l1*l2, l1*h2, h1*l2, h1*h2]
  in Interval (minimum ps) (maximum ps)

-- | Interval reciprocal: \(1 / [a, b] = [1/b, 1/a]\).
--
-- __Precondition:__ the interval must not contain zero.
-- Throws an error if zero is in the interval.
iinv :: Interval -> Interval
iinv (Interval l h)
  | l > 0 || h < 0 = Interval (1/h) (1/l)
  | otherwise       = error "iinv: interval contains zero"

-- | Interval division: @idiv a b = imul a (iinv b)@.
--
-- __Precondition:__ @b@ must not contain zero.
idiv :: Interval -> Interval -> Interval
idiv a b = imul a (iinv b)

-- | Interval exponentiation by a non-negative integer.
--
-- For even exponents, the result is non-negative (accounts for sign
-- changes when the interval spans zero).
-- For negative exponents, computes the inverse first.
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
--
-- Uses bisection to find rational bounds (60 iterations), avoiding
-- the coefficient blowup that Newton's method on rationals would cause.
--
-- __Precondition:__ the interval must be non-negative.
isqrt :: Interval -> Interval
isqrt (Interval l h)
  | l < 0     = error "isqrt: negative interval"
  | h == 0    = Interval 0 0
  | otherwise = Interval (nthRootLower 2 l) (nthRootUpper 2 h)

-- | Interval enclosure of the \(n\)th root.
--
-- For odd \(n\), handles negative values (the real \(n\)th root is defined).
-- For even \(n\), requires a non-negative interval.
--
-- Uses 60 iterations of bisection for rational bound computation.
--
-- __Precondition:__ for even @n@, the interval must be non-negative.
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
      -- Odd root spanning zero: [-a, b] -> [-nth_root(a), nth_root(b)]
      let negPart = nthRootUpper n (negate l)
          posPart = nthRootUpper n h
      in Interval (negate negPart) posPart
  | otherwise = Interval (nthRootLower n l) (nthRootUpper n h)

-- | Find a rational lower bound @r@ such that @r^n <= a@, via 60 steps
-- of bisection.
nthRootLower :: Int -> Rational -> Rational
nthRootLower _ 0 = 0
nthRootLower n a =
  -- Bisect in [0, max(a,1)] to find largest r with r^n <= a
  let upper = max a 1
  in bisectDown n a 0 upper 60

-- | Find a rational upper bound @r@ such that @r^n >= a@, via 60 steps
-- of bisection.
nthRootUpper :: Int -> Rational -> Rational
nthRootUpper _ 0 = 0
nthRootUpper n a =
  let upper = max a 1 + 1
  in bisectUp n a 0 upper 60

-- | Bisect to find largest @r@ in @[lo, hi]@ with @r^n <= a@.
bisectDown :: Int -> Rational -> Rational -> Rational -> Int -> Rational
bisectDown _ _ lo _ 0 = lo
bisectDown n a lo hi iters =
  let mid = (lo + hi) / 2
  in if mid ^^ n <= a
     then bisectDown n a mid hi (iters - 1)
     else bisectDown n a lo mid (iters - 1)

-- | Bisect to find smallest @r@ in @[lo, hi]@ with @r^n >= a@.
bisectUp :: Int -> Rational -> Rational -> Rational -> Int -> Rational
bisectUp _ _ _ hi 0 = hi
bisectUp n a lo hi iters =
  let mid = (lo + hi) / 2
  in if mid ^^ n >= a
     then bisectUp n a lo mid (iters - 1)
     else bisectUp n a mid hi (iters - 1)

-- | Absolute value of an interval.
--
-- If the interval spans zero, the result is @[0, max(|lo|, |hi|)]@.
iabs :: Interval -> Interval
iabs (Interval l h)
  | l >= 0    = Interval l h
  | h <= 0    = Interval (negate h) (negate l)
  | otherwise = Interval 0 (max (negate l) h)

-- | Test if the entire interval is strictly positive (\(lo > 0\)).
strictlyPositive :: Interval -> Bool
strictlyPositive (Interval l _) = l > 0

-- | Test if the entire interval is strictly negative (\(hi < 0\)).
strictlyNegative :: Interval -> Bool
strictlyNegative (Interval _ h) = h < 0

-- | Test if the interval contains zero.
containsZero :: Interval -> Bool
containsZero (Interval l h) = l <= 0 && h >= 0

-- -------------------------------------------------------------------
-- Complex intervals (rectangular: real part x imaginary part)
-- -------------------------------------------------------------------

-- | A complex interval in rectangular form: the Cartesian product of a
-- real-part interval and an imaginary-part interval.
--
-- Represents the set \(\{a + bi \mid a \in \text{ciReal}, b \in \text{ciImag}\}\).
data ComplexInterval = ComplexInterval
  { ciReal :: !Interval  -- ^ Real part interval.
  , ciImag :: !Interval  -- ^ Imaginary part interval.
  } deriving (Eq, Show)

-- | Construct a complex interval from a single rational value (imaginary
-- part is zero).
ciFromRational :: Rational -> ComplexInterval
ciFromRational r = ComplexInterval (fromRational' r) (fromRational' 0)

-- | Construct a complex interval from a real interval (imaginary part
-- is zero).
ciFromReal :: Interval -> ComplexInterval
ciFromReal r = ComplexInterval r (fromRational' 0)

-- | Complex interval addition.
ciadd :: ComplexInterval -> ComplexInterval -> ComplexInterval
ciadd (ComplexInterval r1 i1) (ComplexInterval r2 i2) =
  ComplexInterval (iadd r1 r2) (iadd i1 i2)

-- | Complex interval subtraction.
cisub :: ComplexInterval -> ComplexInterval -> ComplexInterval
cisub (ComplexInterval r1 i1) (ComplexInterval r2 i2) =
  ComplexInterval (isub r1 r2) (isub i1 i2)

-- | Complex interval negation.
cineg :: ComplexInterval -> ComplexInterval
cineg (ComplexInterval r i) =
  ComplexInterval (Interval (negate (hi r)) (negate (lo r)))
                  (Interval (negate (hi i)) (negate (lo i)))

-- | Complex interval multiplication: \((a + bi)(c + di) = (ac - bd) + (ad + bc)i\).
cimul :: ComplexInterval -> ComplexInterval -> ComplexInterval
cimul (ComplexInterval r1 i1) (ComplexInterval r2 i2) =
  ComplexInterval (isub (imul r1 r2) (imul i1 i2))
                  (iadd (imul r1 i2) (imul i1 r2))

-- | Complex interval reciprocal: \(1/(a + bi) = (a - bi)/(a^2 + b^2)\).
--
-- __Precondition:__ the interval must not contain zero (the magnitude
-- squared interval must not contain zero).
ciinv :: ComplexInterval -> ComplexInterval
ciinv (ComplexInterval r i) =
  let magSq = iadd (imul r r) (imul i i)
  in ComplexInterval (idiv r magSq) (idiv (Interval (negate (hi i)) (negate (lo i))) magSq)

-- | Complex interval exponentiation by an integer.
--
-- Uses binary exponentiation. Negative exponents compute the inverse first.
cipow :: ComplexInterval -> Int -> ComplexInterval
cipow _ 0 = ciFromRational 1
cipow z 1 = z
cipow z n
  | n < 0     = cipow (ciinv z) (negate n)
  | even n    = let half = cipow z (n `div` 2) in cimul half half
  | otherwise = cimul z (cipow z (n - 1))

-- | Squared magnitude \(|z|^2 = \text{Re}^2 + \text{Im}^2\) as a real
-- interval (avoids computing the square root).
ciMagnitudeSq :: ComplexInterval -> Interval
ciMagnitudeSq (ComplexInterval r i) = iadd (imul r r) (imul i i)

-- | Extract the real part interval.
ciRealPart :: ComplexInterval -> Interval
ciRealPart = ciReal

-- | Extract the imaginary part interval.
ciImagPart :: ComplexInterval -> Interval
ciImagPart = ciImag

-- -------------------------------------------------------------------
-- Interval atan, cos, sin, atan2, and complex nth root
-- -------------------------------------------------------------------

-- | Rational interval enclosure of \(\pi\) via Machin's formula:
--
-- \[\pi/4 = 4 \cdot \text{atan}(1/5) - \text{atan}(1/239)\]
--
-- Each atan is computed via 50-term Taylor series with rigorous remainder
-- bounds, giving approximately 70 decimal digits of precision.
piInterval :: Interval
piInterval =
  let a = iatanSmall (1 / 5)    -- atan(1/5), |1/5| < 1
      b = iatanSmall (1 / 239)  -- atan(1/239), |1/239| < 1
      four = Interval 4 4
  in imul four (isub (imul four a) b)

-- | Interval enclosure of \(\text{atan}(r)\) for \(|r| \leq 1\) via Taylor
-- series with rigorous remainder bound.
--
-- Uses 50 terms: \(\text{atan}(r) = \sum_{k=0}^{49} (-1)^k r^{2k+1}/(2k+1)\).
-- The remainder satisfies \(|\text{remainder}| \leq |r|^{101}/101\).
iatanSmall :: Rational -> Interval
iatanSmall r =
  let nTerms = 50 :: Int
      -- Compute partial sum
      partialSum = sum [ (if even k then 1 else -1) * r ^^ (2*k+1)
                         / fromIntegral (2*k+1)
                       | k <- [0 .. nTerms - 1] ]
      -- Remainder bound: |r|^(2N+1) / (2N+1)
      ar = abs r
      remBound = ar ^^ (2 * nTerms + 1) / fromIntegral (2 * nTerms + 1)
  in Interval (partialSum - remBound) (partialSum + remBound)


-- | Interval enclosure of \(\text{atan}(x)\) for an interval \(x\).
--
-- Since atan is monotone increasing, \(\text{atan}([a,b]) = [\text{atan}(a), \text{atan}(b)]\).
-- For \(|x| > 1\), uses the identity \(\text{atan}(x) = \text{sign}(x) \cdot \pi/2 - \text{atan}(1/x)\).
iatan :: Interval -> Interval
iatan (Interval l h) =
  Interval (lo (iatanPoint l)) (hi (iatanPoint h))

-- | Interval enclosure of atan for a single rational value.
iatanPoint :: Rational -> Interval
iatanPoint r
  | r == 0    = Interval 0 0
  | abs r <= 1 = iatanSmall r
  | r > 0     =
      -- atan(r) = pi/2 - atan(1/r) for r > 0
      let piHalf = idiv piInterval (Interval 2 2)
      in isub piHalf (iatanSmall (1 / r))
  | otherwise  =
      -- atan(r) = -pi/2 + atan(-1/r) for r < 0
      let piHalf = idiv piInterval (Interval 2 2)
          negPiHalf = Interval (negate (hi piHalf)) (negate (lo piHalf))
      in iadd negPiHalf (iatanSmall (1 / negate r))

-- | Interval enclosure of \(\cos(x)\) for a single rational value via
-- 30-term Taylor series with rigorous remainder bound.
icosPoint :: Rational -> Interval
icosPoint x =
  let nTerms = 30 :: Int
      partialSum = sum [ (if even k then 1 else -1) * x ^^ (2*k) / factorial (2*k)
                       | k <- [0 .. nTerms - 1] ]
      remBound = abs x ^^ (2 * nTerms) / factorial (2 * nTerms)
  in Interval (partialSum - remBound) (partialSum + remBound)

factorial :: Int -> Rational
factorial n = fromIntegral (go 1 n)
  where
    go :: Integer -> Int -> Integer
    go acc 0 = acc
    go acc k = go (acc * fromIntegral k) (k - 1)

-- | Interval enclosure of \(\cos\) over an interval.
--
-- Evaluates at both endpoints, then checks whether any integer multiple
-- of \(\pi\) falls inside the interval. If so, includes the corresponding
-- extremal value (\(\pm 1\)). The result is clamped to \([-1, 1]\).
--
-- For intervals wider than \(2\pi\), returns \([-1, 1]\).
icos :: Interval -> Interval
icos iv@(Interval l h)
  | l == h    = icosPoint l
  | otherwise =
      let cosL = icosPoint l
          cosH = icosPoint h
          -- Start with hull of endpoint evaluations
          baseLo = min (lo cosL) (lo cosH)
          baseHi = max (hi cosL) (hi cosH)
          -- Check for extrema: cos has maximum 1 at 2k*pi, minimum -1 at (2k+1)*pi
          piLo = lo piInterval
          piHi = hi piInterval
          -- k ranges: k*pi in [l, h]
          kMin, kMax :: Integer
          kMin = ceiling (l / piHi)
          kMax = floor (h / piLo)
          hasMax1 = any (\k -> even k) [kMin..kMax]
          hasMinNeg1 = any (\k -> odd k) [kMin..kMax]
          finalLo = if hasMinNeg1 then min baseLo (-1) else baseLo
          finalHi = if hasMax1 then max baseHi 1 else baseHi
          -- Clamp to [-1, 1]
          clampedLo = max (-1) finalLo
          clampedHi = min 1 finalHi
      in if width iv > hi piInterval * 2
         then Interval (-1) 1  -- interval wider than 2*pi: full range
         else Interval clampedLo clampedHi

-- | Interval enclosure of \(\sin\) over an interval.
--
-- Uses the identity \(\sin(x) = \cos(x - \pi/2)\).
isin :: Interval -> Interval
isin iv =
  let piHalf = idiv piInterval (Interval 2 2)
      shifted = isub iv piHalf
  in icos shifted

-- | Interval enclosure of \(\text{atan2}(y, x)\) for interval arguments.
--
-- Returns an interval enclosing the angle \(\theta \in (-\pi, \pi]\) of the
-- point \((x, y)\).
--
-- Handles the four quadrant cases based on the signs of \(x\) and \(y\).
-- When the \(x\) interval contains zero, returns the full range \([-\pi, \pi]\).
iatan2 :: Interval -> Interval -> Interval
iatan2 y x
  | strictlyPositive x =
      -- atan2(y,x) = atan(y/x), monotone up in y, monotone down in x.
      let thetaMin = iatanPoint (lo y / hi x)
          thetaMax = iatanPoint (hi y / lo x)
      in Interval (lo thetaMin) (hi thetaMax)
  | strictlyNegative x && lo y >= 0 =
      -- All y >= 0, all x < 0: theta = atan(y/x) + pi
      let thetaMin = iadd (iatanPoint (hi y / lo x)) piInterval
          thetaMax = iadd (iatanPoint (lo y / hi x)) piInterval
      in Interval (lo thetaMin) (hi thetaMax)
  | strictlyNegative x && hi y <= 0 =
      -- All y <= 0, all x < 0: theta = atan(y/x) - pi
      let negPi = Interval (negate (hi piInterval)) (negate (lo piInterval))
          thetaMin = iadd (iatanPoint (hi y / lo x)) negPi
          thetaMax = iadd (iatanPoint (lo y / hi x)) negPi
      in Interval (lo thetaMin) (hi thetaMax)
  | strictlyNegative x =
      -- x < 0, y spans zero: discontinuity at y=0.
      -- Cover both branches.
      let negPi = Interval (negate (hi piInterval)) (negate (lo piInterval))
          thetaLow = iadd (iatanPoint (lo y / lo x)) negPi
          thetaHigh = iadd (iatanPoint (hi y / hi x)) piInterval
      in Interval (lo thetaLow) (hi thetaHigh)
  | otherwise =
      -- x interval contains zero: the full range of atan2 is possible
      Interval (negate (hi piInterval)) (hi piInterval)

-- | Complex interval nth root via polar decomposition.
--
-- \[z^{1/n} = |z|^{1/n} \cdot (\cos(\theta/n) + i \cdot \sin(\theta/n))\]
--
-- where \(\theta = \text{atan2}(\text{Im}\,z, \text{Re}\,z)\) is the
-- principal argument. Returns the principal \(n\)th root (argument in
-- \((-\pi/n, \pi/n]\)).
--
-- __Precondition:__ @n > 0@.
cinthroot :: Int -> ComplexInterval -> ComplexInterval
cinthroot n z
  | n <= 0    = error "cinthroot: non-positive root index"
  | n == 1    = z
  | otherwise =
      let re = ciReal z
          im = ciImag z
          -- |z|^2 = re^2 + im^2
          magSq = iadd (imul re re) (imul im im)
          -- |z| = sqrt(|z|^2)
          mag = isqrt magSq
          -- |z|^(1/n)
          magRoot = inth n mag
          -- theta = atan2(im, re)
          theta = iatan2 im re
          -- theta/n
          nIv = Interval (fromIntegral n) (fromIntegral n)
          thetaN = idiv theta nIv
          -- cos(theta/n) and sin(theta/n)
          cosTheta = icos thetaN
          sinTheta = isin thetaN
      in ComplexInterval (imul magRoot cosTheta) (imul magRoot sinTheta)
