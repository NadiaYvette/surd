-- | Interval arithmetic with rational endpoints for root isolation
-- and numerical evaluation.
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

-- -------------------------------------------------------------------
-- Interval atan, cos, sin, atan2, and complex nth root
-- -------------------------------------------------------------------

-- | Rational interval enclosure of π via Machin's formula:
--   π/4 = 4·atan(1/5) - atan(1/239)
-- Each atan is computed via Taylor series with rigorous remainder bounds.
piInterval :: Interval
piInterval =
  let a = iatanSmall (1 / 5)    -- atan(1/5), |1/5| < 1
      b = iatanSmall (1 / 239)  -- atan(1/239), |1/239| < 1
      four = Interval 4 4
  in imul four (isub (imul four a) b)

-- | atan(r) for |r| ≤ 1 via Taylor series with remainder bound.
-- atan(r) = r - r³/3 + r⁵/5 - ... + remainder
-- |remainder| after N terms ≤ |r|^(2N+1) / (2N+1)
-- We use 50 terms for plenty of precision.
iatanSmall :: Rational -> Interval
iatanSmall r =
  let nTerms = 50 :: Int
      -- Compute partial sum: Σ_{k=0}^{N-1} (-1)^k · r^(2k+1) / (2k+1)
      partialSum = sum [ (if even k then 1 else -1) * r ^^ (2*k+1)
                         / fromIntegral (2*k+1)
                       | k <- [0 .. nTerms - 1] ]
      -- Remainder bound: |r|^(2N+1) / (2N+1)
      ar = abs r
      remBound = ar ^^ (2 * nTerms + 1) / fromIntegral (2 * nTerms + 1)
  in Interval (partialSum - remBound) (partialSum + remBound)


-- | Interval enclosure of atan(x) for an interval x.
-- Uses the identity: for |x| > 1, atan(x) = sign(x)·π/2 - atan(1/x).
-- atan is monotone increasing, so atan([a,b]) = [atan(a), atan(b)].
iatan :: Interval -> Interval
iatan (Interval l h) =
  Interval (lo (iatanPoint l)) (hi (iatanPoint h))

-- | atan of a single rational value, returned as an interval.
iatanPoint :: Rational -> Interval
iatanPoint r
  | r == 0    = Interval 0 0
  | abs r <= 1 = iatanSmall r
  | r > 0     =
      -- atan(r) = π/2 - atan(1/r) for r > 0
      let piHalf = idiv piInterval (Interval 2 2)
      in isub piHalf (iatanSmall (1 / r))
  | otherwise  =
      -- atan(r) = -π/2 - atan(1/r) for r < 0
      -- atan(r) = -(π/2 - atan(1/(-r))) = -π/2 + atan(-1/r)
      let piHalf = idiv piInterval (Interval 2 2)
          negPiHalf = Interval (negate (hi piHalf)) (negate (lo piHalf))
      in iadd negPiHalf (iatanSmall (1 / negate r))

-- | Interval enclosure of cos(x) for a single rational value via Taylor series.
-- cos(x) = Σ_{k=0}^{N-1} (-1)^k · x^(2k) / (2k)!
-- |remainder| ≤ |x|^(2N) / (2N)!
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

-- | Interval enclosure of cos over an interval.
--
-- Strategy: cos is periodic with period 2π and has extrema at kπ.
-- We evaluate at both endpoints, then check whether any integer multiple
-- of π falls inside the interval; if so, include the corresponding
-- extremal value (±1).
icos :: Interval -> Interval
icos iv@(Interval l h)
  | l == h    = icosPoint l
  | otherwise =
      let cosL = icosPoint l
          cosH = icosPoint h
          -- Start with hull of endpoint evaluations
          baseLo = min (lo cosL) (lo cosH)
          baseHi = max (hi cosL) (hi cosH)
          -- Check for extrema: cos has maximum 1 at 2kπ, minimum -1 at (2k+1)π
          -- We need to find if any kπ ∈ [l, h].
          -- Use rational bounds on π.
          piLo = lo piInterval
          piHi = hi piInterval
          -- k ranges: we need kπ ∈ [l, h]
          -- k ≥ l/π (use l/piHi for lower bound on k) and k ≤ h/π (use h/piLo)
          kMin, kMax :: Integer
          kMin = ceiling (l / piHi)
          kMax = floor (h / piLo)
          -- For each integer k in [kMin..kMax], kπ might be in [l,h].
          -- cos(kπ) = (-1)^k, so include that value.
          hasMax1 = any (\k -> even k) [kMin..kMax]
          hasMinNeg1 = any (\k -> odd k) [kMin..kMax]
          finalLo = if hasMinNeg1 then min baseLo (-1) else baseLo
          finalHi = if hasMax1 then max baseHi 1 else baseHi
          -- Clamp to [-1, 1]
          clampedLo = max (-1) finalLo
          clampedHi = min 1 finalHi
      in if width iv > hi piInterval * 2
         then Interval (-1) 1  -- interval wider than 2π: full range
         else Interval clampedLo clampedHi

-- | Interval enclosure of sin over an interval.
--
-- sin(x) = cos(x - π/2), so we shift and delegate to icos.
isin :: Interval -> Interval
isin iv =
  let piHalf = idiv piInterval (Interval 2 2)
      shifted = isub iv piHalf
  in icos shifted

-- | Interval enclosure of atan2(y, x) for interval arguments.
--
-- atan2(y,x) gives the angle θ ∈ (-π, π] of the point (x,y).
-- For x strictly positive: atan2(y,x) = atan(y/x), monotone increasing in y,
--   decreasing in x.
-- For x strictly negative and y ≥ 0: atan2(y,x) = atan(y/x) + π
-- For x strictly negative and y < 0: atan2(y,x) = atan(y/x) - π
-- For x = 0: atan2(y,0) = sign(y)·π/2
--
-- This implementation requires x strictly positive or strictly negative
-- (i.e., the x interval must not contain zero).
iatan2 :: Interval -> Interval -> Interval
iatan2 y x
  | strictlyPositive x =
      -- atan2(y,x) = atan(y/x), monotone ↑ in y, monotone ↓ in x.
      -- Minimum at (y_lo, x_hi), maximum at (y_hi, x_lo).
      let thetaMin = iatanPoint (lo y / hi x)
          thetaMax = iatanPoint (hi y / lo x)
      in Interval (lo thetaMin) (hi thetaMax)
  | strictlyNegative x && lo y >= 0 =
      -- All y ≥ 0, all x < 0: θ = atan(y/x) + π ∈ [π/2, π]
      -- atan(y/x): y ≥ 0, x < 0 so y/x ≤ 0; atan(y/x) ∈ [-π/2, 0]
      -- Monotone ↑ in y (more negative x denom → less negative ratio when y↑)
      -- Actually: d/dy atan(y/x) = x/(x²+y²) < 0 when x < 0, so ↓ in y!
      -- d/dx atan(y/x) = -y/(x²+y²) ≤ 0 when y ≥ 0, so ↓ in x.
      -- Min at (y_hi, x_lo), max at (y_lo, x_hi).
      let thetaMin = iadd (iatanPoint (hi y / lo x)) piInterval
          thetaMax = iadd (iatanPoint (lo y / hi x)) piInterval
      in Interval (lo thetaMin) (hi thetaMax)
  | strictlyNegative x && hi y <= 0 =
      -- All y ≤ 0, all x < 0: θ = atan(y/x) - π ∈ [-π, -π/2]
      -- d/dy atan(y/x) = x/(x²+y²) < 0, so ↓ in y.
      -- d/dx atan(y/x) = -y/(x²+y²) ≥ 0 when y ≤ 0, so ↑ in x.
      -- Min at (y_hi, x_lo), max at (y_lo, x_hi) ... but signs flip with -π.
      -- min atan(y/x)-π at largest |atan(y/x)| negated: (y_lo, x_hi) gives
      --   most negative y/x ratio → atan closest to 0 → minus π → most negative.
      -- Wait, let me re-derive. y ≤ 0, x < 0 so y/x ≥ 0 → atan(y/x) ≥ 0.
      -- atan(y/x) - π: range is [-π, -π/2].
      -- atan(y/x) ↓ in y (x < 0): so atan(y_lo/x) ≥ atan(y_hi/x)
      -- atan(y/x) ↓ in x (y ≤ 0, -y ≥ 0, so -y/(x²+y²) ≥ 0... wait):
      -- d/dx = -y/(x²+y²), y ≤ 0 ⇒ -y ≥ 0, so ↑ in x.
      -- Max atan at (y_lo, x_hi), min atan at (y_hi, x_lo).
      -- So max (atan-π) at (y_lo, x_hi), min (atan-π) at (y_hi, x_lo).
      let negPi = Interval (negate (hi piInterval)) (negate (lo piInterval))
          thetaMin = iadd (iatanPoint (hi y / lo x)) negPi
          thetaMax = iadd (iatanPoint (lo y / hi x)) negPi
      in Interval (lo thetaMin) (hi thetaMax)
  | strictlyNegative x =
      -- x < 0, y spans zero: union of the two cases above.
      -- θ ranges from just below -π (y slightly negative, x negative)
      -- up to just below π (y slightly positive, x negative).
      -- Actually the range is [-π, π] minus a neighborhood of 0.
      -- We compute both branches and take the hull.
      let negPi = Interval (negate (hi piInterval)) (negate (lo piInterval))
          -- Lower bound: y < 0 branch, minimum at (y near 0⁻, x_lo)
          -- Upper bound: y ≥ 0 branch, maximum at (y near 0⁺, x_hi)
          -- For y crossing zero with x < 0, the discontinuity is at y=0
          -- where θ jumps from -π to +π. We must cover both sides.
          thetaLow = iadd (iatanPoint (lo y / lo x)) negPi
          thetaHigh = iadd (iatanPoint (hi y / hi x)) piInterval
      in Interval (lo thetaLow) (hi thetaHigh)
  | otherwise =
      -- x interval contains zero: the full range of atan2 is possible
      Interval (negate (hi piInterval)) (hi piInterval)

-- | Complex interval nth root.
-- z^(1/n) = |z|^(1/n) · (cos(θ/n) + i·sin(θ/n))
-- where θ = atan2(Im z, Re z) is the principal argument.
-- Returns the principal nth root (argument in (-π/n, π/n]).
cinthroot :: Int -> ComplexInterval -> ComplexInterval
cinthroot n z
  | n <= 0    = error "cinthroot: non-positive root index"
  | n == 1    = z
  | otherwise =
      let re = ciReal z
          im = ciImag z
          -- |z|² = re² + im²
          magSq = iadd (imul re re) (imul im im)
          -- |z| = √(|z|²)
          mag = isqrt magSq
          -- |z|^(1/n)
          magRoot = inth n mag
          -- θ = atan2(im, re)
          theta = iatan2 im re
          -- θ/n
          nIv = Interval (fromIntegral n) (fromIntegral n)
          thetaN = idiv theta nIv
          -- cos(θ/n) and sin(θ/n)
          cosTheta = icos thetaN
          sinTheta = isin thetaN
      in ComplexInterval (imul magRoot cosTheta) (imul magRoot sinTheta)
