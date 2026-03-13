--- Interval arithmetic with rational endpoints for root isolation
--- and numerical evaluation.
module Interval
  ( Interval(..)
  , midpoint
  , width
  , ivContains
  , overlaps
  , bisect
  , refine
  , fromRat
  -- Interval arithmetic
  , iadd, isub, imul, iinv, idiv, ipow
  , isqrt, inth
  , iabs
  , strictlyPositive, strictlyNegative, containsZero
  -- Complex intervals
  , ComplexInterval(..)
  , ciFromRat, ciFromReal
  , ciadd, cisub, cimul, ciinv, cineg, cipow
  , ciMagnitudeSq, ciRealPart, ciImagPart
  -- Trigonometric interval functions
  , iatan, icos, isin, iatan2
  , piInterval
  -- Complex nth root
  , cinthroot
  , showInterval
  ) where

import Rational

--- Convert Int to Rational (local alias to avoid ambiguity with Prelude.ri).
ri :: Int -> Rational
ri = Rational.fromInt

--- A closed interval [lo, hi] with rational endpoints.
data Interval = IV Rational Rational

--- Midpoint.
midpoint :: Interval -> Rational
midpoint (IV l h) = ratDiv (ratAdd l h) (ri 2)

--- Width.
width :: Interval -> Rational
width (IV l h) = ratSub h l

--- Does the interval contain a rational value?
ivContains :: Interval -> Rational -> Bool
ivContains (IV l h) x = ratLe l x && ratLe x h

--- Do two intervals overlap?
overlaps :: Interval -> Interval -> Bool
overlaps (IV l1 h1) (IV l2 h2) = ratLe l1 h2 && ratLe l2 h1

--- Bisect an interval into two halves.
bisect :: Interval -> (Interval, Interval)
bisect iv@(IV l h) =
  let m = midpoint iv
  in (IV l m, IV m h)

--- Narrow by bisection, keeping the half where the predicate holds at midpoint.
refine :: (Rational -> Bool) -> Interval -> Interval
refine p iv =
  let (left, _) = bisect iv
      m = midpoint iv
  in if p m then left else snd (bisect iv)

--- Point interval from a rational.
fromRat :: Rational -> Interval
fromRat x = IV x x

--- Addition.
iadd :: Interval -> Interval -> Interval
iadd (IV l1 h1) (IV l2 h2) = IV (ratAdd l1 l2) (ratAdd h1 h2)

--- Subtraction.
isub :: Interval -> Interval -> Interval
isub (IV l1 h1) (IV l2 h2) = IV (ratSub l1 h2) (ratSub h1 l2)

--- Multiplication.
imul :: Interval -> Interval -> Interval
imul (IV l1 h1) (IV l2 h2) =
  let ps = [ratMul l1 l2, ratMul l1 h2, ratMul h1 l2, ratMul h1 h2]
  in IV (foldl1 ratMin ps) (foldl1 ratMax ps)

--- Inverse of an interval not containing zero.
iinv :: Interval -> Interval
iinv (IV l h)
  | ratGt l (ri 0) || ratLt h (ri 0) =
      IV (ratDiv (ri 1) h) (ratDiv (ri 1) l)
  | otherwise = error "iinv: interval contains zero"

--- Division.
idiv :: Interval -> Interval -> Interval
idiv a b = imul a (iinv b)

--- Integer power.
ipow :: Interval -> Int -> Interval
ipow iv n
  | n == 0    = IV (ri 1) (ri 1)
  | n < 0     = iinv (ipow iv (negate n))
  | odd n     = foldl1 imul (replicate n iv)
  | otherwise =
      let (IV l h) = iv
          pl = ratPow l n
          ph = ratPow h n
          ps = [pl, ph]
      in if ratLe l (ri 0) && ratGe (ri 0) (ratNeg h)
         then IV (ri 0) (foldl1 ratMax ps)
         else IV (foldl1 ratMin ps) (foldl1 ratMax ps)

--- Square root of a non-negative interval via bisection.
isqrt :: Interval -> Interval
isqrt (IV l h)
  | ratLt l (ri 0) = error "isqrt: negative interval"
  | h == ri 0      = IV (ri 0) (ri 0)
  | otherwise           = IV (nthRootLower 2 l) (nthRootUpper 2 h)

--- Nth root of an interval.
inth :: Int -> Interval -> Interval
inth n (IV l h)
  | n <= 0               = error "inth: non-positive root index"
  | n == 1               = IV l h
  | n == 2               = isqrt (IV l h)
  | even n && ratLt l (ri 0) = error "inth: even root of negative interval"
  | h == ri 0 && l == ri 0 = IV (ri 0) (ri 0)
  | odd n && ratLt h (ri 0)  =
      let (IV l' h') = inth n (IV (ratNeg h) (ratNeg l))
      in IV (ratNeg h') (ratNeg l')
  | odd n && ratLt l (ri 0)  =
      let negPart = nthRootUpper n (ratNeg l)
          posPart = nthRootUpper n h
      in IV (ratNeg negPart) posPart
  | otherwise = IV (nthRootLower n l) (nthRootUpper n h)

--- Find rational lower bound r such that r^n <= a, via bisection.
nthRootLower :: Int -> Rational -> Rational
nthRootLower n a
  | a == ri 0 = ri 0
  | otherwise =
      let upper = ratMax a (ri 1)
      in bisectDown n a (ri 0) upper 60

--- Find rational upper bound r such that r^n >= a, via bisection.
nthRootUpper :: Int -> Rational -> Rational
nthRootUpper n a
  | a == ri 0 = ri 0
  | otherwise =
      let upper = ratAdd (ratMax a (ri 1)) (ri 1)
      in bisectUp n a (ri 0) upper 60

bisectDown :: Int -> Rational -> Rational -> Rational -> Int -> Rational
bisectDown n a lo hi iters
  | iters == 0 = lo
  | otherwise  =
      let mid = ratDiv (ratAdd lo hi) (ri 2)
      in if ratLe (ratPow mid n) a
         then bisectDown n a mid hi (iters - 1)
         else bisectDown n a lo mid (iters - 1)

bisectUp :: Int -> Rational -> Rational -> Rational -> Int -> Rational
bisectUp n a lo hi iters
  | iters == 0 = hi
  | otherwise  =
      let mid = ratDiv (ratAdd lo hi) (ri 2)
      in if ratGe (ratPow mid n) a
         then bisectUp n a lo mid (iters - 1)
         else bisectUp n a mid hi (iters - 1)

--- Absolute value of an interval.
iabs :: Interval -> Interval
iabs (IV l h)
  | ratGe l (ri 0) = IV l h
  | ratLe h (ri 0) = IV (ratNeg h) (ratNeg l)
  | otherwise            = IV (ri 0) (ratMax (ratNeg l) h)

--- Test if interval is strictly positive.
strictlyPositive :: Interval -> Bool
strictlyPositive (IV l _) = ratGt l (ri 0)

--- Test if interval is strictly negative.
strictlyNegative :: Interval -> Bool
strictlyNegative (IV _ h) = ratLt h (ri 0)

--- Test if interval contains zero.
containsZero :: Interval -> Bool
containsZero (IV l h) = ratLe l (ri 0) && ratGe h (ri 0)

--- ---------------------------------------------------------------
--- Complex intervals (rectangular: real part x imaginary part)
--- ---------------------------------------------------------------

--- A complex interval [re_lo, re_hi] + i*[im_lo, im_hi].
data ComplexInterval = CI Interval Interval

--- From a rational (real, zero imaginary part).
ciFromRat :: Rational -> ComplexInterval
ciFromRat r = CI (fromRat r) (fromRat (ri 0))

--- From a real interval.
ciFromReal :: Interval -> ComplexInterval
ciFromReal r = CI r (fromRat (ri 0))

--- Complex addition.
ciadd :: ComplexInterval -> ComplexInterval -> ComplexInterval
ciadd (CI r1 i1) (CI r2 i2) = CI (iadd r1 r2) (iadd i1 i2)

--- Complex subtraction.
cisub :: ComplexInterval -> ComplexInterval -> ComplexInterval
cisub (CI r1 i1) (CI r2 i2) = CI (isub r1 r2) (isub i1 i2)

--- Complex negation.
cineg :: ComplexInterval -> ComplexInterval
cineg (CI (IV rl rh) (IV il ih)) =
  CI (IV (ratNeg rh) (ratNeg rl)) (IV (ratNeg ih) (ratNeg il))

--- Complex multiplication: (a+bi)(c+di) = (ac-bd) + (ad+bc)i
cimul :: ComplexInterval -> ComplexInterval -> ComplexInterval
cimul (CI r1 i1) (CI r2 i2) =
  CI (isub (imul r1 r2) (imul i1 i2))
     (iadd (imul r1 i2) (imul i1 r2))

--- Complex inverse: 1/(a+bi) = (a-bi)/(a^2+b^2)
ciinv :: ComplexInterval -> ComplexInterval
ciinv (CI r i) =
  let magSq = iadd (imul r r) (imul i i)
      (IV il ih) = i
  in CI (idiv r magSq) (idiv (IV (ratNeg ih) (ratNeg il)) magSq)

--- Complex power.
cipow :: ComplexInterval -> Int -> ComplexInterval
cipow z n
  | n == 0    = ciFromRat (ri 1)
  | n == 1    = z
  | n < 0     = cipow (ciinv z) (negate n)
  | even n    = let half = cipow z (n `div` 2) in cimul half half
  | otherwise = cimul z (cipow z (n - 1))

--- |z|^2 = re^2 + im^2 as an interval.
ciMagnitudeSq :: ComplexInterval -> Interval
ciMagnitudeSq (CI r i) = iadd (imul r r) (imul i i)

--- Real part.
ciRealPart :: ComplexInterval -> Interval
ciRealPart (CI r _) = r

--- Imaginary part.
ciImagPart :: ComplexInterval -> Interval
ciImagPart (CI _ i) = i

--- ---------------------------------------------------------------
--- Trigonometric interval functions
--- ---------------------------------------------------------------

--- Rational interval enclosure of pi via Machin's formula:
---   pi/4 = 4*atan(1/5) - atan(1/239)
piInterval :: Interval
piInterval =
  let a = iatanSmall (rat 1 5)
      b = iatanSmall (rat 1 239)
      four = IV (ri 4) (ri 4)
  in imul four (isub (imul four a) b)

--- atan(r) for |r| <= 1 via Taylor series with remainder bound.
iatanSmall :: Rational -> Interval
iatanSmall r =
  let nTerms = 50
      partialSum = foldl ratAdd (ri 0)
        [ ratMul (if even k then ri 1 else ri (negate 1))
                 (ratDiv (ratPow r (2*k+1))
                         (ri (2*k+1)))
        | k <- [0 .. nTerms - 1]
        ]
      ar = ratAbs r
      remBound = ratDiv (ratPow ar (2 * nTerms + 1))
                        (ri (2 * nTerms + 1))
  in IV (ratSub partialSum remBound) (ratAdd partialSum remBound)

--- atan of a single rational value, returned as an interval.
iatanPoint :: Rational -> Interval
iatanPoint r
  | r == ri 0     = IV (ri 0) (ri 0)
  | ratLe (ratAbs r) (ri 1) = iatanSmall r
  | ratGt r (ri 0) =
      let piHalf = idiv piInterval (IV (ri 2) (ri 2))
      in isub piHalf (iatanSmall (ratDiv (ri 1) r))
  | otherwise =
      let piHalf = idiv piInterval (IV (ri 2) (ri 2))
          (IV pl ph) = piHalf
          negPiHalf = IV (ratNeg ph) (ratNeg pl)
      in iadd negPiHalf (iatanSmall (ratDiv (ri 1) (ratNeg r)))

--- Interval enclosure of atan over an interval.
iatan :: Interval -> Interval
iatan (IV l h) =
  let (IV al _) = iatanPoint l
      (IV _ bh) = iatanPoint h
  in IV al bh

--- cos of a single rational value via Taylor series.
icosPoint :: Rational -> Interval
icosPoint x =
  let nTerms = 30
      partialSum = foldl ratAdd (ri 0)
        [ ratMul (if even k then ri 1 else ri (negate 1))
                 (ratDiv (ratPow x (2*k))
                         (factorial (2*k)))
        | k <- [0 .. nTerms - 1]
        ]
      remBound = ratDiv (ratPow (ratAbs x) (2 * nTerms))
                        (factorial (2 * nTerms))
  in IV (ratSub partialSum remBound) (ratAdd partialSum remBound)

--- Factorial as Rational.
factorial :: Int -> Rational
factorial n = ri (go 1 n)
  where
    go acc k
      | k == 0    = acc
      | otherwise = go (acc * k) (k - 1)

--- Interval enclosure of cos over an interval.
icos :: Interval -> Interval
icos iv@(IV l h)
  | l == h    = icosPoint l
  | ratGt (width iv) (ratMul (ivHi piInterval) (ri 2)) =
      IV (ri (negate 1)) (ri 1)
  | otherwise =
      let cosL = icosPoint l
          cosH = icosPoint h
          baseLo = ratMin (ivLo cosL) (ivLo cosH)
          baseHi = ratMax (ivHi cosL) (ivHi cosH)
          piLo = ivLo piInterval
          piHi = ivHi piInterval
          kMin = ratCeiling (ratDiv l piHi)
          kMax = ratFloor (ratDiv h piLo)
          hasMax1    = any even [kMin..kMax]
          hasMinNeg1 = any odd  [kMin..kMax]
          finalLo = if hasMinNeg1 then ratMin baseLo (ri (negate 1)) else baseLo
          finalHi = if hasMax1 then ratMax baseHi (ri 1) else baseHi
          clampedLo = ratMax (ri (negate 1)) finalLo
          clampedHi = ratMin (ri 1) finalHi
      in IV clampedLo clampedHi

--- Extract lo/hi from Interval (helpers).
ivLo :: Interval -> Rational
ivLo (IV l _) = l

ivHi :: Interval -> Rational
ivHi (IV _ h) = h

--- sin via cos(x - pi/2).
isin :: Interval -> Interval
isin iv =
  let piHalf = idiv piInterval (IV (ri 2) (ri 2))
      shifted = isub iv piHalf
  in icos shifted

--- atan2(y, x) for interval arguments.
iatan2 :: Interval -> Interval -> Interval
iatan2 y x
  | strictlyPositive x =
      let thetaMin = iatanPoint (ratDiv (ivLo y) (ivHi x))
          thetaMax = iatanPoint (ratDiv (ivHi y) (ivLo x))
      in IV (ivLo thetaMin) (ivHi thetaMax)
  | strictlyNegative x && ratGe (ivLo y) (ri 0) =
      let thetaMin = iadd (iatanPoint (ratDiv (ivHi y) (ivLo x))) piInterval
          thetaMax = iadd (iatanPoint (ratDiv (ivLo y) (ivHi x))) piInterval
      in IV (ivLo thetaMin) (ivHi thetaMax)
  | strictlyNegative x && ratLe (ivHi y) (ri 0) =
      let negPi = let (IV pl ph) = piInterval in IV (ratNeg ph) (ratNeg pl)
          thetaMin = iadd (iatanPoint (ratDiv (ivHi y) (ivLo x))) negPi
          thetaMax = iadd (iatanPoint (ratDiv (ivLo y) (ivHi x))) negPi
      in IV (ivLo thetaMin) (ivHi thetaMax)
  | strictlyNegative x =
      let negPi = let (IV pl ph) = piInterval in IV (ratNeg ph) (ratNeg pl)
          thetaLow  = iadd (iatanPoint (ratDiv (ivLo y) (ivLo x))) negPi
          thetaHigh = iadd (iatanPoint (ratDiv (ivHi y) (ivHi x))) piInterval
      in IV (ivLo thetaLow) (ivHi thetaHigh)
  | otherwise =
      let (IV _ ph) = piInterval
      in IV (ratNeg ph) ph

--- Complex interval nth root via polar decomposition.
cinthroot :: Int -> ComplexInterval -> ComplexInterval
cinthroot n z
  | n <= 0    = error "cinthroot: non-positive root index"
  | n == 1    = z
  | otherwise =
      let re = ciRealPart z
          im = ciImagPart z
          magSq = iadd (imul re re) (imul im im)
          mag = isqrt magSq
          magRoot = inth n mag
          nIv = IV (ri n) (ri n)
          theta = iatan2 im re
          thetaN = idiv theta nIv
          cosTheta = icos thetaN
          sinTheta = isin thetaN
      in CI (imul magRoot cosTheta) (imul magRoot sinTheta)

--- Show an interval.
showInterval :: Interval -> String
showInterval (IV l h) = "[" ++ show l ++ ", " ++ show h ++ "]"

instance Eq Interval where
  (IV l1 h1) == (IV l2 h2) = l1 == l2 && h1 == h2

instance Ord Interval where
  compare (IV l1 h1) (IV l2 h2) = case compare l1 l2 of
                                     EQ -> compare h1 h2
                                     o  -> o

instance Show Interval where
  show = showInterval

instance Eq ComplexInterval where
  (CI r1 i1) == (CI r2 i2) = r1 == r2 && i1 == i2

instance Show ComplexInterval where
  show (CI r i) = show r ++ " + i*" ++ show i
