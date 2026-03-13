module Surd.Interval

import Surd.Rational
import Data.Nat

%default covering

------------------------------------------------------------------------
-- Real intervals
------------------------------------------------------------------------

||| A closed interval [lo, hi] with rational endpoints.
export
record Interval where
  constructor MkInterval
  lo : Rational
  hi : Rational

export
Eq Interval where
  a == b = lo a == lo b && hi a == hi b

export
Show Interval where
  show iv = "[" ++ show (lo iv) ++ ", " ++ show (hi iv) ++ "]"

||| Midpoint of an interval.
export
midpoint : Interval -> Rational
midpoint iv = (lo iv + hi iv) / Rational.fromInteger 2

||| Width of an interval.
export
width : Interval -> Rational
width iv = hi iv - lo iv

||| Does the interval contain a given rational?
export
contains : Interval -> Rational -> Bool
contains iv x = lo iv <= x && x <= hi iv

||| Do two intervals overlap?
export
overlaps : Interval -> Interval -> Bool
overlaps a b = lo a <= hi b && lo b <= hi a

||| Bisect an interval into two halves.
export
bisect : Interval -> (Interval, Interval)
bisect iv =
  let m = midpoint iv
  in (MkInterval (lo iv) m, MkInterval m (hi iv))

||| Narrow an interval by bisection, keeping the half where
||| the midpoint satisfies the predicate.
export
refine : (Rational -> Bool) -> Interval -> Interval
refine p iv =
  let (left, right) = bisect iv
      m = midpoint iv
  in if p m then left else right

||| Point interval from a rational.
export
fromRational : Rational -> Interval
fromRational x = MkInterval x x

------------------------------------------------------------------------
-- Interval arithmetic
------------------------------------------------------------------------

||| Interval addition.
export
iadd : Interval -> Interval -> Interval
iadd a b = MkInterval (lo a + lo b) (hi a + hi b)

||| Interval subtraction.
export
isub : Interval -> Interval -> Interval
isub a b = MkInterval (lo a - hi b) (hi a - lo b)

||| Negation of an interval.
export
ineg : Interval -> Interval
ineg iv = MkInterval (negate (hi iv)) (negate (lo iv))

min4 : Ord a => a -> a -> a -> a -> a
min4 a b c d = min (min a b) (min c d)

max4 : Ord a => a -> a -> a -> a -> a
max4 a b c d = max (max a b) (max c d)

||| Interval multiplication.
export
imul : Interval -> Interval -> Interval
imul a b =
  let p1 = lo a * lo b
      p2 = lo a * hi b
      p3 = hi a * lo b
      p4 = hi a * hi b
  in MkInterval (min4 p1 p2 p3 p4) (max4 p1 p2 p3 p4)

||| Inverse of an interval that does not contain zero.
export
iinv : Interval -> Interval
iinv iv =
  if lo iv > Rational.zero || hi iv < Rational.zero
    then MkInterval (recip (hi iv)) (recip (lo iv))
    else MkInterval Rational.zero Rational.zero  -- degenerate: contains zero

||| Interval division.
export
idiv : Interval -> Interval -> Interval
idiv a b = imul a (iinv b)

-- Helper: check if integer is even
isEvenInt : Int -> Bool
isEvenInt n = mod n 2 == 0

-- Helper: repeated interval multiplication
ipowNat : Interval -> Nat -> Interval
ipowNat _ Z = MkInterval Rational.one Rational.one
ipowNat iv (S Z) = iv
ipowNat iv (S k) = imul iv (ipowNat iv k)

||| Interval power.
export
ipow : Interval -> Int -> Interval
ipow _ 0 = MkInterval Rational.one Rational.one
ipow iv n =
  if n < 0 then iinv (ipow iv (negate n))
  else if not (isEvenInt n)
    then ipowNat iv (cast n)
    else
      let pLo = powRatInt (lo iv) (cast n)
          pHi = powRatInt (hi iv) (cast n)
      in if lo iv <= Rational.zero && hi iv >= Rational.zero
           then MkInterval Rational.zero (max pLo pHi)
           else MkInterval (min pLo pHi) (max pLo pHi)

------------------------------------------------------------------------
-- Nth root via bisection
------------------------------------------------------------------------

||| Bisect to find largest r in [lo, hi] with r^n <= a.
bisectDown : Nat -> Integer -> Rational -> Rational -> Rational -> Rational
bisectDown Z _ lo _ _ = lo
bisectDown (S k) n lo hi a =
  let mid = (lo + hi) / Rational.fromInteger 2
  in if powRatInt mid n <= a
       then bisectDown k n mid hi a
       else bisectDown k n lo mid a

||| Bisect to find smallest r in [lo, hi] with r^n >= a.
bisectUp : Nat -> Integer -> Rational -> Rational -> Rational -> Rational
bisectUp Z _ _ hi _ = hi
bisectUp (S k) n lo hi a =
  let mid = (lo + hi) / Rational.fromInteger 2
  in if powRatInt mid n >= a
       then bisectUp k n lo mid a
       else bisectUp k n mid hi a

nthRootLower : Integer -> Rational -> Rational
nthRootLower n r = if Surd.Rational.isZero r then Rational.zero
  else bisectDown 60 n Rational.zero (max r Rational.one) r

nthRootUpper : Integer -> Rational -> Rational
nthRootUpper n r = if Surd.Rational.isZero r then Rational.zero
  else bisectUp 60 n Rational.zero (max r Rational.one + Rational.one) r

||| Interval enclosure of the square root of a non-negative interval.
export
isqrt : Interval -> Interval
isqrt iv =
  if hi iv == Rational.zero
    then MkInterval Rational.zero Rational.zero
    else MkInterval (nthRootLower 2 (lo iv)) (nthRootUpper 2 (hi iv))

||| Interval enclosure of the nth root of an interval.
export
inth : Int -> Interval -> Interval
inth n iv =
  let ni = cast {to = Integer} n
  in if n <= 0 then MkInterval Rational.zero Rational.zero
  else if n == 1 then iv
  else if n == 2 then isqrt iv
  else if isEvenInt n
    then
      if hi iv == Rational.zero && lo iv == Rational.zero
        then MkInterval Rational.zero Rational.zero
        else MkInterval (nthRootLower ni (lo iv)) (nthRootUpper ni (hi iv))
    else
      if hi iv < Rational.zero
        then let pos = inth n (MkInterval (negate (hi iv)) (negate (lo iv)))
             in MkInterval (negate (hi pos)) (negate (lo pos))
        else if lo iv < Rational.zero
          then let negPart = nthRootUpper ni (negate (lo iv))
                   posPart = nthRootUpper ni (hi iv)
               in MkInterval (negate negPart) posPart
          else MkInterval (nthRootLower ni (lo iv)) (nthRootUpper ni (hi iv))

||| Absolute value of an interval.
export
iabs : Interval -> Interval
iabs iv =
  if lo iv >= Rational.zero then iv
  else if hi iv <= Rational.zero then MkInterval (negate (hi iv)) (negate (lo iv))
  else MkInterval Rational.zero (max (negate (lo iv)) (hi iv))

||| Test if an interval is strictly positive.
export
strictlyPositive : Interval -> Bool
strictlyPositive iv = lo iv > Rational.zero

||| Test if an interval is strictly negative.
export
strictlyNegative : Interval -> Bool
strictlyNegative iv = hi iv < Rational.zero

||| Test if an interval contains zero.
export
containsZero : Interval -> Bool
containsZero iv = lo iv <= Rational.zero && hi iv >= Rational.zero

------------------------------------------------------------------------
-- Complex intervals
------------------------------------------------------------------------

||| A complex interval [re_lo, re_hi] + i*[im_lo, im_hi].
export
record ComplexInterval where
  constructor MkComplexInterval
  ciReal : Interval
  ciImag : Interval

export
Show ComplexInterval where
  show ci = show (ciReal ci) ++ " + i*" ++ show (ciImag ci)

export
Eq ComplexInterval where
  a == b = ciReal a == ciReal b && ciImag a == ciImag b

||| Complex interval from a rational (real part only).
export
ciFromRational : Rational -> ComplexInterval
ciFromRational r = MkComplexInterval (fromRational r) (fromRational Rational.zero)

||| Complex interval from a real interval.
export
ciFromReal : Interval -> ComplexInterval
ciFromReal r = MkComplexInterval r (fromRational Rational.zero)

||| Complex interval addition.
export
ciadd : ComplexInterval -> ComplexInterval -> ComplexInterval
ciadd a b = MkComplexInterval (iadd (ciReal a) (ciReal b)) (iadd (ciImag a) (ciImag b))

||| Complex interval subtraction.
export
cisub : ComplexInterval -> ComplexInterval -> ComplexInterval
cisub a b = MkComplexInterval (isub (ciReal a) (ciReal b)) (isub (ciImag a) (ciImag b))

||| Complex interval negation.
export
cineg : ComplexInterval -> ComplexInterval
cineg a = MkComplexInterval (ineg (ciReal a)) (ineg (ciImag a))

||| Complex interval multiplication: (a + bi)(c + di) = (ac - bd) + (ad + bc)i
export
cimul : ComplexInterval -> ComplexInterval -> ComplexInterval
cimul a b =
  MkComplexInterval
    (isub (imul (ciReal a) (ciReal b)) (imul (ciImag a) (ciImag b)))
    (iadd (imul (ciReal a) (ciImag b)) (imul (ciImag a) (ciReal b)))

||| Complex interval inverse: 1/(a + bi) = (a - bi)/(a^2 + b^2)
export
ciinv : ComplexInterval -> ComplexInterval
ciinv z =
  let magSq = iadd (imul (ciReal z) (ciReal z)) (imul (ciImag z) (ciImag z))
  in MkComplexInterval (idiv (ciReal z) magSq) (idiv (ineg (ciImag z)) magSq)

||| Complex interval power via repeated squaring.
export
cipow : ComplexInterval -> Int -> ComplexInterval
cipow _ 0 = ciFromRational Rational.one
cipow z 1 = z
cipow z n =
  if n < 0 then cipow (ciinv z) (negate n)
  else if isEvenInt n
    then let half = cipow z (div n 2) in cimul half half
    else cimul z (cipow z (n - 1))

||| |z|^2 = re^2 + im^2 as an interval.
export
ciMagnitudeSq : ComplexInterval -> Interval
ciMagnitudeSq z = iadd (imul (ciReal z) (ciReal z)) (imul (ciImag z) (ciImag z))

||| Real part of a complex interval.
export
ciRealPart : ComplexInterval -> Interval
ciRealPart = ciReal

||| Imaginary part of a complex interval.
export
ciImagPart : ComplexInterval -> Interval
ciImagPart = ciImag

------------------------------------------------------------------------
-- Trigonometric interval functions
------------------------------------------------------------------------

-- Helper: check if Nat is even
isEvenNat : Nat -> Bool
isEvenNat n = modNatNZ n 2 SIsNonZero == 0

-- Helper for atan Taylor series
iatanSmall : Rational -> Interval
iatanSmall r =
  let partialSum : Rational
      partialSum = go 0 Rational.zero
      ar = abs r
      remBound = powRatInt ar 101 / Rational.fromInteger 101
  in MkInterval (partialSum - remBound) (partialSum + remBound)
  where
    go : Nat -> Rational -> Rational
    go k acc =
      if k >= 50 then acc
      else
        let sign : Rational = if isEvenNat k then Rational.one else negate Rational.one
            expt : Integer = cast (2 * k + 1)
            term = sign * powRatInt r expt / Rational.fromInteger expt
        in go (S k) (acc + term)

||| Rational interval enclosure of pi via Machin's formula:
|||   pi/4 = 4*atan(1/5) - atan(1/239)
export
piInterval : Interval
piInterval =
  let a = iatanSmall (mkRat 1 5)
      b = iatanSmall (mkRat 1 239)
      four = MkInterval (Rational.fromInteger 4) (Rational.fromInteger 4)
  in imul four (isub (imul four a) b)

||| atan of a single rational value, returned as an interval.
iatanPoint : Rational -> Interval
iatanPoint r =
  if Surd.Rational.isZero r then MkInterval Rational.zero Rational.zero
  else if absLeq r Rational.one
    then iatanSmall r
    else if r > Rational.zero
      then let piHalf = idiv piInterval
                          (MkInterval (Rational.fromInteger 2) (Rational.fromInteger 2))
           in isub piHalf (iatanSmall (recip r))
      else let piHalf = idiv piInterval
                          (MkInterval (Rational.fromInteger 2) (Rational.fromInteger 2))
               negPiHalf = MkInterval (negate (hi piHalf)) (negate (lo piHalf))
           in iadd negPiHalf (iatanSmall (recip (negate r)))

-- Helper: factorial as Rational
factorial : Integer -> Rational
factorial n = go 1 n
  where
    go : Integer -> Integer -> Rational
    go acc 0 = Rational.fromInteger acc
    go acc k = if k < 0 then Rational.fromInteger acc
               else go (acc * k) (k - 1)

||| Interval atan over an interval (atan is monotone).
export
iatan : Interval -> Interval
iatan iv = MkInterval (lo (iatanPoint (lo iv))) (hi (iatanPoint (hi iv)))

||| cos of a single rational via Taylor series.
icosPoint : Rational -> Interval
icosPoint x =
  let partialSum : Rational
      partialSum = go 0 Rational.zero
      remBound = powRatInt (abs x) 60 / factorial 60
  in MkInterval (partialSum - remBound) (partialSum + remBound)
  where
    go : Nat -> Rational -> Rational
    go k acc =
      if k >= 30 then acc
      else
        let sign : Rational = if isEvenNat k then Rational.one else negate Rational.one
            expt : Integer = cast (2 * k)
            term = sign * powRatInt x expt / factorial expt
        in go (S k) (acc + term)

-- Helper: ceiling for rational
ceiling' : Rational -> Integer
ceiling' r = let n = numer r
                 d = denom r
                 q = div n d
             in if mod n d == 0 then q
                else if n > 0 then q + 1 else q

-- Helper: floor for rational
floor' : Rational -> Integer
floor' r = let n = numer r
               d = denom r
               q = div n d
           in if mod n d == 0 then q
               else if n < 0 then q - 1 else q

-- Helper: is there an even integer in [lo, hi]?
anyEven : Integer -> Integer -> Bool
anyEven lo hi =
  if lo > hi then False
  else let start = if mod lo 2 == 0 then lo else lo + 1
       in start <= hi

-- Helper: is there an odd integer in [lo, hi]?
anyOdd : Integer -> Integer -> Bool
anyOdd lo hi =
  if lo > hi then False
  else let start = if mod lo 2 /= 0 then lo else lo + 1
       in start <= hi

||| Interval enclosure of cos over an interval.
export
icos : Interval -> Interval
icos iv =
  if lo iv == hi iv then icosPoint (lo iv)
  else
    let cosL = icosPoint (lo iv)
        cosH = icosPoint (hi iv)
        baseLo = min (lo cosL) (lo cosH)
        baseHi = max (hi cosL) (hi cosH)
        piHi = hi piInterval
        piLo = lo piInterval
        two = Rational.fromInteger 2
        wid = width iv
    in if wid > piHi * two
         then MkInterval (negate Rational.one) Rational.one
         else
           let kMin : Integer = ceiling' (lo iv / piHi)
               kMax : Integer = floor' (hi iv / piLo)
               hasMax1 = anyEven kMin kMax
               hasMinNeg1 = anyOdd kMin kMax
               finalLo = if hasMinNeg1 then min baseLo (negate Rational.one) else baseLo
               finalHi = if hasMax1 then max baseHi Rational.one else baseHi
           in MkInterval (max (negate Rational.one) finalLo)
                         (min Rational.one finalHi)

||| Interval enclosure of sin over an interval.
||| sin(x) = cos(x - pi/2)
export
isin : Interval -> Interval
isin iv =
  let piHalf = idiv piInterval
                 (MkInterval (Rational.fromInteger 2) (Rational.fromInteger 2))
      shifted = isub iv piHalf
  in icos shifted

||| Interval atan2(y, x).
export
iatan2 : Interval -> Interval -> Interval
iatan2 y x =
  if strictlyPositive x
    then MkInterval (lo (iatanPoint (lo y / hi x)))
                    (hi (iatanPoint (hi y / lo x)))
    else if strictlyNegative x && lo y >= Rational.zero
      then let thetaMin = iadd (iatanPoint (hi y / lo x)) piInterval
               thetaMax = iadd (iatanPoint (lo y / hi x)) piInterval
           in MkInterval (lo thetaMin) (hi thetaMax)
      else if strictlyNegative x && hi y <= Rational.zero
        then let negPi = ineg piInterval
                 thetaMin = iadd (iatanPoint (hi y / lo x)) negPi
                 thetaMax = iadd (iatanPoint (lo y / hi x)) negPi
             in MkInterval (lo thetaMin) (hi thetaMax)
        else if strictlyNegative x
          then let negPi = ineg piInterval
                   thetaLow = iadd (iatanPoint (lo y / lo x)) negPi
                   thetaHigh = iadd (iatanPoint (hi y / hi x)) piInterval
               in MkInterval (lo thetaLow) (hi thetaHigh)
          else MkInterval (negate (hi piInterval)) (hi piInterval)

||| Complex interval nth root.
||| z^(1/n) = |z|^(1/n) * (cos(theta/n) + i*sin(theta/n))
export
cinthroot : Int -> ComplexInterval -> ComplexInterval
cinthroot n z =
  if n <= 0 then ciFromRational Rational.zero
  else if n == 1 then z
  else
    let re = ciReal z
        im = ciImag z
        magSq = iadd (imul re re) (imul im im)
        mag = isqrt magSq
        magRoot = inth n mag
        theta = iatan2 im re
        nIv = MkInterval (Rational.fromInteger (cast n)) (Rational.fromInteger (cast n))
        thetaN = idiv theta nIv
        cosTheta = icos thetaN
        sinTheta = isin thetaN
    in MkComplexInterval (imul magRoot cosTheta) (imul magRoot sinTheta)
