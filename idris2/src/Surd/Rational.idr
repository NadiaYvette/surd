||| Exact rational arithmetic with GCD normalization and DecEq.
|||
||| Invariant: den > 0 and gcd(|num|, den) = 1.
||| Implements Num, Neg, Abs, Fractional, Ord, and DecEq.
module Surd.Rational

import Surd.GCD
import Decidable.Equality

%default total

||| A rational number num/den, kept in lowest terms with den > 0.
export
record Rational where
  constructor MkRational
  num : Integer
  den : Integer

||| Smart constructor: normalises sign and divides out GCD.
export
mkRat : Integer -> Integer -> Rational
mkRat _ 0 = MkRational 0 1  -- division by zero -> 0 (or could error)
mkRat 0 _ = MkRational 0 1
mkRat n d =
  let s  : Integer = if d < 0 then -1 else 1
      n' : Integer = s * n
      d' : Integer = s * d
      g  : Integer = gcdInteger (abs n') d'
  in MkRational (n' `div` g) (d' `div` g)

||| Numerator (in lowest terms).
export
numer : Rational -> Integer
numer = num

||| Denominator (in lowest terms, always > 0).
export
denom : Rational -> Integer
denom = den

||| The rational number 0.
export
zero : Rational
zero = MkRational 0 1

||| The rational number 1.
export
one : Rational
one = MkRational 1 1

||| Build a rational from an integer.
export
fromInteger : Integer -> Rational
fromInteger n = MkRational n 1

export
Eq Rational where
  (MkRational a b) == (MkRational c d) = a == c && b == d

export
DecEq Rational where
  decEq (MkRational a b) (MkRational c d) =
    case (decEq a c, decEq b d) of
      (Yes pa, Yes pd) => Yes (rewrite pa in rewrite pd in Refl)
      (No contra, _)   => No (\eq => contra (cong num eq))
      (_, No contra)   => No (\eq => contra (cong den eq))

export
Ord Rational where
  compare (MkRational a b) (MkRational c d) = compare (a * d) (c * b)

export
Show Rational where
  show (MkRational n 1) = show n
  show (MkRational n d) = show n ++ "/" ++ show d

export
Num Rational where
  (MkRational a b) + (MkRational c d) = mkRat (a * d + c * b) (b * d)
  (MkRational a b) * (MkRational c d) = mkRat (a * c) (b * d)
  fromInteger n = MkRational n 1

export
Neg Rational where
  negate (MkRational a b) = MkRational (negate a) b
  (MkRational a b) - (MkRational c d) = mkRat (a * d - c * b) (b * d)

export
Abs Rational where
  abs (MkRational a b) = MkRational (abs a) b

export
Fractional Rational where
  (MkRational a b) / (MkRational c d) = mkRat (a * d) (b * c)
  recip (MkRational a b) = mkRat b a

||| Raise a rational to a non-negative integer power.
export
powRat : Rational -> Nat -> Rational
powRat _ Z = one
powRat r (S k) = r * powRat r k

||| Raise a rational to an integer power (negative exponents use recip).
export
powRatInt : Rational -> Integer -> Rational
powRatInt r n =
  if n >= 0
    then powRat r (cast n)
    else recip (powRat r (cast (negate n)))

||| Check if a rational is zero.
export
isZero : Rational -> Bool
isZero (MkRational 0 _) = True
isZero _ = False

||| Absolute value comparison: |a| <= |b|.
export
absLeq : Rational -> Rational -> Bool
absLeq a b = abs a <= abs b
