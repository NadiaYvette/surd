implementation module Interval

import StdEnv
import Rational

mkInterval :: !Rational !Rational -> Interval
mkInterval l h = { iv_lo = l, iv_hi = h }

pointInterval :: !Rational -> Interval
pointInterval x = { iv_lo = x, iv_hi = x }

ivLo :: !Interval -> Rational
ivLo iv = iv.iv_lo

ivHi :: !Interval -> Rational
ivHi iv = iv.iv_hi

midpoint :: !Interval -> Rational
midpoint iv = (iv.iv_lo + iv.iv_hi) / (ratFromInt 2)

width :: !Interval -> Rational
width iv = iv.iv_hi - iv.iv_lo

contains :: !Interval !Rational -> Bool
contains iv x = iv.iv_lo <= x && x <= iv.iv_hi
where
    (<=) :: !Rational !Rational -> Bool
    (<=) a b = not (b < a)

overlaps :: !Interval !Interval -> Bool
overlaps a b = leq a.iv_lo b.iv_hi && leq b.iv_lo a.iv_hi
where
    leq :: !Rational !Rational -> Bool
    leq x y = not (y < x)

strictlyPositive :: !Interval -> Bool
strictlyPositive iv = zero < iv.iv_lo

strictlyNegative :: !Interval -> Bool
strictlyNegative iv = iv.iv_hi < zero

containsZero :: !Interval -> Bool
containsZero iv = not (zero < iv.iv_lo) && not (iv.iv_hi < zero)
    // lo <= 0 && hi >= 0

bisect :: !Interval -> (Interval, Interval)
bisect iv = (mkInterval iv.iv_lo m, mkInterval m iv.iv_hi)
where
    m = midpoint iv

iadd :: !Interval !Interval -> Interval
iadd a b = mkInterval (a.iv_lo + b.iv_lo) (a.iv_hi + b.iv_hi)

isub :: !Interval !Interval -> Interval
isub a b = mkInterval (a.iv_lo - b.iv_hi) (a.iv_hi - b.iv_lo)

imul :: !Interval !Interval -> Interval
imul a b = mkInterval (minR ps) (maxR ps)
where
    ps = [a.iv_lo * b.iv_lo, a.iv_lo * b.iv_hi, a.iv_hi * b.iv_lo, a.iv_hi * b.iv_hi]

iinv :: !Interval -> Interval
iinv iv
    | zero < iv.iv_lo || iv.iv_hi < zero
        = mkInterval (one / iv.iv_hi) (one / iv.iv_lo)
    = abort "iinv: interval contains zero"

idiv :: !Interval !Interval -> Interval
idiv a b = imul a (iinv b)

ipow :: !Interval !Int -> Interval
ipow _ 0 = mkInterval one one
ipow iv n
    | n < 0 = iinv (ipow iv (0 - n))
    | isOdd n = foldr1 imul (repeatn n iv)
    = let ps = [ratPow iv.iv_lo n, ratPow iv.iv_hi n]
      in if (leq iv.iv_lo zero && leq zero iv.iv_hi)
            (mkInterval zero (maxR ps))
            (mkInterval (minR ps) (maxR ps))
where
    leq :: !Rational !Rational -> Bool
    leq x y = not (y < x)

    foldr1 :: (a a -> a) [a] -> a
    foldr1 _ [x] = x
    foldr1 f [x:xs] = f x (foldr1 f xs)
    foldr1 _ [] = abort "foldr1: empty list"

iabs :: !Interval -> Interval
iabs iv
    | leq zero iv.iv_lo = iv
    | leq iv.iv_hi zero = mkInterval (~ iv.iv_hi) (~ iv.iv_lo)
    = mkInterval zero (maxR [~ iv.iv_lo, iv.iv_hi])
where
    leq :: !Rational !Rational -> Bool
    leq x y = not (y < x)

ineg :: !Interval -> Interval
ineg iv = mkInterval (~ iv.iv_hi) (~ iv.iv_lo)

// Helpers for Rational min/max over lists
minR :: [Rational] -> Rational
minR [x] = x
minR [x:xs]
    | x < m = x
    = m
where m = minR xs
minR [] = abort "minR: empty list"

maxR :: [Rational] -> Rational
maxR [x] = x
maxR [x:xs]
    | m < x = x
    = m
where m = maxR xs
maxR [] = abort "maxR: empty list"

instance == Interval where
    (==) a b = a.iv_lo == b.iv_lo && a.iv_hi == b.iv_hi

instance toString Interval where
    toString iv = "[" +++ toString iv.iv_lo +++ ", " +++ toString iv.iv_hi +++ "]"
