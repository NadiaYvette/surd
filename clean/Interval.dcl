definition module Interval

// Rational interval arithmetic.
// Closed intervals [lo, hi] with Rational endpoints.

import StdOverloaded
from Rational import :: Rational

:: Interval = { iv_lo :: !Rational, iv_hi :: !Rational }

// Construction
mkInterval :: !Rational !Rational -> Interval
pointInterval :: !Rational -> Interval

// Queries
ivLo :: !Interval -> Rational
ivHi :: !Interval -> Rational
midpoint :: !Interval -> Rational
width :: !Interval -> Rational
contains :: !Interval !Rational -> Bool
overlaps :: !Interval !Interval -> Bool
strictlyPositive :: !Interval -> Bool
strictlyNegative :: !Interval -> Bool
containsZero :: !Interval -> Bool

// Bisection
bisect :: !Interval -> (Interval, Interval)

// Interval arithmetic
iadd :: !Interval !Interval -> Interval
isub :: !Interval !Interval -> Interval
imul :: !Interval !Interval -> Interval
iinv :: !Interval -> Interval
idiv :: !Interval !Interval -> Interval
ipow :: !Interval !Int -> Interval
iabs :: !Interval -> Interval
ineg :: !Interval -> Interval

instance == Interval
instance toString Interval
