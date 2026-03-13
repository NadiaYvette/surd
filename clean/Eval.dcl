definition module Eval

// Numerical evaluation of radical expressions.

from RadExpr import :: RadExpr
from Rational import :: Rational
from Interval import :: Interval

// Evaluate to a Real (= Double). Even roots of negative numbers give NaN.
eval :: !(RadExpr Rational) -> Real

// Evaluate to a complex pair (re, im). Handles complex intermediates.
evalComplex :: !(RadExpr Rational) -> (Real, Real)

// Principal nth root of a complex number in (re, im) form.
complexNthRoot :: !Int !(Real, Real) -> (Real, Real)

// Evaluate to a rational interval enclosure.
evalInterval :: !(RadExpr Rational) -> Interval
