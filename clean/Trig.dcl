definition module Trig

// Exact symbolic evaluation of trigonometric functions at
// rational multiples of pi.

from RadExpr import :: RadExpr
from Rational import :: Rational
from Poly import :: Poly

:: TrigResult
    = Radical !(RadExpr Rational)
    | MinPoly !(Poly Rational)

// Compute cos(p*pi/q) exactly.
cosExact :: !Int !Int -> TrigResult

// Compute sin(p*pi/q) exactly.
sinExact :: !Int !Int -> TrigResult

// Simplify a trig result for display.
simplifyTrigResult :: !TrigResult -> TrigResult
