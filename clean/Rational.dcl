definition module Rational

// Arbitrary-precision rational numbers.
// Represented as Integer numerator / Integer denominator, always in lowest terms
// with positive denominator.

import StdOverloaded
from Data.Integer import :: Integer, class toInteger

:: Rational

// Construction
mkRational :: !Integer !Integer -> Rational
ratFromInt :: !Int -> Rational
ratFromInteger :: !Integer -> Rational

// Access
numer :: !Rational -> Integer
denom :: !Rational -> Integer

// Arithmetic instances
instance + Rational
instance - Rational
instance * Rational
instance / Rational
instance zero Rational
instance one Rational
instance ~ Rational
instance abs Rational
instance sign Rational

// Comparison instances
instance == Rational
instance < Rational

// Conversion
instance toString Rational
instance fromInt Rational

// Integer power
ratPow :: !Rational !Int -> Rational

// Algebraic structure instances (see Algebra module)
from Algebra import class Ring, class Field
