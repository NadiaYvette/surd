definition module Resultant

// Polynomial resultant via subresultant PRS.

from Poly import :: Poly
from Rational import :: Rational

// Compute the resultant of two polynomials over Q.
polyResultant :: !(Poly Rational) !(Poly Rational) -> Rational
