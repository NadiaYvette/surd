definition module RootIsolation

// Sturm-based root isolation for polynomials over Q.

from Poly import :: Poly
from Rational import :: Rational
from Interval import :: Interval

// Isolate all real roots of a polynomial as disjoint intervals.
isolateRealRoots :: !(Poly Rational) -> [Interval]

// Count real roots in an interval using Sturm's theorem.
sturmCount :: !(Poly Rational) !Rational !Rational -> Int
