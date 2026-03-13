definition module RootBound

// Cauchy root bound for polynomials.

from Poly import :: Poly
from Rational import :: Rational

// Cauchy bound: all roots of p have |z| < cauchyBound p.
cauchyBound :: !(Poly Rational) -> Rational
