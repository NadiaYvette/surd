definition module TragerFactoring

// Factoring over algebraic extensions (stub).

from Poly import :: Poly
from Rational import :: Rational
from Extension import :: ExtElem

// Factor a polynomial over Q(alpha). Stub: returns the input unfactored.
factorOverExtension :: !(Poly Rational) !(Poly Rational) -> [(Poly Rational, Int)]
