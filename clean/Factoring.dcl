definition module Factoring

// Polynomial factoring over Q.

from Poly import :: Poly
from Rational import :: Rational

// Find rational roots of a polynomial (rational root theorem).
rationalRoots :: !(Poly Rational) -> [Rational]

// Test whether a polynomial has a rational root.
hasRationalRoot :: !(Poly Rational) -> Bool

// Factor a polynomial over Q into irreducible factors.
// Returns a list of (factor, multiplicity) pairs.
factor :: !(Poly Rational) -> [(Poly Rational, Int)]
