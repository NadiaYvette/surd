definition module Resolvent

// Resolvent polynomials for Galois group computation.

from Poly import :: Poly
from Rational import :: Rational

// Approximate all complex roots via Aberth-Ehrlich iteration.
complexRootsOf :: !(Poly Rational) -> [(Real, Real)]

// Test whether a polynomial has a rational root.
hasRationalRoot :: !(Poly Rational) -> Bool

// Test whether a rational number is a perfect square.
isSquareRational :: !Rational -> Bool

// Compute the discriminant of a polynomial.
discriminantOf :: !(Poly Rational) -> Rational

// Complex arithmetic helpers
cadd :: !(Real, Real) !(Real, Real) -> (Real, Real)
csub :: !(Real, Real) !(Real, Real) -> (Real, Real)
cmulR :: !(Real, Real) !(Real, Real) -> (Real, Real)
cmagR :: !(Real, Real) -> Real

// Permutations
perms :: ![a] -> [[a]]
