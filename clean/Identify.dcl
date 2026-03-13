definition module Identify

// Galois group identification for degree-5 polynomials.

from Poly import :: Poly
from Rational import :: Rational
from TransitiveGroup import :: TransitiveGroup

:: GaloisResult = { grGroup :: !TransitiveGroup, grRoots :: ![(Real, Real)] }

// Identify the Galois group of an irreducible degree-5 polynomial.
identifyGaloisGroup5 :: !(Poly Rational) -> ?(GaloisResult)
