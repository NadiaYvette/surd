definition module Identify

// Galois group identification for irreducible polynomials over Q.
//
// For degree 5, uses the optimised sextic resolvent + discriminant test.
// For other prime degrees, uses generalized Frobenius/Chebotarev descent
// through the AGL(1,p) lattice.

from Poly import :: Poly
from Rational import :: Rational
from TransitiveGroup import :: TransitiveGroup

:: GaloisResult = { grGroup :: !TransitiveGroup, grRoots :: ![(Real, Real)] }

// Identify the Galois group of an irreducible degree-5 polynomial.
identifyGaloisGroup5 :: !(Poly Rational) -> ?(GaloisResult)

// Identify the Galois group of an irreducible polynomial of any
// supported degree.  Delegates to identifyGaloisGroup5 for degree 5;
// uses Frobenius/Chebotarev patterns for other prime degrees.
// Returns ?None for unsupported (composite) degrees.
identifyGaloisGroup :: !(Poly Rational) -> ?(GaloisResult)
