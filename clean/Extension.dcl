definition module Extension

// Field extension K(alpha) arithmetic.
// Elements of K(alpha) are represented as polynomials in alpha,
// reduced modulo the minimal polynomial of alpha.

from Poly import :: Poly
from Rational import :: Rational

// An element of Q(alpha) = Q[x]/(minpoly)
:: ExtElem = { extPoly :: !(Poly Rational), extMinPoly :: !(Poly Rational) }

// Construct an extension element from a polynomial and minimal polynomial.
mkExtElem :: !(Poly Rational) !(Poly Rational) -> ExtElem

// The element alpha in Q(alpha)
extAlpha :: !(Poly Rational) -> ExtElem

// Arithmetic
extAdd :: !ExtElem !ExtElem -> ExtElem
extSub :: !ExtElem !ExtElem -> ExtElem
extMul :: !ExtElem !ExtElem -> ExtElem
extInv :: !ExtElem -> ExtElem
extNeg :: !ExtElem -> ExtElem
extFromRat :: !Rational !(Poly Rational) -> ExtElem

// Extended GCD for polynomials: returns (g, s, t) with g = s*a + t*b
extGcdPoly :: !(Poly Rational) !(Poly Rational) -> (Poly Rational, Poly Rational, Poly Rational)

instance == ExtElem
instance toString ExtElem

// Algebraic structure instances (see Algebra module).
from Algebra import class Ring, class Field
