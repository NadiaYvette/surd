definition module GaloisSolve

// Top-level Galois solver (stub).

from RadExpr import :: RadExpr
from Rational import :: Rational
from Poly import :: Poly

// Solve a polynomial in radicals if possible.
solveInRadicals :: !(Poly Rational) -> ?([RadExpr Rational])
