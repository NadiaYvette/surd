definition module GaloisSolve

// Top-level Galois solver: identify the Galois group and solve in
// radicals if possible.  Routes all supported degrees (primes >= 3).

from RadExpr import :: RadExpr
from Rational import :: Rational
from Poly import :: Poly

// Solve a polynomial in radicals if possible.
// Supports prime degrees >= 3.  For degree 5, uses the optimised
// sextic resolvent path; for other primes, uses generalised
// Frobenius/Chebotarev identification and Lagrange resolvent descent.
// Returns ?None for unsupported degrees (composites other than 4),
// non-solvable Galois groups, or identification failure.
solveInRadicals :: !(Poly Rational) -> ?([RadExpr Rational])

// Identify the Galois group and solve, returning the group name too.
identifyAndSolve :: !(Poly Rational) -> ?({#Char}, [RadExpr Rational])
