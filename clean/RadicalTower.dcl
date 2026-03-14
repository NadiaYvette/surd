definition module RadicalTower

// Radical tower construction for solvable polynomials.
//
// Given an irreducible polynomial f(x) ∈ Q[x] of prime degree n with
// solvable Galois group G, constructs radical expressions for its roots
// via Lagrange resolvents descending through the composition series of G.
//
// Degree 5 uses an optimised fast path; other prime degrees use a
// generalised n-step pipeline with ω_n = e^{2πi/n}.

from RadExpr import :: RadExpr
from Rational import :: Rational
from Poly import :: Poly
from Identify import :: GaloisResult

// Solve a solvable quintic: construct radical expressions for the roots.
solveViaTower :: !GaloisResult !(Poly Rational) -> ?([RadExpr Rational])

// Solve a solvable polynomial of any prime degree.
// Delegates to solveViaTower for degree 5; uses the generalised
// Lagrange resolvent pipeline for other prime degrees.
solveViaTowerN :: !GaloisResult !(Poly Rational) -> ?([RadExpr Rational])
