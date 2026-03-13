definition module RadicalTower

// Radical tower construction for solvable quintics.

from RadExpr import :: RadExpr
from Rational import :: Rational
from Poly import :: Poly
from Identify import :: GaloisResult

// Solve a solvable quintic: construct radical expressions for the roots.
solveViaTower :: !GaloisResult !(Poly Rational) -> ?([RadExpr Rational])
