definition module TrigGalois

// Gauss period descent for computing roots of unity as radical expressions.

from RadExpr import :: RadExpr
from Rational import :: Rational

// Compute cos(2pi/n) as a radical expression via Gauss period descent.
cosOfUnityViaGauss :: !Int -> ?((RadExpr Rational))

// Compute all primitive nth roots of unity as radical expressions.
// Returns a list of (k, expr) pairs for zeta^k.
allPeriodsViaGauss :: !Int -> ?([(Int, RadExpr Rational)])

// Euler's totient function
eulerTotientI :: !Int -> Int

// Modular exponentiation: base^exp mod m
modExp :: !Int !Int !Int -> Int

// Find a primitive root modulo n (if one exists)
primitiveRootMod :: !Int -> ?(Int)
