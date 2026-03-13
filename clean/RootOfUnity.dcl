definition module RootOfUnity

// Express roots of unity as radical expressions.

from RadExpr import :: RadExpr
from Rational import :: Rational

// Compute cos(2pi/n) as a radical expression.
cosOfUnity :: !Int -> ?((RadExpr Rational))

// Compute sin(2pi/n) as a radical expression.
sinOfUnity :: !Int -> ?((RadExpr Rational))

// Check if cos(2pi/n) is constructible (only square roots needed).
isConstructible :: !Int -> Bool
