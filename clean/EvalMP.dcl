definition module EvalMP

// Arbitrary-precision evaluation (stubbed with Double eval).

from RadExpr import :: RadExpr
from Rational import :: Rational

// Evaluate to complex with Double precision (stub for MPBall).
evalComplexMP :: !Int !(RadExpr Rational) -> (Real, Real)

// Evaluate to real with Double precision (stub for MPBall).
evalRealMP :: !Int !(RadExpr Rational) -> Real
