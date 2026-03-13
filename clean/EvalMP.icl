implementation module EvalMP

import StdEnv
import RadExpr
import Rational
import Eval

// Stub: delegate to Double-precision eval
evalComplexMP :: !Int !(RadExpr Rational) -> (Real, Real)
evalComplexMP _ expr = evalComplex expr

evalRealMP :: !Int !(RadExpr Rational) -> Real
evalRealMP _ expr = eval expr
