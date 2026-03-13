implementation module Equality

import StdEnv
import RadExpr, Rational

radEqual :: !(RadExpr Rational) !(RadExpr Rational) -> Bool
radEqual a b = a == b
