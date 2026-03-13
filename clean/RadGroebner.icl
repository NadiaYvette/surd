implementation module RadGroebner

import StdEnv
import RadExpr, Rational

simplifyViaGroebner :: !(RadExpr Rational) -> RadExpr Rational
simplifyViaGroebner e = e
