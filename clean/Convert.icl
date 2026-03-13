implementation module Convert

import StdEnv
import RadExpr
import Rational
import AlgNum

radExprToAlgNum :: !(RadExpr Rational) -> ?(AlgNum)
radExprToAlgNum _ = ?None  // stub

algNumToRadExpr :: !AlgNum -> ?((RadExpr Rational))
algNumToRadExpr _ = ?None  // stub
