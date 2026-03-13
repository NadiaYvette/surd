implementation module Order

import StdEnv
import RadExpr, Rational, Eval

radCompare :: !(RadExpr Rational) !(RadExpr Rational) -> Int
radCompare a b
    # va = eval a
    # vb = eval b
    | va < vb = ~1
    | va > vb = 1
    = 0
