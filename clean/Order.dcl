definition module Order

from RadExpr import :: RadExpr
from Rational import :: Rational

// Compare two radical expressions (stub: uses numerical eval).
radCompare :: !(RadExpr Rational) !(RadExpr Rational) -> Int
