definition module Convert

// Conversion between RadExpr and AlgNum.

from RadExpr import :: RadExpr
from Rational import :: Rational
from AlgNum import :: AlgNum

// Convert a radical expression to an algebraic number (stub).
radExprToAlgNum :: !(RadExpr Rational) -> ?(AlgNum)

// Convert an algebraic number to a radical expression (stub).
algNumToRadExpr :: !AlgNum -> ?((RadExpr Rational))
