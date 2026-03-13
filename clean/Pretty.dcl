definition module Pretty

// Pretty-printing radical expressions in human-readable mathematical notation.
// Includes CSE (common subexpression elimination) for readability.

from RadExpr import :: RadExpr
from Rational import :: Rational

// Render a radical expression as a human-readable string.
pretty :: !(RadExpr Rational) -> {#Char}

// Render with precedence context (0 = top level).
prettyPrec :: !Int !(RadExpr Rational) -> {#Char}

// Render with CSE: repeated subexpressions are shown as named intermediates.
prettyCSE :: !(RadExpr Rational) -> {#Char}
