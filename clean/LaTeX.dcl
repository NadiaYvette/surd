definition module LaTeX

// LaTeX math-mode rendering of radical expressions.

from RadExpr import :: RadExpr
from Rational import :: Rational

// Render a radical expression as a LaTeX math-mode string.
latex :: !(RadExpr Rational) -> {#Char}

// Render with precedence context.
latexPrec :: !Int !(RadExpr Rational) -> {#Char}
