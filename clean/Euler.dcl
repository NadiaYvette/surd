definition module Euler

// Euler substitution for integrals of the form
// integral P(x)/Q(x) * (sqrt(ax^2+bx+c))^n dx.

from RadExpr import :: RadExpr
from Rational import :: Rational
from Poly import :: Poly

// Symbolic expression for an antiderivative.
:: SymExpr
    = SRat !Rational
    | SRad !(RadExpr Rational)
    | SVar
    | SSurd !Rational !Rational !Rational  // sqrt(ax^2+bx+c)
    | SNeg !SymExpr
    | SAdd !SymExpr !SymExpr
    | SMul !SymExpr !SymExpr
    | SDiv !SymExpr !SymExpr
    | SPow !SymExpr !Int
    | SLn  !SymExpr
    | SArcTan !SymExpr
    | SArcSin !SymExpr

:: IntegralResult = { irExpr :: !SymExpr, irA :: !Rational, irB :: !Rational, irC :: !Rational }

:: EulerIntegrand = { eiP :: !(Poly Rational), eiQ :: !(Poly Rational), eiSqrtPow :: !Int, eiA :: !Rational, eiB :: !Rational, eiC :: !Rational }

// Perform Euler substitution integration.
eulerIntegrate :: !EulerIntegrand -> ?(IntegralResult)

// Integrate a rational function P/Q.
integrateRational :: !(Poly Rational) !(Poly Rational) -> SymExpr

// Pretty-print a symbolic expression.
prettySymExpr :: !SymExpr -> {#Char}

// LaTeX render a symbolic expression.
latexSymExpr :: !SymExpr -> {#Char}
