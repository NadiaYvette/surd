definition module Elliptic

// Elliptic integral reduction to Legendre normal forms.

from RadExpr import :: RadExpr
from Rational import :: Rational
from Poly import :: Poly

:: LegendreKind = FirstKind | SecondKind | ThirdKind

:: EllipticIntegrand = { eiNum :: !(Poly Rational), eiDen :: !(Poly Rational), eiRadicand :: !(Poly Rational) }

:: EllipticResult = { erModulus :: !(RadExpr Rational), erModulusSq :: !(RadExpr Rational), erRoots :: ![Rational], erDescription :: !{#Char} }

// Reduce an elliptic integral to Legendre normal form.
reduceElliptic :: !Bool !EllipticIntegrand -> ?(EllipticResult)

// Pretty-print an elliptic result.
prettyEllipticResult :: !EllipticResult -> {#Char}

// LaTeX-render an elliptic result.
latexEllipticResult :: !EllipticResult -> {#Char}
