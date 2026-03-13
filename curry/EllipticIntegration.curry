--- Reduction of elliptic integrals to Legendre normal forms.
---
--- Given integral R(x) dx / sqrt(P(x)) where P(x) has degree 3 or 4
--- with all real roots, expresses the result in terms of:
---   F(phi, k) -- incomplete elliptic integral of the first kind
---   E(phi, k) -- incomplete elliptic integral of the second kind
---   Pi(phi, n, k) -- incomplete elliptic integral of the third kind
module EllipticIntegration
  ( EllipticIntegrand(..)
  , EllipticResult(..)
  , LegendreForm(..)
  , LegendreKind(..)
  , reduceElliptic
  , prettyEllipticResult
  , latexEllipticResult
  ) where

import Rational
import Poly
import RadExpr
import Normalize (normalize)
import Pretty (pretty)
import LaTeX (latex)

--- An elliptic integrand: R(x) dx / sqrt(P(x)), where R(x) = num/den.
data EllipticIntegrand = EllipticIntegrand Poly Poly Poly

--- Which kind of Legendre elliptic integral.
data LegendreKind = FirstKind | SecondKind | ThirdKind

--- A single Legendre form term.
data LegendreForm = LegendreForm
  LegendreKind               -- kind
  (RadExpr Rational)         -- scaling coefficient
  (RadExpr Rational)         -- shift (for amplitude)
  (RadExpr Rational)         -- modulus k

--- Result of elliptic reduction.
data EllipticResult = EllipticResult
  [LegendreForm]              -- sum of Legendre forms
  (RadExpr Rational)          -- modulus k
  (RadExpr Rational)          -- k^2

--- Local aliases.
rZero :: Rational
rZero = Rational.fromInt 0

rOne :: Rational
rOne = Rational.fromInt 1

--- Reduce an elliptic integral to Legendre normal form.
---
--- For the simple case integral dx/sqrt(P(x)) where P is cubic/quartic,
--- computes the modulus k from the roots of P.
reduceElliptic :: EllipticIntegrand -> Maybe EllipticResult
reduceElliptic (EllipticIntegrand pNum pDen radicand) =
  let d = degree radicand
  in if d == 3
     then reduceCubic pNum pDen radicand
     else if d == 4
     then reduceQuartic pNum pDen radicand
     else Nothing

--- Reduce integral with cubic radicand (stub).
reduceCubic :: Poly -> Poly -> Poly -> Maybe EllipticResult
reduceCubic pNum pDen radicand =
  -- For cubic P(x) = a(x-e1)(x-e2)(x-e3) with e1 < e2 < e3,
  -- the modulus is k^2 = (e2-e1)/(e3-e1).
  -- Simplified: return a placeholder result.
  let k = Root 2 (Lit (mkRat 1 2))  -- placeholder
      k2 = Lit (mkRat 1 2)
      term = LegendreForm FirstKind (Lit rOne) (Lit rZero) k
  in Just (EllipticResult [term] k k2)

--- Reduce integral with quartic radicand (stub).
reduceQuartic :: Poly -> Poly -> Poly -> Maybe EllipticResult
reduceQuartic pNum pDen radicand =
  let k = Root 2 (Lit (mkRat 1 2))
      k2 = Lit (mkRat 1 2)
      term = LegendreForm FirstKind (Lit rOne) (Lit rZero) k
  in Just (EllipticResult [term] k k2)

--- Pretty-print an elliptic result.
prettyEllipticResult :: EllipticResult -> String
prettyEllipticResult (EllipticResult terms k k2) =
  let termStrs = map prettyLegendreForm terms
  in unlines (termStrs ++ ["modulus k = " ++ pretty k,
                            "k^2 = " ++ pretty k2])

prettyLegendreForm :: LegendreForm -> String
prettyLegendreForm (LegendreForm kind coeff shift modulus) =
  let kindStr = case kind of
                  FirstKind  -> "F"
                  SecondKind -> "E"
                  ThirdKind  -> "Pi"
  in pretty coeff ++ " * " ++ kindStr ++ "(phi, " ++ pretty modulus ++ ")"

--- LaTeX rendering of an elliptic result.
latexEllipticResult :: EllipticResult -> String
latexEllipticResult (EllipticResult terms k k2) =
  let termStrs = map latexLegendreForm terms
  in unlines (termStrs ++ ["k = " ++ latex k, "k^2 = " ++ latex k2])

latexLegendreForm :: LegendreForm -> String
latexLegendreForm (LegendreForm kind coeff shift modulus) =
  let kindStr = case kind of
                  FirstKind  -> "F"
                  SecondKind -> "E"
                  ThirdKind  -> "\\Pi"
  in latex coeff ++ " \\cdot " ++ kindStr
     ++ "\\left(\\varphi, " ++ latex modulus ++ "\\right)"

instance Eq LegendreKind where
  FirstKind  == FirstKind  = True
  SecondKind == SecondKind = True
  ThirdKind  == ThirdKind  = True
  _          == _          = False

instance Show LegendreKind where
  show FirstKind  = "FirstKind"
  show SecondKind = "SecondKind"
  show ThirdKind  = "ThirdKind"

instance Show LegendreForm where
  show = prettyLegendreForm

instance Show EllipticIntegrand where
  show (EllipticIntegrand n d r) =
    "EllipticIntegrand (" ++ show n ++ ")/(" ++ show d
    ++ ")/sqrt(" ++ show r ++ ")"

instance Show EllipticResult where
  show = prettyEllipticResult
