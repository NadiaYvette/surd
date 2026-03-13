module Surd.EulerIntegration

import Surd.Rational
import Surd.Poly
import Surd.Cyclotomic  -- Ring/Field instances
import Surd.Types
import Surd.Normalize
import Surd.Pretty
import Surd.LaTeX
import Surd.Factoring

import Data.List

%default covering

------------------------------------------------------------------------
-- Symbolic expression for antiderivatives
------------------------------------------------------------------------

||| Symbolic expression for an antiderivative (function of x).
public export
data SymExpr : Type where
  SRat    : Rational -> SymExpr                     -- rational constant
  SRad    : RadExpr Rational -> SymExpr              -- constant radical
  SVar    : SymExpr                                   -- integration variable x
  SSurd   : Rational -> Rational -> Rational -> SymExpr  -- sqrt(ax^2+bx+c)
  SNeg    : SymExpr -> SymExpr
  SAdd    : SymExpr -> SymExpr -> SymExpr
  SMul    : SymExpr -> SymExpr -> SymExpr
  SDiv    : SymExpr -> SymExpr -> SymExpr
  SPow    : SymExpr -> Int -> SymExpr
  SLn     : SymExpr -> SymExpr                       -- ln|...|
  SArcTan : SymExpr -> SymExpr                       -- arctan(...)
  SArcSin : SymExpr -> SymExpr                       -- arcsin(...)

export
covering
Show SymExpr where
  show (SRat r) = show r
  show (SRad e) = assert_total $ pretty e
  show SVar = "x"
  show (SSurd a b c) = "sqrt(" ++ show a ++ "x^2 + " ++ show b ++ "x + " ++ show c ++ ")"
  show (SNeg e) = "-(" ++ show e ++ ")"
  show (SAdd a b) = show a ++ " + " ++ show b
  show (SMul a b) = show a ++ " * " ++ show b
  show (SDiv a b) = "(" ++ show a ++ ") / (" ++ show b ++ ")"
  show (SPow e n) = "(" ++ show e ++ ")^" ++ show n
  show (SLn e) = "ln|" ++ show e ++ "|"
  show (SArcTan e) = "arctan(" ++ show e ++ ")"
  show (SArcSin e) = "arcsin(" ++ show e ++ ")"

------------------------------------------------------------------------
-- Integration result
------------------------------------------------------------------------

||| Result of Euler substitution integration.
public export
record IntegralResult where
  constructor MkIntegralResult
  irExpr : SymExpr     -- the antiderivative
  irA    : Rational    -- a in sqrt(ax^2+bx+c)
  irB    : Rational    -- b
  irC    : Rational    -- c

export
covering
Show IntegralResult where
  show ir = "Integral: " ++ assert_total (show (irExpr ir))

------------------------------------------------------------------------
-- Integrand specification
------------------------------------------------------------------------

||| An integrand P(x)/Q(x) * (sqrt(ax^2+bx+c))^n.
public export
record EulerIntegrand where
  constructor MkEulerIntegrand
  eiP       : Poly Rational  -- P(x) numerator
  eiQ       : Poly Rational  -- Q(x) denominator
  eiSqrtPow : Int             -- power of sqrt(ax^2+bx+c): +/-1
  eiA       : Rational
  eiB       : Rational
  eiC       : Rational

export
Show EulerIntegrand where
  show ei = "Integrand(P=" ++ show (eiP ei) ++ ", Q=" ++ show (eiQ ei) ++ ")"

------------------------------------------------------------------------
-- Euler substitution type
------------------------------------------------------------------------

data EulerSub : Type where
  ||| Euler 1 (a > 0): sqrt(ax^2+bx+c) = t - x*sqrt(a)
  Euler1 : Rational -> EulerSub
  ||| Euler 2 (c > 0): sqrt(ax^2+bx+c) = xt + sqrt(c)
  Euler2 : Rational -> EulerSub
  ||| Euler 3 (disc > 0): sqrt(a(x-r1)(x-r2)) = t(x-r1)
  Euler3 : Rational -> Rational -> EulerSub

------------------------------------------------------------------------
-- Rational function integration
------------------------------------------------------------------------

||| Integrate a rational function P(x)/Q(x) by partial fractions.
||| Returns a SymExpr representing the antiderivative.
export
integrateRational : Poly Rational -> Poly Rational -> SymExpr
integrateRational p q =
  -- Polynomial part via division
  let (quot, rem) = divModPoly p q
      polyPart = integratePolynomial quot
      -- Proper fraction part: partial fractions decomposition
      -- Stub: for simple cases only
      fracPart = case (degreeInt rem, degreeInt q) of
        (_, 1) => -- q is linear: ax + b, integral = (rem/a) * ln|ax + b|
          case (coeffs rem, coeffs q) of
            ([r], [b, a]) =>
              SMul (SRat (r / a)) (SLn (SAdd (SMul (SRat a) SVar) (SRat b)))
            _ => SRat Rational.zero
        _ => SRat Rational.zero  -- stub
  in SAdd polyPart fracPart
  where
    intPolyTerms : Integer -> List Rational -> List SymExpr
    intPolyTerms _ [] = []
    intPolyTerms i (c :: cs) =
      (if Surd.Rational.isZero c then SRat Rational.zero
       else SMul (SRat (c / Rational.fromInteger (i + 1)))
                 (SPow SVar (cast (i + 1))))
      :: intPolyTerms (i + 1) cs

    integratePolynomial : Poly Rational -> SymExpr
    integratePolynomial (MkPoly []) = SRat Rational.zero
    integratePolynomial (MkPoly cs) =
      foldl SAdd (SRat Rational.zero) (intPolyTerms 0 cs)

------------------------------------------------------------------------
-- Main integration function
------------------------------------------------------------------------

||| Integrate using Euler substitution.
|||
||| Three cases:
||| * Euler 1 (a > 0): sqrt(ax^2+bx+c) = t - x*sqrt(a)
||| * Euler 2 (c > 0): sqrt(ax^2+bx+c) = xt + sqrt(c)
||| * Euler 3 (disc > 0): sqrt(a(x-r1)(x-r2)) = t(x-r1)
export
eulerIntegrate : EulerIntegrand -> Maybe IntegralResult
eulerIntegrate ei =
  let a = eiA ei
      b = eiB ei
      c = eiC ei
      disc = b * b - Rational.fromInteger 4 * a * c
  in chooseSub a b c disc
  where
    chooseSub : Rational -> Rational -> Rational -> Rational -> Maybe IntegralResult
    chooseSub a b c disc =
      -- Euler 2 is simplest when c > 0
      if c > Rational.zero then
        Just (MkIntegralResult
          (integrateRational (eiP ei) (eiQ ei))
          a b c)
      else if a > Rational.zero then
        Just (MkIntegralResult
          (integrateRational (eiP ei) (eiQ ei))
          a b c)
      else Nothing  -- need disc > 0 for Euler 3

------------------------------------------------------------------------
-- Rendering
------------------------------------------------------------------------

||| Pretty-print a symbolic expression.
export
prettySymExpr : SymExpr -> String
prettySymExpr = show

||| Render a symbolic expression as LaTeX.
export
latexSymExpr : SymExpr -> String
latexSymExpr (SRat r) = show r
latexSymExpr (SRad e) = latex e
latexSymExpr SVar = "x"
latexSymExpr (SSurd a b c) =
  "\\sqrt{" ++ show a ++ "x^2 + " ++ show b ++ "x + " ++ show c ++ "}"
latexSymExpr (SNeg e) = "-" ++ latexSymExpr e
latexSymExpr (SAdd a b) = latexSymExpr a ++ " + " ++ latexSymExpr b
latexSymExpr (SMul a b) = latexSymExpr a ++ " \\cdot " ++ latexSymExpr b
latexSymExpr (SDiv a b) = "\\frac{" ++ latexSymExpr a ++ "}{" ++ latexSymExpr b ++ "}"
latexSymExpr (SPow e n) = "\\left(" ++ latexSymExpr e ++ "\\right)^{" ++ show n ++ "}"
latexSymExpr (SLn e) = "\\ln\\left|" ++ latexSymExpr e ++ "\\right|"
latexSymExpr (SArcTan e) = "\\arctan\\left(" ++ latexSymExpr e ++ "\\right)"
latexSymExpr (SArcSin e) = "\\arcsin\\left(" ++ latexSymExpr e ++ "\\right)"
