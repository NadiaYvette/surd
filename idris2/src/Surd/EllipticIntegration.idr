module Surd.EllipticIntegration

import Surd.Rational
import Surd.Poly
import Surd.Cyclotomic  -- Ring/Field instances
import Surd.Types
import Surd.Normalize
import Surd.Pretty
import Surd.LaTeX

import Data.Nat
import Data.List

%default covering

------------------------------------------------------------------------
-- Legendre forms
------------------------------------------------------------------------

||| Which kind of Legendre elliptic integral.
public export
data LegendreKind : Type where
  FirstKind  : LegendreKind
  SecondKind : LegendreKind
  ThirdKind  : LegendreKind

export
Eq LegendreKind where
  FirstKind == FirstKind = True
  SecondKind == SecondKind = True
  ThirdKind == ThirdKind = True
  _ == _ = False

export
Show LegendreKind where
  show FirstKind = "F"
  show SecondKind = "E"
  show ThirdKind = "Pi"

||| A single Legendre form term: coeff * F/E/Pi(phi, [n,] k).
public export
record LegendreForm where
  constructor MkLegendreForm
  lfKind  : LegendreKind
  lfCoeff : RadExpr Rational    -- multiplicative coefficient
  lfPhi   : RadExpr Rational    -- amplitude phi
  lfK     : RadExpr Rational    -- modulus k
  lfN     : Maybe (RadExpr Rational)  -- characteristic n (ThirdKind only)

export
Show LegendreForm where
  show lf = show (lfCoeff lf) ++ " * " ++ show (lfKind lf)
            ++ "(" ++ show (lfPhi lf) ++ ", " ++ show (lfK lf) ++ ")"

------------------------------------------------------------------------
-- Elliptic integrand
------------------------------------------------------------------------

||| An elliptic integrand: R(x) dx / sqrt(P(x)), where deg(P) = 3 or 4.
public export
record EllipticIntegrand where
  constructor MkEllipticIntegrand
  eiNum      : Poly Rational   -- R(x) numerator
  eiDen      : Poly Rational   -- R(x) denominator
  eiRadicand : Poly Rational   -- P(x), degree 3 or 4

export
Show EllipticIntegrand where
  show ei = "EllipticIntegrand(radicand=" ++ show (eiRadicand ei) ++ ")"

------------------------------------------------------------------------
-- Elliptic result
------------------------------------------------------------------------

||| Result of elliptic integral reduction.
public export
record EllipticResult where
  constructor MkEllipticResult
  erTerms    : List LegendreForm    -- sum of Legendre form terms
  erModulus  : RadExpr Rational      -- modulus k as radical expression
  erRational : Maybe (RadExpr Rational)  -- rational part (if any)

export
Show EllipticResult where
  show er = "EllipticResult(" ++ show (length (erTerms er)) ++ " terms, k=" ++ show (erModulus er) ++ ")"

------------------------------------------------------------------------
-- Reduction to Legendre forms
------------------------------------------------------------------------

||| Reduce an elliptic integral to Legendre normal forms.
|||
||| Given integral R(x) dx / sqrt(P(x)) where P has degree 3 or 4
||| with all real roots, expresses the result in terms of
||| F(phi, k), E(phi, k), Pi(phi, n, k).
|||
||| The modulus k is computed from the cross-ratio of the roots.
export
reduceElliptic : EllipticIntegrand -> Maybe EllipticResult
reduceElliptic ei =
  let d = degreeInt (eiRadicand ei)
  in if d /= 3 && d /= 4 then Nothing
     else
       -- For a full implementation:
       -- 1. Find roots of P(x) (algebraic numbers)
       -- 2. Order roots on the real line
       -- 3. Compute modulus k from cross-ratio
       -- 4. Apply the Moebius transformation to standard form
       -- 5. Decompose R(x) into elementary parts
       --
       -- Stub: return a basic first-kind integral
       let k = Root 2 (Lit (mkRat 1 2))  -- placeholder modulus
           phi = Lit Rational.one          -- placeholder amplitude
           term = MkLegendreForm FirstKind (Lit Rational.one) phi k Nothing
       in Just (MkEllipticResult [term] k Nothing)

------------------------------------------------------------------------
-- Rendering
------------------------------------------------------------------------

||| Pretty-print a Legendre form.
prettyLegendreForm : LegendreForm -> String
prettyLegendreForm lf =
  let coeffStr = pretty (lfCoeff lf)
      kindStr = show (lfKind lf)
      phiStr = pretty (lfPhi lf)
      kStr = pretty (lfK lf)
      args = case lfN lf of
               Nothing => phiStr ++ ", " ++ kStr
               Just n => phiStr ++ ", " ++ pretty n ++ ", " ++ kStr
  in coeffStr ++ " * " ++ kindStr ++ "(" ++ args ++ ")"

joinPlus : List String -> String
joinPlus [] = "0"
joinPlus [x] = x
joinPlus (x :: xs) = foldl (\a, b => a ++ " + " ++ b) x xs

||| Pretty-print an elliptic integral result.
export
prettyEllipticResult : EllipticResult -> String
prettyEllipticResult er =
  let termStrs = map prettyLegendreForm (erTerms er)
  in joinPlus termStrs ++ " + C"

latexLegendreForm : LegendreForm -> String
latexLegendreForm lf =
  let coeffStr = latex (lfCoeff lf)
      kindStr = case lfKind lf of
                  FirstKind => "F"
                  SecondKind => "E"
                  ThirdKind => "\\Pi"
      phiStr = latex (lfPhi lf)
      kStr = latex (lfK lf)
  in coeffStr ++ " \\cdot " ++ kindStr ++ "\\left(" ++ phiStr ++ ", " ++ kStr ++ "\\right)"

||| Render an elliptic integral result as LaTeX.
export
latexEllipticResult : EllipticResult -> String
latexEllipticResult er =
  let termStrs = map latexLegendreForm (erTerms er)
  in joinPlus termStrs ++ " + C"
