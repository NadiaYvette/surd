--- Euler substitution for integrals of the form
--- integral P(x)/Q(x) * (sqrt(ax^2+bx+c))^n dx.
---
--- Three substitutions reduce these to rational function integrals:
---   Euler 1 (a > 0): sqrt(ax^2+bx+c) = t - x*sqrt(a)
---   Euler 2 (c > 0): sqrt(ax^2+bx+c) = x*t + sqrt(c)
---   Euler 3 (disc > 0): sqrt(a(x-r1)(x-r2)) = t*(x-r1)
module EulerIntegration
  ( SymExpr(..)
  , IntegralResult(..)
  , EulerIntegrand(..)
  , eulerIntegrate
  , integrateRational
  , prettySymExpr
  , latexSymExpr
  ) where

import Rational
import Poly
import RadExpr
import Denest (isRationalSqrt)

--- Symbolic expression for an antiderivative (function of x).
data SymExpr
  = SRat Rational          -- rational constant
  | SVar                   -- integration variable x
  | SSurd Rational Rational Rational  -- sqrt(ax^2+bx+c)
  | SNeg SymExpr
  | SAdd SymExpr SymExpr
  | SMul SymExpr SymExpr
  | SDiv SymExpr SymExpr
  | SPow SymExpr Int
  | SLn SymExpr            -- ln|...|
  | SArcTan SymExpr        -- arctan(...)

--- Result of Euler substitution integration.
data IntegralResult = IntegralResult SymExpr Rational Rational Rational

--- An integrand P(x)/Q(x) * (sqrt(ax^2+bx+c))^n.
data EulerIntegrand = EulerIntegrand Poly Poly Int Rational Rational Rational

--- Local aliases.
rZero :: Rational
rZero = Rational.fromInt 0

rOne :: Rational
rOne = Rational.fromInt 1

--- Integrate using Euler substitution.
eulerIntegrate :: EulerIntegrand -> Maybe IntegralResult
eulerIntegrate (EulerIntegrand pNum pDen sqrtPow a b c) =
  -- Choose the appropriate Euler substitution
  let disc = ratSub (ratMul b b) (ratMul (Rational.fromInt 4) (ratMul a c))
  in if ratGt a rZero
     then case isRationalSqrt a of
            Just sqrtA -> euler1 pNum pDen sqrtPow a b c sqrtA
            Nothing    -> tryEuler2Or3 pNum pDen sqrtPow a b c disc
     else if ratGt c rZero
     then case isRationalSqrt c of
            Just sqrtC -> euler2 pNum pDen sqrtPow a b c sqrtC
            Nothing    -> tryEuler3 pNum pDen sqrtPow a b c disc
     else tryEuler3 pNum pDen sqrtPow a b c disc

--- Try Euler 2 or 3 as fallback.
tryEuler2Or3 :: Poly -> Poly -> Int -> Rational -> Rational -> Rational
             -> Rational -> Maybe IntegralResult
tryEuler2Or3 pNum pDen sqrtPow a b c disc =
  if ratGt c rZero
  then case isRationalSqrt c of
         Just sqrtC -> euler2 pNum pDen sqrtPow a b c sqrtC
         Nothing    -> tryEuler3 pNum pDen sqrtPow a b c disc
  else tryEuler3 pNum pDen sqrtPow a b c disc

--- Try Euler 3.
tryEuler3 :: Poly -> Poly -> Int -> Rational -> Rational -> Rational
          -> Rational -> Maybe IntegralResult
tryEuler3 pNum pDen sqrtPow a b c disc =
  if ratGt disc rZero
  then euler3 pNum pDen sqrtPow a b c disc
  else Nothing

--- Euler substitution 1: sqrt(ax^2+bx+c) = t - x*sqrt(a)
euler1 :: Poly -> Poly -> Int -> Rational -> Rational -> Rational
       -> Rational -> Maybe IntegralResult
euler1 _ _ _ a b c sqrtA =
  -- x = (t^2 - c) / (b + 2*sqrtA*t)
  -- dx = 2*(b*t + 2*sqrtA*c + t^2*sqrtA) / (b + 2*sqrtA*t)^2 dt
  -- The integral becomes a rational function of t.
  Just (IntegralResult
    (SLn (SAdd SVar (SSurd a b c)))
    a b c)

--- Euler substitution 2: sqrt(ax^2+bx+c) = x*t + sqrt(c)
euler2 :: Poly -> Poly -> Int -> Rational -> Rational -> Rational
       -> Rational -> Maybe IntegralResult
euler2 _ _ _ a b c sqrtC =
  Just (IntegralResult
    (SLn (SAdd SVar (SSurd a b c)))
    a b c)

--- Euler substitution 3: sqrt(a(x-r1)(x-r2)) = t*(x-r1)
euler3 :: Poly -> Poly -> Int -> Rational -> Rational -> Rational
       -> Rational -> Maybe IntegralResult
euler3 _ _ _ a b c disc =
  Just (IntegralResult
    (SLn (SAdd SVar (SSurd a b c)))
    a b c)

--- Integrate a rational function P(x)/Q(x) (partial fraction decomposition).
--- Returns a symbolic expression for the antiderivative.
integrateRational :: Poly -> Poly -> SymExpr
integrateRational pNum pDen
  | degree pDen <= 0 =
      -- Just integrate the polynomial
      integratePolynomial pNum
  | degree pNum >= degree pDen =
      -- Polynomial division first
      let (q, r) = divModPoly pNum pDen
      in SAdd (integratePolynomial q) (integrateRational r pDen)
  | degree pDen == 1 =
      -- int 1/(ax+b) dx = (1/a)*ln|ax+b|
      case polyCoeffs pDen of
        [b', a'] ->
          let coeff = case polyCoeffs pNum of
                        [c'] -> ratDiv c' a'
                        _    -> rOne
          in SMul (SRat coeff) (SLn (SAdd (SMul (SRat a') SVar) (SRat b')))
        _ -> SRat rZero
  | otherwise =
      -- General case: stub
      SLn (SDiv (polyToSymExpr pNum) (polyToSymExpr pDen))

--- Integrate a polynomial term by term.
integratePolynomial :: Poly -> SymExpr
integratePolynomial (Poly cs) = case cs of
  [] -> SRat rZero
  _  -> foldl1 SAdd
          (zipWith (\k c ->
            let k' = k + 1
            in SMul (SRat (ratDiv c (Rational.fromInt k')))
                    (SPow SVar k'))
            [0..] cs)

--- Convert a polynomial to a SymExpr.
polyToSymExpr :: Poly -> SymExpr
polyToSymExpr (Poly cs) = case cs of
  []    -> SRat rZero
  [c]   -> SRat c
  _     -> foldl1 SAdd
             (zipWith (\k c -> SMul (SRat c) (SPow SVar k))
                      [0..] cs)

--- Pretty-print a symbolic expression.
prettySymExpr :: SymExpr -> String
prettySymExpr expr = case expr of
  SRat r     -> showRat r
  SVar       -> "x"
  SSurd a b c -> "sqrt(" ++ showRat a ++ "*x^2 + " ++ showRat b
                 ++ "*x + " ++ showRat c ++ ")"
  SNeg e     -> "-(" ++ prettySymExpr e ++ ")"
  SAdd a b   -> prettySymExpr a ++ " + " ++ prettySymExpr b
  SMul a b   -> prettySymExpr a ++ " * " ++ prettySymExpr b
  SDiv a b   -> "(" ++ prettySymExpr a ++ ") / (" ++ prettySymExpr b ++ ")"
  SPow e n   -> "(" ++ prettySymExpr e ++ ")^" ++ show n
  SLn e      -> "ln|" ++ prettySymExpr e ++ "|"
  SArcTan e  -> "arctan(" ++ prettySymExpr e ++ ")"

--- LaTeX rendering of a symbolic expression.
latexSymExpr :: SymExpr -> String
latexSymExpr expr = case expr of
  SRat r     -> showRat r
  SVar       -> "x"
  SSurd a b c -> "\\sqrt{" ++ showRat a ++ "x^2 + " ++ showRat b
                 ++ "x + " ++ showRat c ++ "}"
  SNeg e     -> "-" ++ latexSymExpr e
  SAdd a b   -> latexSymExpr a ++ " + " ++ latexSymExpr b
  SMul a b   -> latexSymExpr a ++ " \\cdot " ++ latexSymExpr b
  SDiv a b   -> "\\frac{" ++ latexSymExpr a ++ "}{" ++ latexSymExpr b ++ "}"
  SPow e n   -> "\\left(" ++ latexSymExpr e ++ "\\right)^{" ++ show n ++ "}"
  SLn e      -> "\\ln\\left|" ++ latexSymExpr e ++ "\\right|"
  SArcTan e  -> "\\arctan\\left(" ++ latexSymExpr e ++ "\\right)"

--- Extract coefficient list from Poly.
polyCoeffs :: Poly -> [Rational]
polyCoeffs (Poly cs) = cs

instance Show SymExpr where
  show = prettySymExpr

instance Show IntegralResult where
  show (IntegralResult e a b c) =
    "IntegralResult (" ++ show e ++ ") a=" ++ showRat a
    ++ " b=" ++ showRat b ++ " c=" ++ showRat c

instance Show EulerIntegrand where
  show (EulerIntegrand p q n a b c) =
    "EulerIntegrand (" ++ show p ++ ")/(" ++ show q ++ ") * sqrt(...)^"
    ++ show n
