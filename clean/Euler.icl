implementation module Euler

import StdEnv
import RadExpr
import Rational
import Poly
import Data.Integer

// ─── Euler substitution integration ───
// Three substitutions:
// Euler 1 (a > 0): sqrt(ax^2+bx+c) = t - x*sqrt(a)
// Euler 2 (c > 0): sqrt(ax^2+bx+c) = x*t + sqrt(c)
// Euler 3 (disc > 0): sqrt(a*(x-r1)*(x-r2)) = t*(x-r1)

eulerIntegrate :: !EulerIntegrand -> ?(IntegralResult)
eulerIntegrate ei
    // Try Euler 1 first (a > 0)
    | ei.eiA > zero && isSquare ei.eiA
        # sqrtA = intSqrt (toInt (numer ei.eiA)) (toInt (denom ei.eiA))
        = ?Just { irExpr = eulerResult1 ei sqrtA
                , irA = ei.eiA, irB = ei.eiB, irC = ei.eiC }
    // Try Euler 2 (c > 0)
    | ei.eiC > zero && isSquare ei.eiC
        # sqrtC = intSqrt (toInt (numer ei.eiC)) (toInt (denom ei.eiC))
        = ?Just { irExpr = eulerResult2 ei sqrtC
                , irA = ei.eiA, irB = ei.eiB, irC = ei.eiC }
    // Try Euler 3 (discriminant > 0, has rational roots)
    | discriminant ei > zero
        = case findRationalRoot ei of
            ?Just (r1, r2) ->
                ?Just { irExpr = eulerResult3 ei r1 r2
                      , irA = ei.eiA, irB = ei.eiB, irC = ei.eiC }
            ?None -> ?None
    = ?None

discriminant :: !EulerIntegrand -> Rational
discriminant ei = ei.eiB * ei.eiB - ratFromInt 4 * ei.eiA * ei.eiC

isSquare :: !Rational -> Bool
isSquare r
    | r < zero = False
    | r == zero = True
    # n = toInt (numer r)
    # d = toInt (denom r)
    # sn = toInt (sqrt (toReal (abs n)))
    # sd = toInt (sqrt (toReal (abs d)))
    = sn * sn == abs n && sd * sd == abs d

intSqrt :: !Int !Int -> Rational
intSqrt n d
    # sn = toInt (sqrt (toReal (abs n)))
    # sd = toInt (sqrt (toReal (abs d)))
    = mkRational (toInteger sn) (toInteger sd)

findRationalRoot :: !EulerIntegrand -> ?((Rational, Rational))
findRationalRoot ei
    # disc = discriminant ei
    | disc < zero = ?None
    | ei.eiA == zero = ?None
    # discD = toReal (toInt (numer disc)) / toReal (toInt (denom disc))
    # sqrtDisc = sqrt discD
    # r1cand = (~ (toReal (toInt (numer ei.eiB))) / toReal (toInt (denom ei.eiB)) + sqrtDisc) /
               (2.0 * toReal (toInt (numer ei.eiA)) / toReal (toInt (denom ei.eiA)))
    # r2cand = (~ (toReal (toInt (numer ei.eiB))) / toReal (toInt (denom ei.eiB)) - sqrtDisc) /
               (2.0 * toReal (toInt (numer ei.eiA)) / toReal (toInt (denom ei.eiA)))
    # r1 = approxRat r1cand
    # r2 = approxRat r2cand
    // Verify
    | ei.eiA * r1 * r1 + ei.eiB * r1 + ei.eiC == zero &&
      ei.eiA * r2 * r2 + ei.eiB * r2 + ei.eiC == zero
        = ?Just (r1, r2)
    = ?None

approxRat :: !Real -> Rational
approxRat x
    # candidates = [(abs (toReal n / toReal d - x), mkRational (toInteger n) (toInteger d))
                    \\ d <- [1..1000], let n = toInt (x * toReal d + if (x >= 0.0) 0.5 (~0.5))
                    | abs (toReal n / toReal d - x) < 0.000001]
    | isEmpty candidates = ratFromInt (toInt (if (x >= 0.0) (x + 0.5) (x - 0.5)))
    = snd (hd (sortBy (\(a,_) (b,_) -> a < b) candidates))

// Simplified Euler results (symbolic form)
eulerResult1 :: !EulerIntegrand !Rational -> SymExpr
eulerResult1 ei sqrtA
    // For integral dx/sqrt(ax^2+bx+c) with a > 0:
    // = (1/sqrt(a)) * ln|2*sqrt(a)*sqrt(ax^2+bx+c) + 2*a*x + b|
    | ei.eiSqrtPow == (~1) && degree ei.eiP == 0 && degree ei.eiQ == 0
        = SDiv (SLn (SAdd (SMul (SRat (ratFromInt 2 * sqrtA)) (SSurd ei.eiA ei.eiB ei.eiC))
                          (SAdd (SMul (SRat (ratFromInt 2 * ei.eiA)) SVar) (SRat ei.eiB))))
               (SRat sqrtA)
    = SLn (SAdd SVar (SSurd ei.eiA ei.eiB ei.eiC))

eulerResult2 :: !EulerIntegrand !Rational -> SymExpr
eulerResult2 ei sqrtC
    // For c > 0, similar structure
    | ei.eiSqrtPow == (~1) && ei.eiA < zero
        = SArcSin (SDiv (SAdd (SMul (SRat ei.eiB) SVar) (SRat (ratFromInt 2 * ei.eiC)))
                        (SMul SVar (SRat (ratFromInt 2 * sqrtC))))
    = SLn (SAdd SVar (SSurd ei.eiA ei.eiB ei.eiC))

eulerResult3 :: !EulerIntegrand !Rational !Rational -> SymExpr
eulerResult3 _ r1 _  = SLn (SAdd SVar (SRat (~ r1)))

// Rational function integration (simplified)
integrateRational :: !(Poly Rational) !(Poly Rational) -> SymExpr
integrateRational _ _ = SLn SVar  // stub

// ─── Pretty-printing ───
prettySymExpr :: !SymExpr -> {#Char}
prettySymExpr (SRat r) = toString r
prettySymExpr SVar = "x"
prettySymExpr (SSurd a b c) = "sqrt(" +++ toString a +++ "x^2 + " +++ toString b +++ "x + " +++ toString c +++ ")"
prettySymExpr (SNeg e) = "-(" +++ prettySymExpr e +++ ")"
prettySymExpr (SAdd a b) = "(" +++ prettySymExpr a +++ " + " +++ prettySymExpr b +++ ")"
prettySymExpr (SMul a b) = prettySymExpr a +++ " * " +++ prettySymExpr b
prettySymExpr (SDiv a b) = "(" +++ prettySymExpr a +++ " / " +++ prettySymExpr b +++ ")"
prettySymExpr (SPow a n) = prettySymExpr a +++ "^" +++ toString n
prettySymExpr (SLn a) = "ln|" +++ prettySymExpr a +++ "|"
prettySymExpr (SArcTan a) = "arctan(" +++ prettySymExpr a +++ ")"
prettySymExpr (SArcSin a) = "arcsin(" +++ prettySymExpr a +++ ")"
prettySymExpr (SRad e) = toString e

latexSymExpr :: !SymExpr -> {#Char}
latexSymExpr (SRat r) = toString r
latexSymExpr SVar = "x"
latexSymExpr (SSurd a b c) = "\\sqrt{" +++ toString a +++ "x^2 + " +++ toString b +++ "x + " +++ toString c +++ "}"
latexSymExpr (SNeg e) = "-" +++ latexSymExpr e
latexSymExpr (SAdd a b) = latexSymExpr a +++ " + " +++ latexSymExpr b
latexSymExpr (SMul a b) = latexSymExpr a +++ " \\cdot " +++ latexSymExpr b
latexSymExpr (SDiv a b) = "\\frac{" +++ latexSymExpr a +++ "}{" +++ latexSymExpr b +++ "}"
latexSymExpr (SPow a n) = latexSymExpr a +++ "^{" +++ toString n +++ "}"
latexSymExpr (SLn a) = "\\ln|" +++ latexSymExpr a +++ "|"
latexSymExpr (SArcTan a) = "\\arctan(" +++ latexSymExpr a +++ ")"
latexSymExpr (SArcSin a) = "\\arcsin(" +++ latexSymExpr a +++ ")"
latexSymExpr (SRad e) = toString e
