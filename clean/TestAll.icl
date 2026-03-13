module TestAll

import StdEnv, StdMaybe
import Data.Integer
import Rational
import Positive
import PrimeFactors
import Poly
import RadExpr
import Interval

// Test harness
check :: {#Char} Bool -> ({#Char}, Bool)
check name cond = (name, cond)

showResult :: ({#Char}, Bool) -> {#Char}
showResult (name, True) = "  PASS: " +++ name
showResult (name, False) = "  FAIL: " +++ name

isNothing :: (Maybe a) -> Bool
isNothing Nothing = True
isNothing _ = False

// Polynomial helpers defined at top level
poly_1_2x = mkPoly [ratFromInt 1, ratFromInt 2]
poly_3_4x = mkPoly [ratFromInt 3, ratFromInt 4]
poly_4_6x = mkPoly [ratFromInt 4, ratFromInt 6]
poly_1_x = mkPoly [ratFromInt 1, ratFromInt 1]
poly_m1_x = mkPoly [ratFromInt (~1), ratFromInt 1]
poly_m1_0_x2 = mkPoly [ratFromInt (~1), ratFromInt 0, ratFromInt 1]
poly_1_0_3x2 = mkPoly [ratFromInt 1, ratFromInt 0, ratFromInt 3]
poly_0_6x = mkPoly [ratFromInt 0, ratFromInt 6]
poly_3_6x = mkPoly [ratFromInt 3, ratFromInt 6]
poly_half_1 = mkPoly [ratFromInt 1 / ratFromInt 2, ratFromInt 1]
poly_m1_m2 = mkPoly [ratFromInt (~1), ratFromInt (~2)]

// Interval helpers
iv_1_2 = mkInterval (ratFromInt 1) (ratFromInt 2)
iv_3_4 = mkInterval (ratFromInt 3) (ratFromInt 4)
iv_4_6 = mkInterval (ratFromInt 4) (ratFromInt 6)
iv_3_5 = mkInterval (ratFromInt 3) (ratFromInt 5)
iv_1_4 = mkInterval (ratFromInt 1) (ratFromInt 4)
iv_2_3 = mkInterval (ratFromInt 2) (ratFromInt 3)
iv_4_5 = mkInterval (ratFromInt 4) (ratFromInt 5)
iv_8_15 = mkInterval (ratFromInt 8) (ratFromInt 15)
iv_1_5 = mkInterval (ratFromInt 1) (ratFromInt 5)
iv_3_6 = mkInterval (ratFromInt 3) (ratFromInt 6)
iv_m1_5 = mkInterval (ratFromInt (~1)) (ratFromInt 5)
iv_m5_m1 = mkInterval (ratFromInt (~5)) (ratFromInt (~1))
iv_m1_1 = mkInterval (ratFromInt (~1)) (ratFromInt 1)
iv_2_8 = mkInterval (ratFromInt 2) (ratFromInt 8)
iv_2_4 = mkInterval (ratFromInt 2) (ratFromInt 4)
iv_2_5 = mkInterval (ratFromInt 2) (ratFromInt 5)
iv_m5_m2 = mkInterval (ratFromInt (~5)) (ratFromInt (~2))
iv_0_10 = mkInterval (ratFromInt 0) (ratFromInt 10)

Start = (header, map showResult tests, summary)
where
    header = "=== Surd Clean Test Suite ==="

    tests = rationalTests ++ positiveTests ++ primeTests ++ polyTests ++ radExprTests ++ intervalTests

    npass = length (filter snd tests)
    ntotal = length tests
    summary = toString npass +++ "/" +++ toString ntotal +++ " tests passed"

    // ---- Rational tests ----
    rationalTests =
        [ check "rat: 1/3 + 1/6 = 1/2"
            (toString (ratFromInt 1 / ratFromInt 3 + ratFromInt 1 / ratFromInt 6) == "1/2")
        , check "rat: 3 * 7 = 21"
            (toString (ratFromInt 3 * ratFromInt 7) == "21")
        , check "rat: 10/4 normalises to 5/2"
            (toString (mkRational (toInteger 10) (toInteger 4)) == "5/2")
        , check "rat: negation"
            (toString (~ (ratFromInt 5)) == "-5")
        , check "rat: sign of 0"
            (sign (ratFromInt 0) == 0)
        , check "rat: sign of positive"
            (sign (ratFromInt 3) == 1)
        , check "rat: sign of negative"
            (sign (ratFromInt (~3)) == (~1))
        , check "rat: ordering"
            (ratFromInt 2 < ratFromInt 3)
        , check "rat: power"
            (toString (ratPow (ratFromInt 2 / ratFromInt 3) 3) == "8/27")
        , check "rat: abs of negative"
            (toString (abs (ratFromInt (~7))) == "7")
        , check "rat: negative denominator normalises"
            (toString (mkRational (toInteger 3) (toInteger (~4))) == "-3/4")
        , check "rat: subtraction"
            (toString (ratFromInt 5 - ratFromInt 3) == "2")
        , check "rat: division"
            (toString (ratFromInt 2 / ratFromInt 5) == "2/5")
        ]

    // ---- Positive tests ----
    positiveTests =
        [ check "pos: valid construction"
            (isJust (positive 5))
        , check "pos: zero rejected"
            (isNothing (positive 0))
        , check "pos: negative rejected"
            (isNothing (positive (~3)))
        , check "pos: unPositive roundtrip"
            (unPositive (unsafePositive 42) == 42)
        , check "pos: ordering"
            (unsafePositive 3 < unsafePositive 7)
        , check "pos: equality"
            (unsafePositive 5 == unsafePositive 5)
        ]

    // ---- PrimeFactors tests ----
    primeTests =
        [ check "prime: isPrime 2" (isPrime 2)
        , check "prime: isPrime 3" (isPrime 3)
        , check "prime: isPrime 17" (isPrime 17)
        , check "prime: not isPrime 1" (not (isPrime 1))
        , check "prime: not isPrime 4" (not (isPrime 4))
        , check "prime: not isPrime 15" (not (isPrime 15))
        , check "prime: factorise 360"
            (factorise (unsafePositive 360) == [(2,3),(3,2),(5,1)])
        , check "prime: factorise 1"
            (factorise (unsafePositive 1) == [])
        , check "prime: factorise 7"
            (factorise (unsafePositive 7) == [(7,1)])
        , check "prime: primeFactors 12"
            (primeFactors (unsafePositive 12) == [2,3])
        , check "prime: first 5 primes"
            (take 5 primes == [2,3,5,7,11])
        ]

    // ---- Poly tests (using Rational coefficients) ----
    polyTests =
        [ check "poly: degree of zero" (degree zeroPolyR == ~1)
        , check "poly: degree of constant" (degree (constPoly (ratFromInt 3)) == 0)
        , check "poly: degree of x" (degree monoXR == 1)
        , check "poly: add polynomials"
            (addPoly poly_1_2x poly_3_4x == poly_4_6x)
        , check "poly: mul (1+x)(-1+x) = -1+x^2"
            (mulPoly poly_1_x poly_m1_x == poly_m1_0_x2)
        , check "poly: eval 1+2x+3x^2 at x=2 = 17"
            (evalPoly (mkPoly [ratFromInt 1, ratFromInt 2, ratFromInt 3]) (ratFromInt 2) == ratFromInt 17)
        , check "poly: scale"
            (scalePoly (ratFromInt 3) poly_1_2x == poly_3_6x)
        , check "poly: divmod (x^2-1)/(x+1)"
            (divModPoly poly_m1_0_x2 poly_1_x == (poly_m1_x, zeroPoly))
        , check "poly: gcd"
            (gcdPoly poly_m1_0_x2 poly_1_x == poly_1_x)
        , check "poly: monic"
            (monicPoly (mkPoly [ratFromInt 2, ratFromInt 4]) == poly_half_1)
        , check "poly: diff 1+3x^2 = 6x"
            (diffPoly poly_1_0_3x2 == poly_0_6x)
        , check "poly: negate"
            (negatePoly poly_1_2x == poly_m1_m2)
        ]

    // ---- RadExpr tests ----
    radExprTests =
        [ check "rad: depth of lit" (depth (Lit (ratFromInt 3)) == 0)
        , check "rad: depth of add"
            (depth (Add (Lit (ratFromInt 1)) (Lit (ratFromInt 2))) == 1)
        , check "rad: size of mul"
            (exprSize (Mul (Lit (ratFromInt 1)) (Lit (ratFromInt 2))) == 3)
        , check "rad: sub construction"
            (rSub (Lit (ratFromInt 3)) (Lit (ratFromInt 1))
                == Add (Lit (ratFromInt 3)) (Neg (Lit (ratFromInt 1))))
        , check "rad: div construction"
            (rDiv (Lit (ratFromInt 6)) (Lit (ratFromInt 2))
                == Mul (Lit (ratFromInt 6)) (Inv (Lit (ratFromInt 2))))
        , check "rad: sqrt construction"
            (rSqrt (Lit (ratFromInt 2)) == Root 2 (Lit (ratFromInt 2)))
        , check "rad: intE"
            (intE 5 == Lit (ratFromInt 5))
        , check "rad: equality"
            (Root 3 (Lit (ratFromInt 2)) == Root 3 (Lit (ratFromInt 2)))
        , check "rad: inequality"
            (not (Root 3 (Lit (ratFromInt 2)) == Root 2 (Lit (ratFromInt 2))))
        , check "rad: mapCoeffs"
            (mapCoeffs (\r -> r + ratFromInt 1) (Lit (ratFromInt 3))
                == Lit (ratFromInt 4))
        , check "rad: toString"
            (toString (rSqrt (intE 2)) == "Root(2, 2)")
        ]

    // ---- Interval tests ----
    intervalTests =
        [ check "iv: point interval width 0"
            (width (pointInterval (ratFromInt 3)) == zero)
        , check "iv: add intervals"
            (iadd iv_1_2 iv_3_4 == iv_4_6)
        , check "iv: sub intervals"
            (isub iv_3_5 iv_1_2 == iv_1_4)
        , check "iv: mul positive intervals"
            (imul iv_2_3 iv_4_5 == iv_8_15)
        , check "iv: contains"
            (contains iv_1_5 (ratFromInt 3))
        , check "iv: not contains"
            (not (contains iv_1_5 (ratFromInt 7)))
        , check "iv: overlaps"
            (overlaps (mkInterval (ratFromInt 1) (ratFromInt 4)) iv_3_6)
        , check "iv: strictly positive"
            (strictlyPositive iv_1_5)
        , check "iv: not strictly positive"
            (not (strictlyPositive iv_m1_5))
        , check "iv: strictly negative"
            (strictlyNegative iv_m5_m1)
        , check "iv: contains zero"
            (containsZero iv_m1_1)
        , check "iv: midpoint"
            (midpoint iv_2_8 == ratFromInt 5)
        , check "iv: width"
            (width iv_2_8 == ratFromInt 6)
        , check "iv: inv lo"
            (ivLo (iinv iv_2_4) == ratFromInt 1 / ratFromInt 4)
        , check "iv: inv hi"
            (ivHi (iinv iv_2_4) == ratFromInt 1 / ratFromInt 2)
        , check "iv: negate"
            (ineg iv_2_5 == iv_m5_m2)
        , check "iv: abs of negative"
            (iabs iv_m5_m2 == iv_2_5)
        , check "iv: bisect"
            (ivHi (fst (bisect iv_0_10)) == ratFromInt 5)
        ]

zeroPolyR :: Poly Rational
zeroPolyR = zeroPoly

monoXR :: Poly Rational
monoXR = monoX
