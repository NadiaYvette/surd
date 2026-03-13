module TestPhaseCD

import StdEnv
import Data.Integer
import Rational
import RadExpr
import Normalize
import Pretty
import LaTeX
import Eval
import Expr
import Interval

check :: {#Char} Bool -> ({#Char}, Bool)
check name cond = (name, cond)

showResult :: ({#Char}, Bool) -> {#Char}
showResult (name, True) = "  PASS: " +++ name
showResult (name, False) = "  FAIL: " +++ name

// Test expressions
r2 = ratFromInt 2
r3 = ratFromInt 3
r5 = ratFromInt 5
r0 = ratFromInt 0
r1 = ratFromInt 1
r12 = ratFromInt 12
rm1 = ratFromInt (~1)

sqrt2 = Root 2 (Lit r2)       // sqrt(2)
sqrt3 = Root 2 (Lit r3)       // sqrt(3)
sqrt12 = Root 2 (Lit r12)     // sqrt(12) should normalize to 2*sqrt(3)

Start = (header, map showResult tests, summary)
where
    header = "=== Phase C+D Test Suite ==="

    tests = normalizeTests ++ prettyTests ++ latexTests ++ evalTests ++ exprTests

    npass = length (filter snd tests)
    ntotal = length tests
    summary = toString npass +++ "/" +++ toString ntotal +++ " tests passed"

    // ---- Normalize tests ----
    normalizeTests =
        [ check "norm: flattenArith double neg"
            (flattenArith (Neg (Neg (Lit r2))) == Lit r2)
        , check "norm: flattenArith double inv"
            (flattenArith (Inv (Inv (Lit r3))) == Lit r3)
        , check "norm: foldConstants add"
            (foldConstants (Add (Lit r2) (Lit r3)) == Lit r5)
        , check "norm: foldConstants mul zero"
            (foldConstants (Mul (Lit r0) sqrt2) == Lit r0)
        , check "norm: foldConstants mul one"
            (foldConstants (Mul (Lit r1) sqrt2) == sqrt2)
        , check "norm: foldConstants neg neg"
            (foldConstants (Neg (Neg (Lit r2))) == Lit r2)
        , check "norm: simplifyPowers sqrt*sqrt"
            (simplifyPowers (Mul (Root 2 (Lit r2)) (Root 2 (Lit r2))) == Lit r2)
        , check "norm: simplifyPowers nested roots"
            (simplifyPowers (Root 2 (Root 3 (Lit r2))) == Root 6 (Lit r2))
        , check "norm: extractPerfectPowers sqrt(12) = 2*sqrt(3)"
            (extractPerfectPowers sqrt12 == Mul (Lit r2) sqrt3)
        , check "norm: extractPerfectPowers sqrt(0) = 0"
            (extractPerfectPowers (Root 2 (Lit r0)) == Lit r0)
        , check "norm: normalize 0+sqrt(2) = sqrt(2)"
            (normalize (Add (Lit r0) sqrt2) == sqrt2)
        , check "norm: normalize 1*sqrt(2) = sqrt(2)"
            (normalize (Mul (Lit r1) sqrt2) == sqrt2)
        , check "norm: normalize sqrt(2)+sqrt(2) = 2*sqrt(2)"
            (normalize (Add sqrt2 sqrt2) == Mul (Lit r2) sqrt2)
        ]

    // ---- Pretty tests ----
    prettyTests =
        [ check "pretty: literal 3"
            (pretty (Lit r3) == "3")
        , check "pretty: literal 1/3"
            (pretty (Lit (r1 / r3)) == "(1/3)")
        , check "pretty: neg literal"
            (pretty (Neg (Lit r3)) == "-3")
        , check "pretty: sqrt(-1) = i"
            (pretty (Root 2 (Lit rm1)) == "i")
        , check "pretty: Pow e 0 = 1"
            (pretty (Pow sqrt2 0) == "1")
        , check "pretty: inv"
            (pretty (Inv (Lit r3)) == "1/3")
        ]

    // ---- LaTeX tests ----
    latexTests =
        [ check "latex: literal 3"
            (latex (Lit r3) == "3")
        , check "latex: fraction"
            (latex (Lit (r1 / r3)) == "\\frac{1}{3}")
        , check "latex: sqrt(-1) = i"
            (latex (Root 2 (Lit rm1)) == "\\mathrm{i}")
        , check "latex: sqrt(2)"
            (latex sqrt2 == "\\sqrt{2}")
        , check "latex: cube root"
            (latex (Root 3 (Lit r5)) == "\\sqrt[3]{5}")
        , check "latex: inv"
            (latex (Inv (Lit r3)) == "\\frac{1}{3}")
        , check "latex: a/b via Mul/Inv"
            (latex (Mul (Lit r2) (Inv (Lit r3))) == "\\frac{2}{3}")
        ]

    // ---- Eval tests ----
    evalTests =
        [ check "eval: literal"
            (eval (Lit r3) == 3.0)
        , check "eval: add"
            (eval (Add (Lit r2) (Lit r3)) == 5.0)
        , check "eval: mul"
            (eval (Mul (Lit r2) (Lit r3)) == 6.0)
        , check "eval: inv"
            (abs (eval (Inv (Lit r3)) - 1.0/3.0) < 1.0E-10)
        , check "eval: sqrt(4) = 2"
            (abs (eval (Root 2 (Lit (ratFromInt 4))) - 2.0) < 1.0E-10)
        , check "eval: pow"
            (abs (eval (Pow (Lit r2) 3) - 8.0) < 1.0E-10)
        , check "evalComplex: real"
            (let (re, im) = evalComplex (Lit r3) in abs (re - 3.0) < 1.0E-10 && abs im < 1.0E-10)
        , check "evalComplex: sqrt(-1) = i"
            (let (re, im) = evalComplex (Root 2 (Lit rm1)) in abs re < 1.0E-10 && abs (im - 1.0) < 1.0E-10)
        , check "evalInterval: literal point"
            (let iv = evalInterval (Lit r3) in ivLo iv == r3 && ivHi iv == r3)
        , check "evalInterval: add"
            (let iv = evalInterval (Add (Lit r2) (Lit r3)) in ivLo iv == r5 && ivHi iv == r5)
        ]

    // ---- Expr tests ----
    exprTests =
        [ check "expr: freeOf true"
            (freeOf (\r -> r == r3) (Lit r3))
        , check "expr: freeOf false"
            (not (freeOf (\r -> r == r3) (Lit r2)))
        , check "expr: collectRadicals"
            (length (collectRadicals (Add sqrt2 sqrt3)) == 2)
        , check "expr: collectRadicals no dups"
            (length (collectRadicals (Add sqrt2 sqrt2)) == 1)
        , check "expr: allRootsResolved"
            (allRootsResolved [(2, Lit r2)] (Root 2 (Lit r2)))
        , check "expr: allRootsResolved false"
            (not (allRootsResolved [] (Root 2 (Lit r2))))
        ]
