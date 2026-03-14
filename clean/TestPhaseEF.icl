module TestPhaseEF

import StdEnv
import Data.Integer
import Rational
import RadExpr
import Poly
import Extension
import NormalForm
import Cyclotomic
import Factoring
import Resultant
import Denest
import Normalize
import Eval
import Algebra
import RootOfUnity
import DAG

check :: {#Char} Bool -> ({#Char}, Bool)
check name cond = (name, cond)

showResult :: ({#Char}, Bool) -> {#Char}
showResult (name, True) = "  PASS: " +++ name
showResult (name, False) = "  FAIL: " +++ name

// ─── Helpers ───
r :: !Int -> Rational
r n = ratFromInt n

ri :: !Int -> RadExpr Rational
ri n = Lit (r n)

sqrt2 :: RadExpr Rational
sqrt2 = Root 2 (ri 2)

sqrt3 :: RadExpr Rational
sqrt3 = Root 2 (ri 3)

sqrt5 :: RadExpr Rational
sqrt5 = Root 2 (ri 5)

Start = (header, map showResult tests, summary)
where
    header = "=== Phase E+F Test Suite ==="

    tests = extensionTests ++ factoringTests ++ cyclotomicTests
         ++ resultantTests ++ normalFormTests ++ denestTests
         ++ algebraTests ++ dagTests ++ rootOfUnityTests

    npass = length (filter snd tests)
    ntotal = length tests
    summary = toString npass +++ "/" +++ toString ntotal +++ " tests passed"

    // ─── Extension field tests ───
    // Work in Q(sqrt(2)) via minpoly x^2 - 2
    minpolyS2 = mkPoly [~ (r 2), r 0, r 1]   // x^2 - 2

    extZero = extFromRat (r 0) minpolyS2
    extOne  = extFromRat (r 1) minpolyS2
    alpha   = extAlpha minpolyS2               // alpha = sqrt(2)

    onePlusAlpha = extAdd extOne alpha

    extensionTests =
        [ check "ext: alpha != 0"
            (not (alpha == extZero))
        , check "ext: 1 + 0 = 1 in Q(sqrt(2))"
            (extAdd extOne extZero == extOne)
        , check "ext: alpha - alpha = 0"
            (extSub alpha alpha == extZero)
        , check "ext: alpha^2 = 2 in Q(sqrt(2))"
            (extMul alpha alpha == extFromRat (r 2) minpolyS2)
        , check "ext: (1+alpha) + (1-alpha) = 2"
            (extAdd (extAdd extOne alpha) (extSub extOne alpha)
                == extFromRat (r 2) minpolyS2)
        , check "ext: extNeg extOne == fromRat(-1)"
            (extNeg extOne == extFromRat (~ (r 1)) minpolyS2)
        , check "ext: inv of 1 = 1"
            (extInv extOne == extOne)
        , check "ext: alpha * inv(alpha) = 1"
            (extMul alpha (extInv alpha) == extOne)
        , check "ext: (1+alpha) * inv(1+alpha) = 1"
            (extMul onePlusAlpha (extInv onePlusAlpha) == extOne)
        , check "ext: toString"
            (toString extOne == toString (constPoly (r 1)) +++ " mod " +++ toString minpolyS2)
        ]

    // ─── Factoring tests ───
    factoringTests =
        [ check "factor: x^2-1 has rational root 1"
            (isMember (r 1) (rationalRoots (mkPoly [~ (r 1), r 0, r 1])))
        , check "factor: x^2-1 has rational root -1"
            (isMember (~ (r 1)) (rationalRoots (mkPoly [~ (r 1), r 0, r 1])))
        , check "factor: x^2+1 has no rational root"
            (not (hasRationalRoot (mkPoly [r 1, r 0, r 1])))
        , check "factor: x has rational root 0"
            (isMember (r 0) (rationalRoots (mkPoly [r 0, r 1])))
        , check "factor: 2x-1 has root 1/2"
            (isMember (mkRational (toInteger 1) (toInteger 2))
                      (rationalRoots (mkPoly [~ (r 1), r 2])))
        , check "factor: x^3-1 has root 1"
            (isMember (r 1) (rationalRoots (mkPoly [~ (r 1), r 0, r 0, r 1])))
        , check "factor: factor returns non-empty"
            (not (isEmpty (factor (mkPoly [~ (r 1), r 0, r 1]))))
        ]

    // ─── Cyclotomic tests ───
    cyclotomicTests =
        [ check "cyclotomic(1) = x - 1"
            (cyclotomic 1 == mkPoly [~ (r 1), r 1])
        , check "cyclotomic(2) = x + 1"
            (cyclotomic 2 == mkPoly [r 1, r 1])
        , check "cyclotomic(3) = x^2 + x + 1"
            (cyclotomic 3 == mkPoly [r 1, r 1, r 1])
        , check "cyclotomic(4) = x^2 + 1"
            (cyclotomic 4 == mkPoly [r 1, r 0, r 1])
        , check "cyclotomic(6) = x^2 - x + 1"
            (cyclotomic 6 == mkPoly [r 1, ~ (r 1), r 1])
        , check "cyclotomic(5) degree = phi(5) = 4"
            (degree (cyclotomic 5) == 4)
        , check "cyclotomic(7) degree = phi(7) = 6"
            (degree (cyclotomic 7) == 6)
        , check "cyclotomic(12) degree = phi(12) = 4"
            (degree (cyclotomic 12) == 4)
        , check "eulerTotient 1 = 1"
            (eulerTotient 1 == 1)
        , check "eulerTotient 6 = 2"
            (eulerTotient 6 == 2)
        , check "eulerTotient 12 = 4"
            (eulerTotient 12 == 4)
        , check "eulerTotient 7 = 6"
            (eulerTotient 7 == 6)
        ]

    // ─── Resultant tests ───
    resultantTests =
        [ check "resultant: res(x, x) = 0"
            (polyResultant (mkPoly [r 0, r 1]) (mkPoly [r 0, r 1]) == r 0)
        , check "resultant: res(x-1, x-2) nonzero"
            (not (polyResultant (mkPoly [~ (r 1), r 1]) (mkPoly [~ (r 2), r 1]) == r 0))
        , check "resultant: res(x-1, x^2-1) = 0 (common root)"
            (polyResultant (mkPoly [~ (r 1), r 1]) (mkPoly [~ (r 1), r 0, r 1]) == r 0)
        , check "resultant: res(const, const) = expected"
            (polyResultant (constPoly (r 3)) (constPoly (r 5)) == r 1)
        ]

    // ─── NormalForm tests ───
    normalFormTests =
        [ check "normform: literal roundtrip"
            (normalFormRoundTrip (ri 3) == ri 3)
        , check "normform: sqrt(2) roundtrip"
            (normalFormRoundTrip sqrt2 == sqrt2)
        , check "normform: 0+x roundtrip simplifies"
            (normalFormRoundTrip (Add (ri 0) sqrt2) == sqrt2)
        , check "normform: 2*sqrt(2)+3*sqrt(2) = 5*sqrt(2)"
            (normalFormRoundTrip (Add (Mul (ri 2) sqrt2) (Mul (ri 3) sqrt2))
                == Mul (ri 5) sqrt2)
        , check "normform: sqrt(12) roundtrip = 2*sqrt(3)"
            (normalFormRoundTrip (Root 2 (ri 12)) == Mul (ri 2) sqrt3)
        , check "normform: 1*x roundtrip = x"
            (normalFormRoundTrip (Mul (ri 1) sqrt5) == sqrt5)
        , check "normform: 0*x roundtrip = 0"
            (normalFormRoundTrip (Mul (ri 0) sqrt2) == ri 0)
        , check "normform: x - x = 0"
            (normalFormRoundTrip (Add sqrt2 (Neg sqrt2)) == ri 0)
        ]

    // ─── Denesting tests ───
    // sqrt(3 + 2*sqrt(2)) = sqrt(2) + 1   (since disc = 9-8 = 1, d=2, e=1)
    denestableExpr = Root 2 (Add (ri 3) (Mul (ri 2) (Root 2 (ri 2))))

    denestResult = denest denestableExpr
    denestInput2 = Add (ri 1) denestableExpr
    denestResult2 = denest denestInput2

    denestTests =
        [ check "denest: sqrt(3+2*sqrt(2)) denests"
            (not (denestResult == denestableExpr))
        , check "denest: sqrt(3+2*sqrt(2)) = sqrt(2)+sqrt(1)"
            (denestResult == Add (Root 2 (ri 2)) (Root 2 (ri 1)))
        , check "denest: literal unchanged"
            (denest (ri 5) == ri 5)
        , check "denest: plain sqrt unchanged"
            (denest sqrt2 == sqrt2)
        , check "denest: cube root stub returns input"
            (denestCubeRoot (Root 3 (ri 2)) == Root 3 (ri 2))
        , check "denest: denest recurses into subexpressions"
            (not (denestResult2 == denestInput2))
        ]

    // ─── Algebra class tests ───
    algebraTests =
        [ check "algebra: Rational rzero + rone = rone"
            (radd ratRingZero ratRingOne == ratRingOne)
        , check "algebra: Rational rmul rone (r 5) = r 5"
            (rmul ratRingOne (r 5) == r 5)
        , check "algebra: Rational rneg (r 3) = r (-3)"
            (rneg (r 3) == ~ (r 3))
        , check "algebra: Rational finv (r 2) = 1/2"
            (finv (r 2) == mkRational (toInteger 1) (toInteger 2))
        , check "algebra: Rational fdiv (r 6) (r 3) = r 2"
            (fdiv (r 6) (r 3) == r 2)
        , check "algebra: rpow (r 2) 3 = r 8"
            (rpow (r 2) 3 == r 8)
        , check "algebra: rpow (r 3) 0 = rone"
            (rpow (r 3) 0 == ratRingOne)
        , check "algebra: Poly Ring rzero"
            (degree polyRingZero == ~1)
        , check "algebra: Poly Ring radd"
            (radd (mkPoly [r 1, r 2]) (mkPoly [r 3, r 4]) == mkPoly [r 4, r 6])
        , check "algebra: Poly Ring rmul"
            (rmul (mkPoly [r 1, r 1]) (mkPoly [~ (r 1), r 1]) == mkPoly [~ (r 1), r 0, r 1])
        ]

    // ─── DAG tests ───
    cseExpr = Add sqrt2 sqrt2
    dagEvalLit = dagEvalComplex (toDAG (ri 3))
    dagEvalSqrt2 = dagEvalComplex (toDAG sqrt2)

    dagTests =
        [ check "dag: toDAG/fromDAG roundtrip for literal"
            (fromDAG (toDAG (ri 5)) == ri 5)
        , check "dag: toDAG/fromDAG roundtrip for sqrt(2)"
            (fromDAG (toDAG sqrt2) == sqrt2)
        , check "dag: toDAG/fromDAG roundtrip for add"
            (fromDAG (toDAG (Add sqrt2 sqrt3)) == Add sqrt2 sqrt3)
        , check "dag: dagSize >= 1 for literal"
            (dagSize (toDAG (ri 3)) >= 1)
        , check "dag: CSE deduplication reduces size"
            (dagSize (toDAG cseExpr) < exprSize cseExpr)
        , check "dag: dagDepth of literal = 0"
            (dagDepth (toDAG (ri 3)) == 0)
        , check "dag: dagFoldConstants 2+3 = 5"
            (fromDAG (dagFoldConstants (toDAG (Add (ri 2) (ri 3)))) == ri 5)
        , check "dag: dagFoldConstants 0*sqrt(2) = 0"
            (fromDAG (dagFoldConstants (toDAG (Mul (ri 0) sqrt2))) == ri 0)
        , check "dag: dagEvalComplex literal"
            (abs (fst dagEvalLit - 3.0) < 1.0E-10 && abs (snd dagEvalLit) < 1.0E-10)
        , check "dag: dagEvalComplex sqrt(2)"
            (abs (fst dagEvalSqrt2 - 1.41421356) < 0.0001 && abs (snd dagEvalSqrt2) < 1.0E-10)
        ]

    // ─── RootOfUnity tests ───
    rootOfUnityTests =
        [ check "rou: cos(2pi/1) = 1"
            (optEq (cosOfUnity 1) (ri 1))
        , check "rou: cos(2pi/2) = -1"
            (optEq (cosOfUnity 2) (ri (~1)))
        , check "rou: cos(2pi/4) = 0"
            (optEq (cosOfUnity 4) (ri 0))
        , check "rou: cos(2pi/3) = -1/2"
            (optEq (cosOfUnity 3) (Lit (mkRational (toInteger (~1)) (toInteger 2))))
        , check "rou: cos(2pi/6) = 1/2"
            (optEq (cosOfUnity 6) (Lit (mkRational (toInteger 1) (toInteger 2))))
        , check "rou: cos(2pi/5) is Just"
            (isJust (cosOfUnity 5))
        , check "rou: sin(2pi/1) = 0"
            (optEq (sinOfUnity 1) (ri 0))
        , check "rou: sin(2pi/4) = 1"
            (optEq (sinOfUnity 4) (ri 1))
        , check "rou: isConstructible 8"
            (isConstructible 8)
        , check "rou: isConstructible 5"
            (isConstructible 5)
        , check "rou: isConstructible 17"
            (isConstructible 17)
        , check "rou: not isConstructible 7"
            (not (isConstructible 7))
        , check "rou: not isConstructible 9"
            (not (isConstructible 9))
        , check "rou: cosOfUnity 0 = None"
            (isNone (cosOfUnity 0))
        , check "rou: cosOfUnity negative = None"
            (isNone (cosOfUnity (~5)))
        ]

// Helper: check whether ?(a) is ?Just and equals the expected value
optEq :: ?(a) a -> Bool | == a
optEq (?Just x) expected = x == expected
optEq ?None _ = False

isJust :: ?(a) -> Bool
isJust (?Just _) = True
isJust _ = False

isNone :: ?(a) -> Bool
isNone ?None = True
isNone _ = False

// Typed helpers for Ring/Field disambiguation
ratRingZero :: Rational
ratRingZero = rzero

ratRingOne :: Rational
ratRingOne = rone

polyRingZero :: Poly Rational
polyRingZero = rzero
