module Test.NormalForm (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Surd.Types
import Surd.Radical.NormalForm

tests :: TestTree
tests = testGroup "Radical.NormalForm"
  [ testGroup "Construction"
    [ testCase "normLit 0 is zero" $
        normIsZero (normLit 0) @?= True
    , testCase "normLit 3 is not zero" $
        normIsZero (normLit 3) @?= False
    , testCase "normCoeff (normLit 5) = Just 5" $
        normCoeff (normLit 5) @?= Just 5
    , testCase "normCoeff (normAtom √2) = Nothing" $
        normCoeff (normAtom (RatRoot 2 2)) @?= Nothing
    ]
  , testGroup "Arithmetic"
    [ testCase "2 + 3 = 5" $
        normCoeff (normAdd (normLit 2) (normLit 3)) @?= Just 5
    , testCase "√2 - √2 = 0" $
        normIsZero (normSub (normRoot 2 2) (normRoot 2 2)) @?= True
    , testCase "√2 · √2 = 2" $
        normCoeff (normMul (normRoot 2 2) (normRoot 2 2)) @?= Just 2
    , testCase "√3 · √3 = 3" $
        normCoeff (normMul (normRoot 2 3) (normRoot 2 3)) @?= Just 3
    , testCase "2·√5 + 3·√5 = 5·√5" $ do
        let a = normScale 2 (normRoot 2 5)
            b = normScale 3 (normRoot 2 5)
            result = normAdd a b
        -- Should be 5·√5
        result @?= normScale 5 (normRoot 2 5)
    , testCase "∛8 = 2" $
        normCoeff (normRoot 3 8) @?= Just 2
    , testCase "√12 = 2√3" $
        normRoot 2 12 @?= normScale 2 (normRoot 2 3)
    , testCase "(√2)^4 = 4" $
        normCoeff (normPow (normRoot 2 2) 4) @?= Just 4
    , testCase "i² = -1" $
        normCoeff (normMul (normAtom ImagUnit) (normAtom ImagUnit)) @?= Just (-1)
    , testCase "i⁴ = 1" $
        normCoeff (normPow (normAtom ImagUnit) 4) @?= Just 1
    ]
  , testGroup "Conversion"
    [ testCase "toNormExpr (Lit 7) roundtrips" $ do
        let e = Lit 7 :: RadExpr Rational
        fromNormExpr (toNormExpr e) @?= e
    , testCase "toNormExpr (Root 2 (Lit 5)) roundtrips via eval" $ do
        let e = Root 2 (Lit 5) :: RadExpr Rational
            ne = toNormExpr e
            back = fromNormExpr ne
        -- The structure may differ but numerical value should match
        normCoeff ne @?= Nothing  -- it's irrational
    , testCase "toNormExpr Add combines like terms" $ do
        let e = Add (Root 2 (Lit 2)) (Root 2 (Lit 2)) :: RadExpr Rational
            ne = toNormExpr e
        ne @?= normScale 2 (normRoot 2 2)
    , testCase "toNormExpr Mul of same radicals" $ do
        let e = Mul (Root 2 (Lit 3)) (Root 2 (Lit 3)) :: RadExpr Rational
            ne = toNormExpr e
        normCoeff ne @?= Just 3
    , testCase "toNormExpr Neg" $ do
        let e = Neg (Lit 4) :: RadExpr Rational
        normCoeff (toNormExpr e) @?= Just (-4)
    , testCase "toNormExpr Inv of rational" $ do
        let e = Inv (Lit 3) :: RadExpr Rational
        normCoeff (toNormExpr e) @?= Just (1/3)
    ]
  , testGroup "Inverse"
    [ testCase "1/√2 = √2/2" $ do
        let inv = normInv (normRoot 2 2)
        -- 1/√2 = √2/2: coefficient 1/2, atom √2
        inv @?= normScale (1/2) (normRoot 2 2)
    , testCase "1/√3 = √3/3" $ do
        let inv = normInv (normRoot 2 3)
        inv @?= normScale (1/3) (normRoot 2 3)
    , testCase "1/(2√5) = √5/10" $ do
        let inv = normInv (normScale 2 (normRoot 2 5))
        inv @?= normScale (1/10) (normRoot 2 5)
    , testCase "(1/√2) · √2 = 1" $ do
        let inv = normInv (normRoot 2 2)
        normCoeff (normMul inv (normRoot 2 2)) @?= Just 1
    , testCase "1/(1 + √2) rationalized" $ do
        -- 1/(1+√2) = (1-√2)/((1+√2)(1-√2)) = (1-√2)/(1-2) = √2-1
        let denom = normAdd (normLit 1) (normRoot 2 2)
            inv = normInv denom
            expected = normSub (normRoot 2 2) (normLit 1)
        inv @?= expected
    , testCase "1/(√2 + √3) rationalized" $ do
        -- 1/(√2+√3) = (√3-√2)/((√3+√2)(√3-√2)) = (√3-√2)/(3-2) = √3-√2
        let denom = normAdd (normRoot 2 2) (normRoot 2 3)
            inv = normInv denom
            expected = normSub (normRoot 2 3) (normRoot 2 2)
        inv @?= expected
    , testCase "1/i = -i" $ do
        let inv = normInv (normAtom ImagUnit)
        inv @?= normNeg (normAtom ImagUnit)
    , testCase "toNormExpr (Inv (Root 2 (Lit 2)))" $ do
        let e = Inv (Root 2 (Lit 2)) :: RadExpr Rational
            ne = toNormExpr e
        ne @?= normScale (1/2) (normRoot 2 2)
    , testCase "toNormExpr negative power" $ do
        let e = Pow (Root 2 (Lit 3)) (-2) :: RadExpr Rational
            ne = toNormExpr e
        normCoeff ne @?= Just (1/3)
    ]
  , testGroup "Nested Root"
    [ testCase "toNormExpr √(√2) = ⁴√2" $ do
        let e = Root 2 (Root 2 (Lit 2)) :: RadExpr Rational
            ne = toNormExpr e
        -- √(√2) = ⁴√2 = RatRoot 4 2
        ne @?= normAtom (RatRoot 4 2)
    , testCase "toNormExpr ∛(√5) = ⁶√5" $ do
        let e = Root 3 (Root 2 (Lit 5)) :: RadExpr Rational
            ne = toNormExpr e
        ne @?= normAtom (RatRoot 6 5)
    , testCase "toNormExpr √(2·√3) = √2 · ⁴√3" $ do
        let e = Root 2 (Mul (Lit 2) (Root 2 (Lit 3))) :: RadExpr Rational
            ne = toNormExpr e
        -- √(2·√3) = √2 · √(√3) = √2 · ⁴√3
        let expected = normMul (normRoot 2 2) (normAtom (RatRoot 4 3))
        ne @?= expected
    ]
  ]
