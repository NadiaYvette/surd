module Test.DynTower (tests) where

import Data.Complex (Complex (..))
import Surd.Field.DynTower
import Surd.Radical.DAG (dagEvalComplex, toDAG)
import Surd.Trig.TowerDescent
import Surd.Types (RadExpr (..))
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "DynTower"
    [ basicArithTests,
      extensionTests,
      inversionTests,
      nestedTests,
      conversionTests,
      towerDescentTests
    ]

basicArithTests :: TestTree
basicArithTests =
  testGroup
    "basic rational arithmetic"
    [ testCase "TRat addition" $
        TRat 3 + TRat 4 @?= TRat 7,
      testCase "TRat multiplication" $
        TRat 3 * TRat 4 @?= TRat 12,
      testCase "TRat inversion" $
        recip (TRat 5) @?= TRat (1 / 5),
      testCase "fromRational" $
        (fromRational (3 / 7) :: TowerElem) @?= TRat (3 / 7)
    ]

extensionTests :: TestTree
extensionTests =
  testGroup
    "simple extension Q(√2)"
    [ testCase "√2 · √2 = 2" $ do
        -- Adjoin √2: α² = 2
        let (_, alpha) = adjoinTowerRoot 1 2 (TRat 2)
        alpha * alpha @?= TRat 2,
      testCase "(1 + √2)² = 3 + 2√2" $ do
        let (lvl, alpha) = adjoinTowerRoot 1 2 (TRat 2)
            one_plus_sqrt2 = TRat 1 + alpha
            result = one_plus_sqrt2 * one_plus_sqrt2
            -- Should be 3 + 2√2 = TExt [3, 2] lvl
            expected = TExt [TRat 3, TRat 2] lvl
        result @?= expected,
      testCase "(1 + √2)(1 - √2) = -1" $ do
        let (_, alpha) = adjoinTowerRoot 1 2 (TRat 2)
            a = TRat 1 + alpha
            b = TRat 1 - alpha
        a * b @?= TRat (-1),
      testCase "√2 + √2 = 2√2" $ do
        let (lvl, alpha) = adjoinTowerRoot 1 2 (TRat 2)
            expected = TExt [TRat 0, TRat 2] lvl
        alpha + alpha @?= expected
    ]

inversionTests :: TestTree
inversionTests =
  testGroup
    "field inversion"
    [ testCase "1/√2 · √2 = 1" $ do
        let (_, alpha) = adjoinTowerRoot 1 2 (TRat 2)
            inv_alpha = recip alpha
        inv_alpha * alpha @?= TRat 1,
      testCase "1/(1+√2) · (1+√2) = 1" $ do
        let (_, alpha) = adjoinTowerRoot 1 2 (TRat 2)
            x = TRat 1 + alpha
        recip x * x @?= TRat 1,
      testCase "1/(1+√2) = -1+√2" $ do
        let (lvl, alpha) = adjoinTowerRoot 1 2 (TRat 2)
            x = TRat 1 + alpha
            expected = TExt [TRat (-1), TRat 1] lvl
        recip x @?= expected
    ]

nestedTests :: TestTree
nestedTests =
  testGroup
    "nested tower Q(√2)(√3)"
    [ testCase "√3 · √3 = 3 (in Q(√2)(√3))" $ do
        let (_, _alpha1) = adjoinTowerRoot 1 2 (TRat 2)
            -- Adjoin √3 over Q(√2). The radicand 3 is in Q ⊂ Q(√2).
            (_, alpha2) = adjoinTowerRoot 2 2 (TRat 3)
        alpha2 * alpha2 @?= TRat 3,
      testCase "√2·√3 in Q(√2)(√3)" $ do
        let (lvl1, _alpha1) = adjoinTowerRoot 1 2 (TRat 2)
            -- √2 as an element of Q(√2): [0, 1]
            sqrt2inK1 = TExt [TRat 0, TRat 1] lvl1
            -- Adjoin √3 over Q(√2). Radicand is 3 ∈ Q.
            (lvl2, alpha2) = adjoinTowerRoot 2 2 (TRat 3)
            -- √2 embedded in Q(√2)(√3): constant in the √3 variable
            sqrt2inK2 = TExt [sqrt2inK1] lvl2
            -- √2 · √3 should be [0, √2] at level 2
            expected = TExt [TRat 0, sqrt2inK1] lvl2
            result = sqrt2inK2 * alpha2
        result @?= expected
    ]

conversionTests :: TestTree
conversionTests =
  testGroup
    "tower to RadExpr conversion"
    [ testCase "TRat 5 → Lit 5" $
        towerToRadExpr (TRat 5) @?= Lit 5,
      testCase "√2 → Root 2 (Lit 2)" $ do
        let (_, alpha) = adjoinTowerRoot 1 2 (TRat 2)
        towerToRadExpr alpha @?= Mul (Lit 1) (Root 2 (Lit 2)),
      testCase "1 + √2 → Add (Lit 1) (Mul (Lit 1) (Root 2 (Lit 2)))" $ do
        let (_, alpha) = adjoinTowerRoot 1 2 (TRat 2)
            x = TRat 1 + alpha
            result = towerToRadExpr x
        -- Should be Add (Lit 1) (Mul (Lit 1) (Root 2 (Lit 2)))
        -- or equivalently Lit 1 + Root 2 (Lit 2)
        case result of
          Add (Lit 1) (Mul (Lit 1) (Root 2 (Lit 2))) -> pure ()
          _ -> assertFailure $ "unexpected: " ++ show result
    ]

towerDescentTests :: TestTree
towerDescentTests =
  testGroup
    "tower descent"
    [ testCase "cosViaTower 5 produces a result" $ do
        case cosViaTower 5 of
          Nothing -> assertFailure "cosViaTower 5 returned Nothing"
          Just _expr -> pure (),
      testCase "cosViaTower 7 produces a result" $ do
        case cosViaTower 7 of
          Nothing -> assertFailure "cosViaTower 7 returned Nothing"
          Just _expr -> pure (),
      testCase "cosViaTower 5 RadExpr is numerically correct" $ do
        case cosViaTower 5 of
          Nothing -> assertFailure "cosViaTower 5 returned Nothing"
          Just expr -> do
            let val = dagEvalComplex (toDAG expr)
                expected = cos (2 * pi / 5)
            assertBool
              ("cos(2π/5): " ++ show val ++ " vs " ++ show expected)
              (abs (realPart val - expected) < 1e-10),
      testCase "cosViaTower 7 RadExpr is numerically correct" $ do
        case cosViaTower 7 of
          Nothing -> assertFailure "cosViaTower 7 returned Nothing"
          Just expr -> do
            let val = dagEvalComplex (toDAG expr)
                expected = cos (2 * pi / 7)
            assertBool
              ("cos(2π/7): " ++ show val ++ " vs " ++ show expected)
              (abs (realPart val - expected) < 1e-10),
      testCase "cosViaTower 11 produces a result" $ do
        case cosViaTower 11 of
          Nothing -> assertFailure "cosViaTower 11 returned Nothing"
          Just _expr -> pure (),
      testCase "cosViaTower 13 produces a result" $ do
        case cosViaTower 13 of
          Nothing -> assertFailure "cosViaTower 13 returned Nothing"
          Just _expr -> pure (),
      testCase "cosViaTower 9 produces a result" $ do
        case cosViaTower 9 of
          Nothing -> assertFailure "cosViaTower 9 returned Nothing"
          Just _expr -> pure (),
      testCase "cosViaTower 11 RadExpr is numerically correct" $ do
        case cosViaTower 11 of
          Nothing -> assertFailure "cosViaTower 11 returned Nothing"
          Just expr -> do
            let val = dagEvalComplex (toDAG expr)
                expected = cos (2 * pi / 11)
            assertBool
              ("cos(2π/11): " ++ show val ++ " vs " ++ show expected)
              (abs (realPart val - expected) < 1e-8),
      testCase "cosViaTower 13 RadExpr is numerically correct" $ do
        case cosViaTower 13 of
          Nothing -> assertFailure "cosViaTower 13 returned Nothing"
          Just expr -> do
            let val = dagEvalComplex (toDAG expr)
                expected = cos (2 * pi / 13)
            assertBool
              ("cos(2π/13): " ++ show val ++ " vs " ++ show expected)
              (abs (realPart val - expected) < 1e-8)
    ]

realPart :: Complex Double -> Double
realPart (r :+ _) = r
