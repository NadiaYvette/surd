module Test.Denest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Surd.Types
import Surd.Radical.Denest.Sqrt
import Surd.Radical.Eval (eval)
import Surd.Radical.Normalize (normalize)

tests :: TestTree
tests = testGroup "Radical.Denest.Sqrt"
  [ testCase "√(3 + 2√2) = 1 + √2" $ do
      -- √(3 + 2√2) should denest to √1 + √2 = 1 + √2
      let result = trySqrtDenest 3 2 2
      result @?= Just (1, 2, 1)
      -- sign=1, x=2, y=1, so √2 + √1 = √2 + 1

  , testCase "√(5 + 2√6) = √2 + √3" $ do
      let result = trySqrtDenest 5 2 6
      result @?= Just (1, 3, 2)

  , testCase "√(3 - 2√2) = √2 - 1" $ do
      let result = trySqrtDenest 3 (-2) 2
      result @?= Just (-1, 2, 1)

  , testCase "non-denestable returns Nothing" $ do
      -- √(1 + √2) cannot be denested over Q
      -- disc = 1 - 2 = -1, so no rational sqrt
      let result = trySqrtDenest 1 1 2
      result @?= Nothing

  , testCase "denestSqrtExpr matches pattern" $ do
      -- √(3 + 2√2) as an expression
      let expr = Root 2 (Add (Lit 3) (Mul (Lit 2) (Root 2 (Lit 2))))
      case denestSqrtExpr expr of
        Nothing -> assertFailure "should denest"
        Just denested -> do
          let orig = eval expr :: Double
              new  = eval denested :: Double
          abs (orig - new) < 1e-10 @? "values should match"

  , testCase "denestSqrt recursive" $ do
      -- Build √(3 + 2√2) and denest
      let expr = Root 2 (Add (Lit 3) (Mul (Lit 2) (Root 2 (Lit 2))))
          denested = denestSqrt expr
          orig = eval expr :: Double
          new  = eval denested :: Double
      abs (orig - new) < 1e-10 @? "values should match after recursive denesting"

  , testCase "normalize √12 = 2√3" $ do
      let expr = normalize (Root 2 (Lit 12))
          v = eval expr :: Double
      abs (v - sqrt 12) < 1e-10 @? "√12 should evaluate correctly after normalization"
  ]
