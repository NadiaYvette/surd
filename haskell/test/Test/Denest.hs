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

  , testCase "√(1/3) rationalizes to (1/3)·√3" $ do
      let expr = normalize (Root 2 (Lit (1/3)))
      expr @?= Mul (Lit (1/3)) (Root 2 (Lit 3))

  , testCase "√(49/6912) rationalizes to (7/144)·√3" $ do
      -- 49/6912 = 7²/(2⁸·3³)
      -- √(49/6912) = 7/√(6912) = 7/(48√3) = 7√3/144
      let expr = normalize (Root 2 (Lit (49/6912)))
      expr @?= Mul (Lit (7/144)) (Root 2 (Lit 3))

  , testCase "∛(1/4) rationalizes to (1/4)·∛2" $ do
      -- ∛(1/4) = ∛(2)/∛(8) = ∛2/2 ... wait:
      -- ∛(1/4) = ∛(1·16)/(4) = ∛16/4 = 2∛2/4 = (1/2)·∛2
      let expr = normalize (Root 3 (Lit (1/4)))
      expr @?= Mul (Lit (1/2)) (Root 3 (Lit 2))

  , testCase "√(2/5) rationalizes to (1/5)·√10" $ do
      let expr = normalize (Root 2 (Lit (2/5)))
      expr @?= Mul (Lit (1/5)) (Root 2 (Lit 10))

  , testCase "rationalization preserves value" $ do
      let cases = [1/3, 49/6912, 2/5, 1/4, 7/12 :: Rational]
      mapM_ (\r -> do
        let orig = eval (Root 2 (Lit r)) :: Double
            normed = eval (normalize (Root 2 (Lit r))) :: Double
        abs (orig - normed) < 1e-10 @? ("√(" ++ show r ++ ") value mismatch")
        ) cases
  ]
