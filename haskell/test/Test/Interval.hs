module Test.Interval (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Surd.Internal.Interval
import Surd.Internal.PSLQ (pslq, findMinPoly)
import Surd.Types
import Surd.Radical.Eval (evalInterval)
import Surd.Radical.Equality (radicalEq)
import Surd.Radical.Order (radicalCompare)

tests :: TestTree
tests = testGroup "Tier 3"
  [ intervalTests
  , pslqTests
  , rigorousEqTests
  ]

intervalTests :: TestTree
intervalTests = testGroup "Interval nth root"
  [ testCase "∛8 = 2" $ do
      let iv = inth 3 (Interval 8 8)
      lo iv <= 2 && hi iv >= 2 @? "∛8 should contain 2"

  , testCase "∛27 = 3" $ do
      let iv = inth 3 (Interval 27 27)
      lo iv <= 3 && hi iv >= 3 @? "∛27 should contain 3"

  , testCase "⁴√16 = 2" $ do
      let iv = inth 4 (Interval 16 16)
      lo iv <= 2 && hi iv >= 2 @? "⁴√16 should contain 2"

  , testCase "⁵√32 = 2" $ do
      let iv = inth 5 (Interval 32 32)
      lo iv <= 2 && hi iv >= 2 @? "⁵√32 should contain 2"

  , testCase "∛(-8) = -2 (odd root)" $ do
      let iv = inth 3 (Interval (-8) (-8))
      lo iv <= (-2) && hi iv >= (-2) @? "∛(-8) should contain -2"

  , testCase "∛2 interval is narrow" $ do
      let iv = inth 3 (Interval 2 2)
          w = width iv
      w < 1/100 @? ("∛2 interval too wide: " ++ show w)
      lo iv > 0 @? "∛2 lower bound should be positive"

  , testCase "√2 via isqrt matches inth 2" $ do
      let iv1 = isqrt (Interval 2 2)
          iv2 = inth 2 (Interval 2 2)
      lo iv1 <= fromRational (hi iv2) && lo iv2 <= fromRational (hi iv1) @?
        "isqrt and inth 2 should overlap"

  , testCase "evalInterval with ∛2 encloses true value" $ do
      let expr = Root 3 (Lit 2) :: RadExpr Rational
          iv = evalInterval expr
      -- ∛2 satisfies x³ = 2, so lo³ ≤ 2 ≤ hi³
      lo iv ^^ (3 :: Int) <= 2 @? "lo³ should be ≤ 2"
      hi iv ^^ (3 :: Int) >= 2 @? "hi³ should be ≥ 2"
      width iv < 1/1000 @? ("interval too wide: " ++ show (width iv))

  , testCase "evalInterval with ∛2 + ∛3 is narrow" $ do
      let expr = Add (Root 3 (Lit 2)) (Root 3 (Lit 3)) :: RadExpr Rational
          iv = evalInterval expr
      width iv < 1/100 @? ("interval too wide: " ++ show (width iv))
      lo iv > 0 @? "sum of positive cube roots should be positive"
  ]

pslqTests :: TestTree
pslqTests = testGroup "PSLQ"
  [ testCase "finds √2 minimal polynomial: x² - 2" $ do
      let alpha = sqrt 2
      case findMinPoly alpha 4 of
        Just coeffs -> do
          length coeffs @?= 3  -- degree 2: c₀ + c₁x + c₂x²
          let residual = sum [fromIntegral c * alpha ^ (i :: Int) | (i, c) <- zip [0..] coeffs]
          abs residual < 1e-8 @? ("residual too large: " ++ show residual)
        Nothing -> assertFailure "PSLQ should find √2 relation"

  , testCase "finds ∛2 minimal polynomial: x³ - 2" $ do
      let alpha = 2 ** (1/3 :: Double)
      case findMinPoly alpha 4 of
        Just coeffs -> do
          length coeffs @?= 4  -- degree 3
          let residual = sum [fromIntegral c * alpha ^ (i :: Int) | (i, c) <- zip [0..] coeffs]
          abs residual < 1e-8 @? ("residual too large: " ++ show residual)
        Nothing -> assertFailure "PSLQ should find ∛2 relation"

  , testCase "finds cos(2π/7) minimal polynomial (degree 3)" $ do
      let alpha = cos (2 * pi / 7)
      case findMinPoly alpha 6 of
        Just coeffs -> do
          length coeffs @?= 4  -- degree 3
          let residual = sum [fromIntegral c * alpha ^ (i :: Int) | (i, c) <- zip [0..] coeffs]
          abs residual < 1e-8 @? ("residual too large: " ++ show residual)
        Nothing -> assertFailure "PSLQ should find cos(2π/7) relation"

  , testCase "φ = (1+√5)/2 minimal polynomial: x² - x - 1" $ do
      let alpha = (1 + sqrt 5) / 2
      case findMinPoly alpha 4 of
        Just coeffs -> do
          length coeffs @?= 3
          let residual = sum [fromIntegral c * alpha ^ (i :: Int) | (i, c) <- zip [0..] coeffs]
          abs residual < 1e-8 @? ("residual too large: " ++ show residual)
        Nothing -> assertFailure "PSLQ should find golden ratio relation"

  , testCase "integer relation: 1 + √2 - √2 = 1" $ do
      let x = [1, sqrt 2, -(sqrt 2)] :: [Double]
      case pslq x 500 of
        Just coeffs -> do
          let dot = sum (zipWith (\c v -> fromIntegral c * v) coeffs x) :: Double
          abs dot < 1e-8 @? ("relation not satisfied: " ++ show dot)
        Nothing -> assertFailure "should find trivial relation"
  ]

rigorousEqTests :: TestTree
rigorousEqTests = testGroup "Rigorous equality/ordering"
  [ testCase "√2 == √2" $ do
      radicalEq (Root 2 (Lit 2)) (Root 2 (Lit 2)) @? "√2 should equal √2"

  , testCase "√2 ≠ √3" $ do
      not (radicalEq (Root 2 (Lit 2)) (Root 2 (Lit 3))) @? "√2 should not equal √3"

  , testCase "1 + √2 == √2 + 1" $ do
      let e1 = Add (Lit 1) (Root 2 (Lit 2)) :: RadExpr Rational
          e2 = Add (Root 2 (Lit 2)) (Lit 1) :: RadExpr Rational
      radicalEq e1 e2 @? "1 + √2 should equal √2 + 1"

  , testCase "√2 < √3" $ do
      radicalCompare (Root 2 (Lit 2)) (Root 2 (Lit 3)) @?= LT

  , testCase "∛3 > 1" $ do
      radicalCompare (Root 3 (Lit 3)) (Lit 1) @?= GT

  , testCase "√2 + √3 > √5" $ do
      let e1 = Add (Root 2 (Lit 2)) (Root 2 (Lit 3)) :: RadExpr Rational
          e2 = Root 2 (Lit 5) :: RadExpr Rational
      radicalCompare e1 e2 @?= GT
  ]
