module Test.Polynomial (tests) where

import Math.Polynomial.Univariate
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Polynomial.Univariate"
    [ testCase "addPoly" $ do
        let p = mkPoly [1, 2, 3 :: Rational] -- 1 + 2x + 3x²
            q = mkPoly [4, 5 :: Rational] -- 4 + 5x
        unPoly (addPoly p q) @?= [5, 7, 3 :: Rational],
      testCase "mulPoly" $ do
        let p = mkPoly [1, 1 :: Rational] -- 1 + x
            q = mkPoly [1, 1 :: Rational] -- 1 + x
        unPoly (mulPoly p q) @?= [1, 2, 1 :: Rational], -- 1 + 2x + x²
      testCase "divModPoly" $ do
        -- (x² + 2x + 1) / (x + 1) = (x + 1), rem 0
        let p = mkPoly [1, 2, 1 :: Rational]
            q = mkPoly [1, 1 :: Rational]
            (quot', rem') = divModPoly p q
        unPoly quot' @?= [1, 1 :: Rational]
        unPoly rem' @?= ([] :: [Rational]),
      testCase "gcdPoly" $ do
        -- gcd(x² - 1, x - 1) = x - 1 (made monic)
        let p = mkPoly [-1, 0, 1 :: Rational] -- x² - 1
            q = mkPoly [-1, 1 :: Rational] -- x - 1
            g = gcdPoly p q
        unPoly g @?= [-1, 1 :: Rational],
      testCase "evalPoly" $ do
        let p = mkPoly [1, 2, 3 :: Rational]
        evalPoly p 2 @?= (1 + 2 * 2 + 3 * 4 :: Rational),
      testCase "diffPoly" $ do
        let p = mkPoly [5, 3, 2, 1 :: Rational] -- 5 + 3x + 2x² + x³
            p' = diffPoly p
        unPoly p' @?= [3, 4, 3 :: Rational], -- 3 + 4x + 3x²
      testCase "zeroPoly identity" $ do
        let p = mkPoly [1, 2, 3 :: Rational]
        unPoly (addPoly p zeroPoly) @?= unPoly p
        unPoly (mulPoly p zeroPoly) @?= ([] :: [Rational])
    ]
