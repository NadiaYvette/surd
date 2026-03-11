module Test.Groebner (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Math.Polynomial.Multivariate
import Math.Polynomial.Groebner
import Surd.Types (RadExpr(..))
import Surd.Radical.NormalForm (toNormExpr, fromNormExpr, normIsZero)
import Surd.Radical.Groebner qualified as RG
import Data.Ratio ((%), Ratio)

tests :: TestTree
tests = testGroup "Groebner"
  [ monoOpTests
  , divisionTests
  , basisTests
  , incrementalTests
  , radicalTests
  ]

-- Helpers
x, y, z :: MPoly Rational
x = varPoly (Var 0)
y = varPoly (Var 1)
z = varPoly (Var 2)

monoOpTests :: TestTree
monoOpTests = testGroup "monomial operations"
  [ testCase "monoGcd xy, yz = y" $
      monoGcd (monoVar (Var 0) 1 `monoMul` monoVar (Var 1) 1)
              (monoVar (Var 1) 1 `monoMul` monoVar (Var 2) 1)
      @?= monoVar (Var 1) 1

  , testCase "monoLcm xy, yz = xyz" $
      monoLcm (monoVar (Var 0) 1 `monoMul` monoVar (Var 1) 1)
              (monoVar (Var 1) 1 `monoMul` monoVar (Var 2) 1)
      @?= (monoVar (Var 0) 1 `monoMul` monoVar (Var 1) 1 `monoMul` monoVar (Var 2) 1)

  , testCase "monoDivides x^2 into x^3y" $
      monoDivides (monoVar (Var 0) 2)
                  (monoVar (Var 0) 3 `monoMul` monoVar (Var 1) 1)
      @?= True

  , testCase "monoDivides xy does not divide x^2" $
      monoDivides (monoVar (Var 0) 1 `monoMul` monoVar (Var 1) 1)
                  (monoVar (Var 0) 2)
      @?= False

  , testCase "monoDiv x^3y / xy = x^2" $
      monoDiv (monoVar (Var 0) 3 `monoMul` monoVar (Var 1) 1)
              (monoVar (Var 0) 1 `monoMul` monoVar (Var 1) 1)
      @?= monoVar (Var 0) 2
  ]

divisionTests :: TestTree
divisionTests = testGroup "multivariate division"
  [ testCase "x^2 + xy + y^2 mod [xy - 1]" $ do
      let f = x*x + x*y + y*y
          gs = [x*y - 1]
          (_, r) = divModMPoly grevlex f gs
      -- x^2 + xy + y^2 = 1*(xy - 1) + (x^2 + y^2 + 1)
      r @?= x*x + y*y + 1

  , testCase "division by zero divisors gives remainder = input" $ do
      let f = x*x + y
          (_, r) = divModMPoly grevlex f [y*y]
      -- No term of f is divisible by y^2
      r @?= f
  ]

basisTests :: TestTree
basisTests = testGroup "Groebner basis"
  [ testCase "trivial: single generator" $ do
      let gb = groebnerBasis grevlex [x - 1]
      reduce gb (x*x - 1) @?= constPoly 0

  , testCase "x^2 - y, xy - 1 (twisted cubic fragment)" $ do
      -- Ideal: x^2 = y, xy = 1 → y^2 = x (from S-poly), then x = y^2
      -- Reduced: {y^3 - 1, x - y^2} or similar
      let gb = groebnerBasis grevlex [x*x - y, x*y - 1]
      -- x^3 = x·(x^2) = x·y and x·y = 1, so x^3 - 1 should be in ideal
      reduce gb (x*x*x - 1) @?= constPoly 0
      -- y = x^2, y^3 = x^6 = (x^3)^2 = 1, so y^3 - 1 in ideal
      reduce gb (y*y*y - 1) @?= constPoly 0

  , testCase "ideal membership: f in I iff reduce f = 0" $ do
      let gb = groebnerBasis grevlex [x*x + y*y - 1, x - y]
      -- x = y and x^2 + y^2 = 1 → 2y^2 = 1
      -- x^2 - y^2 = (x-y)(x+y) should be in ideal since x-y is
      reduce gb (x*x - y*y) @?= constPoly 0
      -- But x + y - 1 is not in the ideal (generically)
      let r = reduce gb (x + y - 1)
      assertBool "x+y-1 not in ideal" (r /= constPoly 0)

  , testCase "basis of {0} is empty" $ do
      let gb = groebnerBasis grevlex [zeroPoly, zeroPoly]
      gbPolys gb @?= []

  , testCase "lex elimination order" $ do
      -- With lex order (x > y), the basis should contain a polynomial in y only
      let gb = groebnerBasis lexOrd [x*x + y*y - 1, x - y]
      -- x - y is already in the basis
      reduce gb x @?= reduce gb y
      -- There should be a univariate in y: 2y^2 - 1
      let ry = reduce gb (2 * y * y)
      ry @?= constPoly 1
  ]

incrementalTests :: TestTree
incrementalTests = testGroup "incremental extension"
  [ testCase "extend empty basis" $ do
      let gb0 = groebnerBasis grevlex []
          gb1 = extendBasis [x - 1] gb0
      reduce gb1 (x*x - 1) @?= constPoly 0

  , testCase "extend with second generator" $ do
      -- Start with x^2 - y
      let gb1 = groebnerBasis grevlex [x*x - y]
          -- x^2 = y, but x*y - 1 adds more info
          gb2 = extendBasis [x*y - 1] gb1
      reduce gb2 (x*x*x - 1) @?= constPoly 0
      reduce gb2 (y*y*y - 1) @?= constPoly 0

  , testCase "incremental matches full computation" $ do
      let gb_full = groebnerBasis grevlex [x*x - y, x*y - 1]
          gb1 = groebnerBasis grevlex [x*x - y]
          gb_inc = extendBasis [x*y - 1] gb1
      -- Both should reduce the same test polynomials to the same results
      let tests' = [x*x*x - 1, y*y*y - 1, x*x + y*y, x - y*y]
      mapM_ (\f -> reduce gb_full f @?= reduce gb_inc f) tests'
  ]

-- ---------------------------------------------------------------------------
-- Radical expression reduction tests
-- ---------------------------------------------------------------------------

radicalTests :: TestTree
radicalTests = testGroup "radical reduction"
  [ testCase "i^2 reduces to -1" $ do
      -- i^2 = -1 is the simplest relation
      let expr = Mul (Root 2 (Lit (-1))) (Root 2 (Lit (-1))) :: RadExpr Rational
          reduced = RG.reduceRadExpr expr
      toNormExpr reduced @?= toNormExpr (Lit (-1) :: RadExpr Rational)

  , testCase "sqrt2 * sqrt2 reduces to 2" $ do
      let expr = Mul (Root 2 (Lit 2)) (Root 2 (Lit 2)) :: RadExpr Rational
          reduced = RG.reduceRadExpr expr
      toNormExpr reduced @?= toNormExpr (Lit 2 :: RadExpr Rational)

  , testCase "sqrt3^2 + sqrt2^2 reduces to 5" $ do
      let s2 = Root 2 (Lit 2) :: RadExpr Rational
          s3 = Root 2 (Lit 3) :: RadExpr Rational
          expr = Add (Mul s3 s3) (Mul s2 s2)
          reduced = RG.reduceRadExpr expr
      toNormExpr reduced @?= toNormExpr (Lit 5 :: RadExpr Rational)

  , testCase "reduceRadExpr preserves already-reduced expression" $ do
      -- √2 + √3 has no relations to reduce
      let expr = Add (Root 2 (Lit 2)) (Root 2 (Lit 3)) :: RadExpr Rational
          reduced = RG.reduceRadExpr expr
      toNormExpr reduced @?= toNormExpr expr
  ]
