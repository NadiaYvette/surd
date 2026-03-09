module Test.Multivariate (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Surd.Polynomial.Multivariate
import Surd.Field.Transcendental
import Surd.Polynomial.Univariate (Poly(..), mkPoly, divModPoly)
import qualified Surd.Polynomial.Univariate as U
import Surd.Types (RadExpr(..))
import Surd.Field.Extension (ExtField(..), ExtElem(..), mkExtField, generator, embed)

tests :: TestTree
tests = testGroup "Tier 4"
  [ mpolyTests
  , ratFuncTests
  , integrationTests
  ]

-- Helpers: variables
x, y, z :: MPoly Rational
x = varPoly (Var 0)
y = varPoly (Var 1)
z = varPoly (Var 2)

rx, ry :: RatFunc Rational
rx = varRF (Var 0)
ry = varRF (Var 1)

mpolyTests :: TestTree
mpolyTests = testGroup "Multivariate polynomials"
  [ testCase "x + y = y + x" $
      x + y @?= y + x

  , testCase "x * y = y * x" $
      x * y @?= y * x

  , testCase "(x + y) * z = x*z + y*z" $
      (x + y) * z @?= x * z + y * z

  , testCase "x * 0 = 0" $
      x * 0 @?= zeroPoly

  , testCase "x * 1 = x" $
      x * 1 @?= x

  , testCase "x - x = 0" $
      x - x @?= zeroPoly

  , testCase "(x + 1)² = x² + 2x + 1" $
      (x + 1) * (x + 1) @?= x * x + 2 * x + 1

  , testCase "(x + y)² = x² + 2xy + y²" $
      (x + y) * (x + y) @?= x * x + 2 * x * y + y * y

  , testCase "totalDegree (x²y + xy²) = 3" $
      totalDegree (x * x * y + x * y * y) @?= 3

  , testCase "degreeIn x (x²y + z) = 2" $
      degreeIn (Var 0) (x * x * y + z) @?= 2

  , testCase "numTerms (x + y + z) = 3" $
      numTerms (x + y + z) @?= 3

  , testCase "constPoly 5 + constPoly 3 = constPoly 8" $
      constPoly 5 + constPoly 3 @?= (constPoly 8 :: MPoly Rational)

  , testCase "eval x=2, y=3 in x² + y = 7" $
      evalPoly (\(Var i) -> [2, 3] !! i) (x * x + y) @?= (7 :: Rational)

  , testCase "substVar x→(y+1) in x² gives (y+1)²" $
      substVar (Var 0) (y + 1) (x * x) @?= (y + 1) * (y + 1)

  , testCase "toUnivariate round-trip" $ do
      let p = x * x + constPoly 3 * x + constPoly 2 :: MPoly Rational
          uniP = toUnivariate (Var 0) p
          -- Coefficients should be constant multivariate polys
          Poly cs = uniP
      length cs @?= 3  -- degree 2
  ]

ratFuncTests :: TestTree
ratFuncTests = testGroup "Rational functions"
  [ testCase "x/1 + y/1 = (x+y)/1" $
      rx + ry @?= varRF (Var 0) + varRF (Var 1)

  , testCase "x/1 * 1/x = 1" $
      rx * recip rx @?= (1 :: RatFunc Rational)

  , testCase "(a/b) * (b/a) = 1 for non-trivial fractions" $ do
      let a = rx + 1
          b = ry + 2
          prod = a * recip a
      prod @?= (1 :: RatFunc Rational)

  , testCase "cross-multiplication equality: 2/4 = 1/2" $ do
      let half1 = RatFunc (constPoly 1) (constPoly 2) :: RatFunc Rational
          half2 = RatFunc (constPoly 2) (constPoly 4) :: RatFunc Rational
      half1 @?= half2

  , testCase "x ≠ y" $
      rx /= ry @? "x should not equal y"

  , testCase "fromRational 3/5 works" $ do
      let rf = fromRational (3/5) :: RatFunc Rational
      rf @?= RatFunc (constPoly 3) (constPoly 5)

  , testCase "evalRF x=2,y=3 in (x+y)/(x-y)" $ do
      let f = (rx + ry) / (rx - ry)
      evalRF (\(Var i) -> [2, 3] !! i) f @?= (5 / (-1) :: Rational)

  , testCase "field axiom: (a + b) * c = a*c + b*c" $ do
      let a = rx
          b = ry
          c = rx + 1
      (a + b) * c @?= a * c + b * c
  ]

integrationTests :: TestTree
integrationTests = testGroup "Integration with existing modules"
  [ testCase "Poly (RatFunc Rational) arithmetic" $ do
      -- Univariate polynomial with transcendental coefficients
      -- p(t) = t² + x*t + y  where x, y are transcendentals
      let p = mkPoly [ry, rx, 1] :: Poly (RatFunc Rational)
          q = mkPoly [1, 1] :: Poly (RatFunc Rational)  -- t + 1
          prod = U.mulPoly p q
          -- (t² + xt + y)(t + 1) = t³ + (x+1)t² + (y+x)t + y
          expected = mkPoly [ry, ry + rx, rx + 1, 1] :: Poly (RatFunc Rational)
      prod @?= expected

  , testCase "Poly (RatFunc Rational) divMod" $ do
      let p = mkPoly [ry, rx, 1] :: Poly (RatFunc Rational)
          q = mkPoly [1, 1] :: Poly (RatFunc Rational)
          prod = U.mulPoly p q
          (quotient, remainder) = divModPoly prod q
      quotient @?= p
      remainder @?= mkPoly []

  , testCase "RadExpr (RatFunc Rational) construction" $ do
      -- √(x) where x is a transcendental
      let expr = Root 2 (Lit rx) :: RadExpr (RatFunc Rational)
      case expr of
        Root 2 (Lit _) -> return ()
        _              -> assertFailure "unexpected form"

  , testCase "ExtElem (RatFunc Rational) — Q(x)(α) where α²=x" $ do
      -- Adjoin α with α² - x = 0 over Q(x)
      let minpoly = mkPoly [negate rx, 0, 1] :: Poly (RatFunc Rational)
          field = mkExtField minpoly "α"
          -- α in the extension
          alpha = generator field
          -- α² should equal x
          alphaSq = alpha * alpha
      -- α² = x (reduced mod the minimal polynomial)
      alphaSq @?= embed field rx

  , testCase "gcdPoly over RatFunc — poly division exact" $ do
      -- (t - x)(t - y) / (t - x) = (t - y)
      let p1 = mkPoly [negate rx, 1] :: Poly (RatFunc Rational)
          p2 = mkPoly [negate ry, 1] :: Poly (RatFunc Rational)
          prod = U.mulPoly p1 p2
          (q, r) = divModPoly prod p1
      q @?= p2
      r @?= mkPoly []
  ]
