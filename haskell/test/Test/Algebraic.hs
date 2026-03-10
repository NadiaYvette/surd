module Test.Algebraic (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Surd.Types
import Math.Polynomial.Univariate
import Surd.Algebraic.Number (AlgNum(..), algFromRational, algNeg, algAdd, algMul, algApprox)
import Surd.Radical.Pretty (pretty)
import Data.List (isInfixOf)
import Surd.Algebraic.Convert
import Surd.Radical.Eval (eval, evalComplex)
import Surd.Algebraic.RootIsolation (isolateRealRoots, rootInInterval, sturmCount)
import Surd.Polynomial.MinimalPolyTower (minimalPolyTower, collectRadicals)
import Math.Polynomial.TragerFactoring (factorSFOverExtension)
import Math.Field.Extension (mkExtField, generator, embed)
import Surd.Trig (cosExact, TrigResult(..))

tests :: TestTree
tests = testGroup "Algebraic"
  [ testGroup "RootIsolation"
    [ testCase "x - 3 has one root at 3" $ do
        let roots = isolateRealRoots (mkPoly [-3, 1])
        length roots @?= 1
        case roots of
          [r] -> rootInInterval r @?= Just 3
          _   -> assertFailure "expected 1 root"
    , testCase "x^2 - 2 has two roots" $ do
        let roots = isolateRealRoots (mkPoly [-2, 0, 1])
        length roots @?= 2
    , testCase "x^2 + 1 has no real roots" $ do
        let roots = isolateRealRoots (mkPoly [1, 0, 1])
        length roots @?= 0
    , testCase "Sturm count for x^2 - 2 on (-2, 2]" $ do
        sturmCount (mkPoly [-2, 0, 1]) (-2) 2 @?= 2
    ]
  , testGroup "AlgNum basics"
    [ testCase "rational 1/2" $ do
        let a = algFromRational (1/2)
        algApprox (1/100) a @?= (1/2)
        degree (anMinPoly a) @?= 1
    , testCase "negation" $ do
        let a = algFromRational 3
            b = algNeg a
        algApprox (1/100) b @?= (-3)
    , testCase "addition of rationals" $ do
        let a = algFromRational 2
            b = algFromRational 3
            c = algAdd a b
        algApprox (1/100) c @?= 5
    , testCase "multiplication of rationals" $ do
        let a = algFromRational 3
            b = algFromRational 7
            c = algMul a b
        algApprox (1/100) c @?= 21
    ]
  , testGroup "RadExpr -> AlgNum conversion"
    [ testCase "Lit 5 -> degree 1" $ do
        let a = radExprToAlgNum (Lit 5 :: RadExpr Rational)
        degree (anMinPoly a) @?= 1
    , testCase "sqrt(2) -> degree 2, minpoly x^2-2" $ do
        let a = radExprToAlgNum (Root 2 (Lit 2) :: RadExpr Rational)
        degree (anMinPoly a) @?= 2
        anMinPoly a @?= mkPoly [-2, 0, 1]
    , testCase "sqrt(2) approx is positive" $ do
        let a = radExprToAlgNum (Root 2 (Lit 2) :: RadExpr Rational)
            v = algApprox (1/1000) a
        assertBool "sqrt(2) > 0" (v > 0)
        assertBool "sqrt(2) > 1.4" (v > 14/10)
        assertBool "sqrt(2) < 1.5" (v < 15/10)
    , testCase "1 + sqrt(2) -> degree 2" $ do
        let e = Add (Lit 1) (Root 2 (Lit 2)) :: RadExpr Rational
            a = radExprToAlgNum e
        degree (anMinPoly a) @?= 2
    ]
  , testGroup "AlgNum -> RadExpr conversion"
    [ testCase "degree 1 -> rational" $ do
        let a = algFromRational (7/3)
        algNumToRadExpr a @?= Just (Lit (7/3))
    , testCase "sqrt(2) round-trips" $ do
        let e = Root 2 (Lit 2) :: RadExpr Rational
            a = radExprToAlgNum e
        case algNumToRadExpr a of
          Just _  -> return ()  -- got a radical form back
          Nothing -> assertFailure "should produce radical for degree 2"
    ]
  , testGroup "simplifyViaCanonical"
    [ testCase "sqrt(2)*sqrt(2) simplifies to 2" $ do
        let e = Mul (Root 2 (Lit 2)) (Root 2 (Lit 2)) :: RadExpr Rational
            s = simplifyViaCanonical e
        s @?= Lit 2
    ]
  , testGroup "cosExact via canonical"
    [ testCase "cos(pi/3) = 1/2 via AlgNum" $ do
        case cosExact 1 3 of
          Radical e -> do
            let a = radExprToAlgNum e
            degree (anMinPoly a) @?= 1
            algApprox (1/100) a @?= (1/2)
          MinPoly _ -> assertFailure "expected radical"
    , testCase "cos(pi/4) is degree 2 via AlgNum" $ do
        case cosExact 1 4 of
          Radical e -> do
            let a = radExprToAlgNum e
            degree (anMinPoly a) @?= 2
          MinPoly _ -> assertFailure "expected radical"
    , testCase "cos(pi/5) is degree 2 via AlgNum" $ do
        case cosExact 1 5 of
          Radical e -> do
            let a = radExprToAlgNum e
            degree (anMinPoly a) @?= 2
          MinPoly _ -> assertFailure "expected radical"
    ]
  , testGroup "Tower-based minimalPoly"
    [ testCase "collectRadicals on sqrt(2)+sqrt(3)" $ do
        let e = Add (Root 2 (Lit 2)) (Root 2 (Lit 3)) :: RadExpr Rational
            rads = collectRadicals e
        length rads @?= 2
    , testCase "collectRadicals shares repeated sqrt(2)" $ do
        let e = Add (Root 2 (Lit 2)) (Root 2 (Lit 2)) :: RadExpr Rational
            rads = collectRadicals e
        length rads @?= 1
    , testCase "minimalPolyTower of sqrt(2)" $ do
        let e = Root 2 (Lit 2) :: RadExpr Rational
            mp = minimalPolyTower e
        mp @?= mkPoly [-2, 0, 1]
    , testCase "minimalPolyTower of 1 + sqrt(2)" $ do
        let e = Add (Lit 1) (Root 2 (Lit 2)) :: RadExpr Rational
            mp = minimalPolyTower e
        degree mp @?= 2
    , testCase "minimalPolyTower of sqrt(2) + sqrt(3)" $ do
        let e = Add (Root 2 (Lit 2)) (Root 2 (Lit 3)) :: RadExpr Rational
            mp = minimalPolyTower e
        degree mp @?= 4
    , testCase "cbrt(2) via tower — degree 3" $ do
        let e = Root 3 (Lit 2) :: RadExpr Rational
            mp = minimalPolyTower e
        -- ∛2 has minpoly x^3 - 2
        mp @?= mkPoly [-2, 0, 0, 1]
    , testCase "sqrt(2) + cbrt(3) via tower — degree 6" $ do
        let e = Add (Root 2 (Lit 2)) (Root 3 (Lit 3)) :: RadExpr Rational
            mp = minimalPolyTower e
        degree mp @?= 6
    , testCase "cos(2π/7) via tower — degree 3" $ do
        case cosExact 2 7 of
          Radical e -> do
            let mp = minimalPolyTower e
            degree mp @?= 3
          MinPoly _ -> assertFailure "expected Radical"
    ]
  , testCase "collectRadicals on cos(2π/7)" $ do
      case cosExact 2 7 of
        Radical e -> do
          let rads = collectRadicals e
          length rads @?= 5
        MinPoly _ -> assertFailure "expected Radical"
  , testGroup "cos(2π/7) pipeline"
    [ testCase "minimalPolyTower finds degree 3" $ do
        case cosExact 2 7 of
          Radical e -> do
            let mp = minimalPolyTower e
            degree mp @?= 3
            -- monic form of 8x³+4x²-4x-1
            unPoly mp @?= [(-1)/8, (-1)/2, 1/2, 1]
          MinPoly _ -> assertFailure "expected Radical"
    , testCase "radExprToAlgNum gives degree 3" $ do
        case cosExact 2 7 of
          Radical e -> do
            let a = radExprToAlgNum e
            degree (anMinPoly a) @?= 3
            let v = algApprox (1/10000) a
            assertBool "cos(2π/7) ≈ 0.6235" (abs (fromRational v - (0.6235 :: Double)) < 0.01)
          MinPoly _ -> assertFailure "expected Radical"
    , testCase "algNumInfo shows correct summary" $ do
        case cosExact 2 7 of
          Radical e -> do
            let info = algNumInfo e
            putStrLn $ "\n" ++ info
            assertBool "info mentions degree 3" ("Degree: 3" `isInfixOf` info)
    , testCase "simplifyViaCanonical gives Cardano form" $ do
        case cosExact 2 7 of
          Radical e -> do
            let simplified = simplifyViaCanonical e
            putStrLn $ "\n  cos(2π/7) = " ++ pretty simplified
            -- Original has 5 radicals, simplified uses Cardano (cube roots of complex)
            let simpRads = collectRadicals simplified
            putStrLn $ "  Original radicals: " ++ show (length (collectRadicals e))
            putStrLn $ "  Simplified radicals: " ++ show (length simpRads)
            assertBool "simplified has fewer radicals" (length simpRads < length (collectRadicals e))
          MinPoly _ -> assertFailure "expected Radical"
    ]
  , testGroup "Degree 4 (Ferrari)"
    [ testCase "√(2+√3) is degree 4" $ do
        -- √(2+√3) has minpoly x⁴ - 4x² + 1
        let e = Root 2 (Add (Lit 2) (Root 2 (Lit 3))) :: RadExpr Rational
            a = radExprToAlgNum e
        degree (anMinPoly a) @?= 4
    , testCase "√(2+√3) round-trips via Ferrari" $ do
        let e = Root 2 (Add (Lit 2) (Root 2 (Lit 3))) :: RadExpr Rational
            a = radExprToAlgNum e
        case algNumToRadExpr a of
          Just rad -> do
            let orig = eval e :: Double
                new  = eval rad :: Double
            assertBool "values match" (abs (orig - new) < 1e-8)
          Nothing -> assertFailure "Ferrari should produce a radical for degree 4"
    , testCase "√2 + √3 is degree 4, round-trips" $ do
        let e = Add (Root 2 (Lit 2)) (Root 2 (Lit 3)) :: RadExpr Rational
            a = radExprToAlgNum e
        degree (anMinPoly a) @?= 4
        case algNumToRadExpr a of
          Just rad -> do
            let orig = eval e :: Double
                new  = eval rad :: Double
            assertBool "values match" (abs (orig - new) < 1e-8)
          Nothing -> assertFailure "Ferrari should produce a radical for degree 4"
    , testCase "cos(π/8) is degree 4" $ do
        case cosExact 1 8 of
          Radical e -> do
            let a = radExprToAlgNum e
            degree (anMinPoly a) @?= 4
            case algNumToRadExpr a of
              Just rad -> do
                let orig = cos (pi/8 :: Double)
                    new  = eval rad :: Double
                assertBool "cos(π/8) value matches" (abs (orig - new) < 1e-8)
              Nothing -> assertFailure "Ferrari should handle cos(π/8)"
          MinPoly _ -> assertFailure "expected radical"
    ]
  , testGroup "Trager factoring"
    [ testCase "x^2 - 3 factors over Q(sqrt(3))" $ do
        let mp = mkPoly [-3, 0, 1 :: Rational]
            field = mkExtField mp "α"
            alpha = generator field
            -- x^2 - 3 in Q(sqrt(3))[x] should factor as (x-α)(x+α)
            f = mkPoly [negate (embed field 3), embed field 0, embed field 1]
            factors = factorSFOverExtension field f
        length factors @?= 2
    , testCase "x^2 - 2 is irreducible over Q(sqrt(3))" $ do
        let mp = mkPoly [-3, 0, 1 :: Rational]
            field = mkExtField mp "α"
            f = mkPoly [negate (embed field 2), embed field 0, embed field 1]
            factors = factorSFOverExtension field f
        length factors @?= 1
    ]
  ]
