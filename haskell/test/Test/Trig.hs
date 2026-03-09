module Test.Trig (tests) where

import Data.Complex (Complex(..), realPart, imagPart)
import Test.Tasty
import Test.Tasty.HUnit

import Surd.Trig
import Surd.Radical.Eval (eval, evalComplex)

tests :: TestTree
tests = testGroup "Trig"
  [ testGroup "Constructible (square roots only)"
    [ testCase "cos(0) = 1" $ do
        case cosExact 0 1 of
          Radical e -> eval e @?= (1.0 :: Double)
          _ -> assertFailure "expected radical"

    , testCase "cos(π/2) = 0" $ do
        case cosExact 1 2 of
          Radical e -> abs (eval e) < 1e-15 @? "should be ~0"
          _ -> assertFailure "expected radical"

    , testCase "cos(π/3) = 1/2" $ do
        case cosExact 1 3 of
          Radical e -> abs (eval e - 0.5) < 1e-15 @? "should be 1/2"
          _ -> assertFailure "expected radical"

    , testCase "cos(π/4) = √2/2" $ do
        case cosExact 1 4 of
          Radical e -> abs (eval e - sqrt 2 / 2) < 1e-15 @? "should be √2/2"
          _ -> assertFailure "expected radical"

    , testCase "cos(π/6) = √3/2" $ do
        case cosExact 1 6 of
          Radical e -> abs (eval e - sqrt 3 / 2) < 1e-15 @? "should be √3/2"
          _ -> assertFailure "expected radical"

    , testCase "cos(π/5)" $ do
        case cosExact 1 5 of
          Radical e -> abs (eval e - cos (pi / 5)) < 1e-10 @? "should match cos(π/5)"
          _ -> assertFailure "expected radical"

    , testCase "sin(π/3) = √3/2" $ do
        case sinExact 1 3 of
          Radical e -> abs (eval e - sqrt 3 / 2) < 1e-10 @? "should be √3/2"
          _ -> assertFailure "expected radical"

    , testCase "sin(π/6) = 1/2" $ do
        case sinExact 1 6 of
          Radical e -> abs (eval e - 0.5) < 1e-10 @? "should be 1/2"
          _ -> assertFailure "expected radical"

    , testCase "cos(2π/3) = -1/2" $ do
        case cosExact 2 3 of
          Radical e -> abs (eval e - (-0.5)) < 1e-15 @? "should be -1/2"
          _ -> assertFailure "expected radical"

    , testCase "cos(π) = -1" $ do
        case cosExact 1 1 of
          Radical e -> eval e @?= (-1.0 :: Double)
          _ -> assertFailure "expected radical"
    ]

  , testGroup "Non-constructible (via Gauss period descent)"
    [ testCase "cos(2π/7) — cubic descent" $ do
        -- p=7, p-1=6=2·3, needs square and cube roots
        case cosExact 2 7 of
          Radical e -> do
            let v = realPart (evalComplex e)
            abs (v - cos (2 * pi / 7)) < 1e-10 @?
              ("cos(2π/7) should be " ++ show (cos (2*pi/7)) ++ " but got " ++ show v)
          MinPoly _ -> assertFailure "expected radical, got MinPoly"

    , testCase "cos(2π/9) — prime power 3²" $ do
        case cosExact 2 9 of
          Radical e -> do
            let v = realPart (evalComplex e)
            abs (v - cos (2 * pi / 9)) < 1e-10 @?
              ("cos(2π/9) should be " ++ show (cos (2*pi/9)) ++ " but got " ++ show v)
          MinPoly _ -> assertFailure "expected radical, got MinPoly"

    , testCase "cos(2π/13) — p-1=12=2²·3" $ do
        case cosExact 2 13 of
          Radical e -> do
            let v = realPart (evalComplex e)
            abs (v - cos (2 * pi / 13)) < 1e-10 @?
              ("cos(2π/13) should be " ++ show (cos (2*pi/13)) ++ " but got " ++ show v)
          MinPoly _ -> assertFailure "expected radical, got MinPoly"

    , testCase "cos(π/7) — first quadrant" $ do
        case cosExact 1 7 of
          Radical e -> do
            let v = realPart (evalComplex e)
            abs (v - cos (pi / 7)) < 1e-10 @?
              ("cos(π/7) should be " ++ show (cos (pi/7)) ++ " but got " ++ show v)
          MinPoly _ -> assertFailure "expected radical, got MinPoly"

    , testCase "radical result is real (imaginary part ≈ 0)" $ do
        case cosExact 2 7 of
          Radical e -> do
            let c = evalComplex e
            abs (imagPart c) < 1e-6 @?
              ("should be real but imaginary part is " ++ show (imagPart c))
          MinPoly _ -> assertFailure "expected radical"
    ]

  , testGroup "Lagrange resolvent (q ≥ 5)"
    [ testCase "cos(2π/11) — φ(11)=10=2×5, needs quintic resolvent" $ do
        case cosExact 2 11 of
          Radical e -> do
            let v = realPart (evalComplex e)
            abs (v - cos (2 * pi / 11)) < 1e-8 @?
              ("cos(2π/11) should be " ++ show (cos (2*pi/11)) ++ " but got " ++ show v)
          MinPoly _ -> assertFailure "expected radical via Lagrange resolvent"

    , testCase "cos(2π/11) is real" $ do
        case cosExact 2 11 of
          Radical e -> do
            let c = evalComplex e
            abs (imagPart c) < 1e-6 @?
              ("should be real but imaginary part is " ++ show (imagPart c))
          MinPoly _ -> assertFailure "expected radical"

    , testCase "cos(2π/23) — φ(23)=22=2×11, needs q=11 resolvent" $ do
        case cosExact 2 23 of
          Radical e -> do
            let v = realPart (evalComplex e)
            abs (v - cos (2 * pi / 23)) < 1e-8 @?
              ("cos(2π/23) should be " ++ show (cos (2*pi/23)) ++ " but got " ++ show v)
          MinPoly _ -> assertFailure "expected radical via Lagrange resolvent"

    , testCase "cos(2π/31) — φ(31)=30=2×3×5" $ do
        case cosExact 2 31 of
          Radical e -> do
            let v = realPart (evalComplex e)
            abs (v - cos (2 * pi / 31)) < 1e-8 @?
              ("cos(2π/31) should be " ++ show (cos (2*pi/31)) ++ " but got " ++ show v)
          MinPoly _ -> assertFailure "expected radical via Lagrange resolvent"

    -- cos(2π/37) and cos(2π/41) verified manually but too slow for CI:
    -- cos(2π/37) φ=36=2²×3²: 143s (deep Gauss descent, quadratic+cubic only)
    -- cos(2π/41) φ=40=2³×5:  69s (quintic resolvent, large expression DAG)
    ]

  , testGroup "Composite n (CRT decomposition)"
    [ testCase "cos(2π/15) — 15=3×5, non-cyclic" $ do
        case cosExact 2 15 of
          Radical e -> do
            let v = realPart (evalComplex e)
            abs (v - cos (2 * pi / 15)) < 1e-10 @?
              ("cos(2π/15) should be " ++ show (cos (2*pi/15)) ++ " but got " ++ show v)
          MinPoly _ -> assertFailure "expected radical"

    , testCase "cos(2π/21) — 21=3×7" $ do
        case cosExact 2 21 of
          Radical e -> do
            let v = realPart (evalComplex e)
            abs (v - cos (2 * pi / 21)) < 1e-10 @?
              ("cos(2π/21) should be " ++ show (cos (2*pi/21)) ++ " but got " ++ show v)
          MinPoly _ -> assertFailure "expected radical"

    , testCase "cos(π/12) — n=24, 24=2³×3" $ do
        case cosExact 1 12 of
          Radical e -> do
            let v = eval e
            abs (v - cos (pi / 12)) < 1e-10 @?
              ("cos(π/12) should be " ++ show (cos (pi/12)) ++ " but got " ++ show v)
          MinPoly _ -> assertFailure "expected radical"

    , testCase "cos(2π/32) — 32=2⁵, power of 2" $ do
        case cosExact 2 32 of
          Radical e -> do
            let v = eval e
            abs (v - cos (2 * pi / 32)) < 1e-10 @?
              ("cos(2π/32) should be " ++ show (cos (2*pi/32)) ++ " but got " ++ show v)
          MinPoly _ -> assertFailure "expected radical"

    , testCase "cos(2π/45) — 45=3²×5" $ do
        case cosExact 2 45 of
          Radical e -> do
            let v = realPart (evalComplex e)
            abs (v - cos (2 * pi / 45)) < 1e-10 @?
              ("cos(2π/45) should be " ++ show (cos (2*pi/45)) ++ " but got " ++ show v)
          MinPoly _ -> assertFailure "expected radical"

    , testCase "cos(2π/60) — 60=2²×3×5" $ do
        case cosExact 2 60 of
          Radical e -> do
            let v = eval e
            abs (v - cos (2 * pi / 60)) < 1e-10 @?
              ("cos(2π/60) should be " ++ show (cos (2*pi/60)) ++ " but got " ++ show v)
          MinPoly _ -> assertFailure "expected radical"
    ]
  ]
