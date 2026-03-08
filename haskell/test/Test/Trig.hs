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
  ]
