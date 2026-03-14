module Test

import Surd.GCD
import Surd.Rational
import Surd.Positive
import Surd.PrimeFactors
import Surd.Poly
import Surd.Cyclotomic
import Surd.Types
import Surd.Normalize
import Surd.Eval
import Surd.NormalForm
import Surd.Trig
import Surd.Extension

import Data.Nat
import Data.List
import Data.IORef
import Data.Maybe

%default covering

------------------------------------------------------------------------
-- Test harness
------------------------------------------------------------------------

record TestState where
  constructor MkTestState
  passed : Nat
  failed : Nat
  count  : Nat

initState : TestState
initState = MkTestState 0 0 0

test : IORef TestState -> String -> Bool -> IO ()
test ref name True  = do
  s <- readIORef ref
  writeIORef ref ({ passed $= (+ 1), count $= (+ 1) } s)
  putStrLn ("  PASS: " ++ name)
test ref name False = do
  s <- readIORef ref
  writeIORef ref ({ failed $= (+ 1), count $= (+ 1) } s)
  putStrLn ("  FAIL: " ++ name)

||| Approximate equality for Doubles.
approxEq : Double -> Double -> Bool
approxEq a b = abs (a - b) < 1.0e-10

------------------------------------------------------------------------
-- Rational arithmetic tests
------------------------------------------------------------------------

testRational : IORef TestState -> IO ()
testRational ref = do
  putStrLn ""
  putStrLn "--- Rational arithmetic ---"

  test ref "rat add: 1/2 + 1/3 = 5/6"
    (mkRat 1 2 + mkRat 1 3 == mkRat 5 6)

  test ref "rat mul: 2/3 * 3/4 = 1/2"
    (mkRat 2 3 * mkRat 3 4 == mkRat 1 2)

  test ref "rat inv: recip(2/3) = 3/2"
    (recip (mkRat 2 3) == mkRat 3 2)

  test ref "rat sub: 1 - 1/3 = 2/3"
    (mkRat 1 1 - mkRat 1 3 == mkRat 2 3)

  test ref "rat eq: 2/4 = 1/2"
    (mkRat 2 4 == mkRat 1 2)

  test ref "rat neg: -(1/3) = -1/3"
    (negate (mkRat 1 3) == mkRat (-1) 3)

------------------------------------------------------------------------
-- GCD tests
------------------------------------------------------------------------

testGCD : IORef TestState -> IO ()
testGCD ref = do
  putStrLn ""
  putStrLn "--- GCD ---"

  test ref "gcd 12 8 = 4"
    (gcdInteger 12 8 == 4)

  test ref "gcd 0 5 = 5"
    (gcdInteger 0 5 == 5)

  test ref "gcd 7 13 = 1 (coprime)"
    (gcdInteger 7 13 == 1)

  test ref "lcm 4 6 = 12"
    (lcmInteger 4 6 == 12)

------------------------------------------------------------------------
-- Positive tests
------------------------------------------------------------------------

testPositive : IORef TestState -> IO ()
testPositive ref = do
  putStrLn ""
  putStrLn "--- Positive ---"

  test ref "positive 5 = Just"
    (isJust (positive 5))

  test ref "positive 0 = Nothing"
    (isNothing (positive 0))

  test ref "positive 1: unPositive = 1"
    (case positive 1 of Just p => unPositive p == 1; Nothing => False)

------------------------------------------------------------------------
-- PrimeFactors tests
------------------------------------------------------------------------

testPrimeFactors : IORef TestState -> IO ()
testPrimeFactors ref = do
  putStrLn ""
  putStrLn "--- PrimeFactors ---"

  test ref "factorise 12 = [(2,2),(3,1)]"
    (case positive 12 of
       Just p => factorise p == [(2, 2), (3, 1)]
       Nothing => False)

  test ref "factorise 360 = [(2,3),(3,2),(5,1)]"
    (case positive 360 of
       Just p => factorise p == [(2, 3), (3, 2), (5, 1)]
       Nothing => False)

  test ref "isPrime 17 = True"
    (isPrime 17)

  test ref "isPrime 15 = False"
    (not (isPrime 15))

------------------------------------------------------------------------
-- Polynomial tests
------------------------------------------------------------------------

testPoly : IORef TestState -> IO ()
testPoly ref = do
  putStrLn ""
  putStrLn "--- Polynomial ---"

  let p1 : Poly Rational = mkPoly [mkRat 1 1, mkRat 2 1]  -- 1 + 2x
  let p2 : Poly Rational = mkPoly [mkRat 3 1, mkRat 4 1]  -- 3 + 4x

  test ref "poly add: (1+2x) + (3+4x) = 4+6x"
    (addPoly p1 p2 == mkPoly [mkRat 4 1, mkRat 6 1])

  test ref "poly mul: (1+2x) * (3+4x) = 3+10x+8x^2"
    (mulPoly p1 p2 == mkPoly [mkRat 3 1, mkRat 10 1, mkRat 8 1])

  test ref "poly degree: deg(1+2x) = 1"
    (degreeInt p1 == 1)

  test ref "poly eval: (1+2x) at x=3 => 7"
    (evalPoly p1 (mkRat 3 1) == mkRat 7 1)

  let p3 : Poly Rational = mkPoly [mkRat (-6) 1, mkRat 1 1, mkRat 1 1]  -- -6 + x + x^2
  let p4 : Poly Rational = mkPoly [mkRat (-2) 1, mkRat 1 1]  -- -2 + x
  let (q, r) = divModPoly p3 p4

  test ref "poly divmod: (-6+x+x^2) / (-2+x), quotient"
    (q == mkPoly [mkRat 3 1, mkRat 1 1])

  test ref "poly divmod: (-6+x+x^2) / (-2+x), remainder = 0"
    (r == zeroPoly)

  let g = gcdPoly p3 p4
  test ref "poly gcd: gcd(-6+x+x^2, -2+x) = x-2 (monic)"
    (g == mkPoly [mkRat (-2) 1, mkRat 1 1])

  test ref "poly diff: d/dx(1+2x+3x^2) = 2+6x"
    (diffPoly (mkPoly [mkRat 1 1, mkRat 2 1, mkRat 3 1]) ==
     mkPoly [mkRat 2 1, mkRat 6 1])

------------------------------------------------------------------------
-- RadExpr tests
------------------------------------------------------------------------

testRadExpr : IORef TestState -> IO ()
testRadExpr ref = do
  putStrLn ""
  putStrLn "--- RadExpr ---"

  let e1 : RadExpr Rational = Add (Lit (mkRat 1 2)) (Lit (mkRat 1 3))
  test ref "normalize lit add: 1/2 + 1/3 = 5/6"
    (normalize e1 == Lit (mkRat 5 6))

  let e2 : RadExpr Rational = Mul (Lit (mkRat 2 1)) (Lit (mkRat 3 1))
  test ref "normalize lit mul: 2 * 3 = 6"
    (normalize e2 == Lit (mkRat 6 1))

  -- sqrt(4) should normalize to 2
  let e3 : RadExpr Rational = Types.sqrt (Lit (mkRat 4 1))
  let n3 = normalize e3
  test ref "normalize sqrt(4) = 2"
    (n3 == Lit (mkRat 2 1))

  -- normalize is idempotent
  let e4 : RadExpr Rational = Add (Types.sqrt (Lit (mkRat 2 1))) (Types.sqrt (Lit (mkRat 2 1)))
  let n4 = normalize e4
  test ref "normalize idempotent: normalize(normalize(e)) = normalize(e)"
    (normalize n4 == n4)

  -- eval consistency
  let e5 : RadExpr Rational = Types.sqrt (Lit (mkRat 2 1))
  test ref "eval sqrt(2) ~ 1.414"
    (approxEq (eval e5) 1.4142135623730951)

  -- complex eval
  let e6 : RadExpr Rational = Types.sqrt (Lit (mkRat (-1) 1))
  let (re, im) = evalComplex e6
  test ref "evalComplex sqrt(-1) ~ (0, 1)"
    (approxEq re 0.0 && approxEq im 1.0)

------------------------------------------------------------------------
-- NormalForm tests
------------------------------------------------------------------------

testNormalForm : IORef TestState -> IO ()
testNormalForm ref = do
  putStrLn ""
  putStrLn "--- NormalForm ---"

  -- Round-trip: toNormExpr -> fromNormExpr for a literal
  let e1 : RadExpr Rational = Lit (mkRat 5 3)
  let nf1 = toNormExpr e1
  let rt1 = fromNormExpr nf1
  test ref "NF round-trip: Lit(5/3)"
    (rt1 == Lit (mkRat 5 3))

  -- Round-trip for sqrt(2)
  let e2 : RadExpr Rational = Types.sqrt (Lit (mkRat 2 1))
  let nf2 = toNormExpr e2
  let rt2 = fromNormExpr nf2
  test ref "NF round-trip: sqrt(2)"
    (rt2 == Root 2 (Lit (mkRat 2 1)))

  -- ImagUnit: sqrt(-1) should have ImagUnit atom
  let e3 : RadExpr Rational = Types.sqrt (Lit (mkRat (-1) 1))
  let nf3 = toNormExpr e3
  test ref "NF: sqrt(-1) is not zero"
    (not (normIsZero nf3))

  -- normAdd cancellation
  let one' = normLit (mkRat 1 1)
  let negOne' = normLit (mkRat (-1) 1)
  test ref "NF: 1 + (-1) = 0"
    (normIsZero (normAdd one' negOne'))

  -- normMul identity
  let atom2 = normAtom (RatRoot 2 (mkRat 3 1))
  test ref "NF: 1 * sqrt(3) = sqrt(3)"
    (normMul one' atom2 == atom2)

  -- normMul commutativity
  let atom5 = normAtom (RatRoot 2 (mkRat 5 1))
  test ref "NF: sqrt(3)*sqrt(5) = sqrt(5)*sqrt(3)"
    (normMul atom2 atom5 == normMul atom5 atom2)

------------------------------------------------------------------------
-- Trig tests
------------------------------------------------------------------------

testTrig : IORef TestState -> IO ()
testTrig ref = do
  putStrLn ""
  putStrLn "--- Trig ---"

  -- cos(pi/3) = 1/2
  test ref "cos(pi/3) = 1/2"
    (case cosExact 1 3 of
       Radical e => approxEq (eval e) 0.5
       _ => False)

  -- cos(pi/4) = sqrt(2)/2
  test ref "cos(pi/4) ~ sqrt(2)/2"
    (case cosExact 1 4 of
       Radical e => approxEq (eval e) (Prelude.sqrt 2.0 / 2.0)
       _ => False)

  -- cos(pi/2) = 0
  test ref "cos(pi/2) = 0"
    (case cosExact 1 2 of
       Radical e => approxEq (eval e) 0.0
       _ => False)

  -- sin(pi/6) = 1/2
  test ref "sin(pi/6) ~ 1/2"
    (case sinExact 1 6 of
       Radical e => approxEq (eval e) 0.5
       _ => False)

  -- cos(0) = 1
  test ref "cos(0) = 1"
    (case cosExact 0 1 of
       Radical e => approxEq (eval e) 1.0
       _ => False)

  -- cos(pi) = -1
  test ref "cos(pi) = -1"
    (case cosExact 1 1 of
       Radical e => approxEq (eval e) (-1.0)
       _ => False)

------------------------------------------------------------------------
-- Extension field tests
------------------------------------------------------------------------

testExtension : IORef TestState -> IO ()
testExtension ref = do
  putStrLn ""
  putStrLn "--- Extension field Q(sqrt(2)) ---"

  -- Q(sqrt(2)): minpoly = x^2 - 2
  let minp : Poly Rational = mkPoly [mkRat (-2) 1, mkRat 0 1, mkRat 1 1]
  let ef = mkExtField minp "sqrt2"
  let alpha = generator ef  -- sqrt(2)
  let one' = embedScalar ef (mkRat 1 1)

  -- alpha * alpha = 2
  let a2 = extMul alpha alpha
  let two' = embedScalar ef (mkRat 2 1)
  test ref "ext: sqrt(2)^2 = 2"
    (a2 == two')

  -- (1 + alpha)^2 = 3 + 2*alpha
  let onePlusAlpha = extAdd one' alpha
  let sq = extMul onePlusAlpha onePlusAlpha
  let expected = extAdd (embedScalar ef (mkRat 3 1)) (extMul (embedScalar ef (mkRat 2 1)) alpha)
  test ref "ext: (1+sqrt(2))^2 = 3+2sqrt(2)"
    (sq == expected)

  -- power: alpha^4 = 4 (since alpha^2 = 2, so alpha^4 = 4)
  let a4 = extPow alpha 4
  let four' = embedScalar ef (mkRat 4 1)
  test ref "ext: sqrt(2)^4 = 4"
    (a4 == four')

  -- (1+alpha)*(1-alpha) = 1-2 = -1
  let oneMinusAlpha = extSub one' alpha
  let prod = extMul onePlusAlpha oneMinusAlpha
  let negOne' = embedScalar ef (mkRat (-1) 1)
  test ref "ext: (1+sqrt(2))*(1-sqrt(2)) = -1"
    (prod == negOne')

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

main : IO ()
main = do
  putStrLn "=== Surd Idris 2 Tests ==="

  ref <- newIORef initState

  testGCD ref
  testRational ref
  testPositive ref
  testPrimeFactors ref
  testPoly ref
  testRadExpr ref
  testNormalForm ref
  testTrig ref
  testExtension ref

  s <- readIORef ref
  putStrLn ""
  putStrLn ("=== Results: " ++ show (passed s) ++ " passed, "
            ++ show (failed s) ++ " failed, "
            ++ show (count s) ++ " total ===")

  if failed s > 0
    then putStrLn "SOME TESTS FAILED"
    else putStrLn "ALL TESTS PASSED"
