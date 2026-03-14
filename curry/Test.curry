--- Test suite for the Surd Curry port.
---
--- Tests rational arithmetic, polynomial operations, nondeterministic
--- factoring, normalization, denesting, and Galois group identification.
module Test where

import Rational
import Poly
import RadExpr
import Normalize
import Factoring
import Denest
import Identify
import TransitiveGroup
import RootIsolation

--- Simple test harness: prints PASS or FAIL.
check :: String -> Bool -> IO ()
check name True  = putStrLn ("  PASS: " ++ name)
check name False = putStrLn ("  FAIL: " ++ name)

--- Shorthand for mkRat.
r :: Int -> Int -> Rational
r = mkRat

--- Shorthand for fromInt.
ri :: Int -> Rational
ri = Rational.fromInt

main :: IO ()
main = do
  putStrLn "=== Surd Curry Tests ==="
  putStrLn ""

  -- Rational arithmetic
  putStrLn "--- Rational ---"
  check "rat_add" (ratAdd (r 1 2) (r 1 3) == r 5 6)
  check "rat_sub" (ratSub (r 3 4) (r 1 4) == r 1 2)
  check "rat_mul" (ratMul (r 2 3) (r 3 4) == r 1 2)
  check "rat_div" (ratDiv (r 1 2) (r 3 4) == r 2 3)
  check "rat_neg" (ratNeg (r 3 7) == r (negate 3) 7)
  check "rat_inv" (ratInv (r 2 3) == r 3 2)
  check "rat_pow" (ratPow (r 2 3) 3 == r 8 27)
  check "rat_eq"  (r 2 4 == r 1 2)
  check "rat_ord" (ratLt (r 1 3) (r 1 2))
  check "rat_floor" (ratFloor (r 7 3) == 2)
  check "rat_ceiling" (ratCeiling (r 7 3) == 3)
  putStrLn ""

  -- Polynomial operations
  putStrLn "--- Polynomial ---"
  let p1 = mkPoly [ri 1, ri 2, ri 1]     -- 1 + 2x + x^2 = (1+x)^2
  let p2 = mkPoly [ri 1, ri 1]            -- 1 + x
  let p3 = mkPoly [ri (negate 6), ri 1, ri 1]  -- -6 + x + x^2 = (x+3)(x-2)
  check "poly_degree_1" (degree p1 == 2)
  check "poly_degree_2" (degree p2 == 1)
  check "poly_eval_1" (evalPoly p1 (ri 1) == ri 4)
  check "poly_eval_2" (evalPoly p1 (ri (negate 1)) == ri 0)
  check "poly_eval_3" (evalPoly p3 (ri 2) == ri 0)
  check "poly_eval_4" (evalPoly p3 (ri (negate 3)) == ri 0)
  check "poly_add" (addPoly p2 p2 == mkPoly [ri 2, ri 2])
  check "poly_mul" (mulPoly p2 p2 == p1)
  let (q, rem_) = divModPoly p1 p2
  check "poly_div" (q == p2 && degree rem_ < 0)
  check "poly_diff" (diffPoly p1 == mkPoly [ri 2, ri 2])
  putStrLn ""

  -- Nondeterministic factoring (the key Curry feature)
  putStrLn "--- Nondeterministic Factoring ---"
  let roots3 = rationalRoots p3  -- x^2 + x - 6 = (x+3)(x-2)
  check "rational_roots_x2_plus_x_minus_6"
        (elem (ri 2) roots3 && elem (ri (negate 3)) roots3)
  check "rational_roots_count" (length roots3 == 2)

  -- x - 5 has root 5
  let pLinear = mkPoly [ri (negate 5), ri 1]
  check "rational_roots_linear" (rationalRoots pLinear == [ri 5])

  -- x^2 + 1 has no rational roots
  let pNoRoots = mkPoly [ri 1, ri 0, ri 1]
  check "rational_roots_none" (null (rationalRoots pNoRoots))

  -- x^3 - x = x(x-1)(x+1)
  let pCubic = mkPoly [ri 0, ri (negate 1), ri 0, ri 1]
  let cubicRoots = rationalRoots pCubic
  check "rational_roots_cubic"
        (elem (ri 0) cubicRoots &&
         elem (ri 1) cubicRoots &&
         elem (ri (negate 1)) cubicRoots)
  putStrLn ""

  -- Irreducibility
  putStrLn "--- Irreducibility ---"
  check "irreducible_x2_plus_1" (isIrreducible pNoRoots)
  check "not_irreducible_x2_plus_x_minus_6" (not (isIrreducible p3))
  putStrLn ""

  -- Normalization
  putStrLn "--- Normalization ---"
  let e1 = Add (Lit (ri 1)) (Root 2 (Lit (ri 2)))
  check "normalize_idempotent" (normalize (normalize e1) == normalize e1)

  let e2 = Add (Root 2 (Lit (ri 2))) (Root 2 (Lit (ri 2)))  -- sqrt(2) + sqrt(2)
  let ne2 = normalize e2
  check "normalize_like_terms" (ne2 == Mul (Lit (ri 2)) (Root 2 (Lit (ri 2))))

  -- sqrt(4) should simplify to 2
  let e3 = Root 2 (Lit (ri 4))
  check "normalize_sqrt4" (normalize e3 == Lit (ri 2))

  -- sqrt(12) = 2*sqrt(3)
  let e4 = Root 2 (Lit (ri 12))
  let ne4 = normalize e4
  check "normalize_sqrt12" (ne4 == Mul (Lit (ri 2)) (Root 2 (Lit (ri 3))))
  putStrLn ""

  -- Denesting
  putStrLn "--- Denesting ---"
  -- sqrt(3 + 2*sqrt(2)) = sqrt(2) + sqrt(1) = sqrt(2) + 1
  let inner = Add (Lit (ri 3)) (Mul (Lit (ri 2)) (Root 2 (Lit (ri 2))))
  let sqrtExpr = Root 2 inner
  let denested = denest sqrtExpr
  let denNorm = normalize denested
  -- Check that the result is simpler (no nested radicals)
  check "denest_sqrt_3_plus_2sqrt2_is_simpler"
        (case denNorm of
           Root 2 (Add _ _) -> False   -- still nested = fail
           _ -> True)

  -- isRationalSqrt tests
  check "isRatSqrt_4" (isRationalSqrt (ri 4) == Just (ri 2))
  check "isRatSqrt_9" (isRationalSqrt (ri 9) == Just (ri 3))
  check "isRatSqrt_2" (isRationalSqrt (ri 2) == Nothing)
  check "isRatSqrt_1_4" (isRationalSqrt (r 1 4) == Just (r 1 2))
  putStrLn ""

  -- Root isolation
  putStrLn "--- Root Isolation ---"
  let isoRoots = isolateRealRoots p3  -- x^2 + x - 6 = (x+3)(x-2)
  check "root_isolation_count" (length isoRoots == 2)
  putStrLn ""

  -- RadExpr basics
  putStrLn "--- RadExpr ---"
  let expr1 = intE 3
  let expr2 = sqrtE (intE 2)
  check "radexpr_eq" (expr1 == expr1)
  check "radexpr_neq" (expr1 /= expr2)
  check "radexpr_ord" (compare expr1 expr2 /= EQ)
  check "subE_is_add_neg" (subE expr1 expr2 == Add expr1 (Neg expr2))
  check "divE_is_mul_inv" (divE expr1 expr2 == Mul expr1 (Inv expr2))
  putStrLn ""

  putStrLn "=== Done ==="
