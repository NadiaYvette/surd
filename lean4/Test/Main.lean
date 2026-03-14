/-
  Test.Main — Test suite for Surd.
-/
import Surd
import Surd.Notation

open Std.Internal
open Surd

structure TestState where
  passed : Nat := 0
  failed : Nat := 0

def check (state : IO.Ref TestState) (name : String) (b : Bool) : IO Unit := do
  if b then
    IO.println s!"  PASS: {name}"
    state.modify fun s => { s with passed := s.passed + 1 }
  else
    IO.println s!"  FAIL: {name}"
    state.modify fun s => { s with failed := s.failed + 1 }

private def ratFrac (n d : Int) : Rat := (n : Rat) / (d : Rat)

-- ========================================================================
-- Rational arithmetic tests
-- ========================================================================

def testRat (state : IO.Ref TestState) : IO Unit := do
  IO.println "--- Rational arithmetic ---"
  let r12 := ratFrac 1 2
  let r13 := ratFrac 1 3
  let r56 := ratFrac 5 6
  check state "rat_add_1/2+1/3=5/6" (r12 + r13 == r56)

  let r23 := ratFrac 2 3
  let r34 := ratFrac 3 4
  let rHalf := ratFrac 1 2
  check state "rat_mul_2/3*3/4=1/2" (r23 * r34 == rHalf)

  check state "rat_neg" (-(r12) + r12 == (0 : Rat))

-- ========================================================================
-- Positive tests
-- ========================================================================

def testPositive (state : IO.Ref TestState) : IO Unit := do
  IO.println "--- Positive ---"
  let p3 : Positive := 3
  let p5 : Positive := 5
  check state "pos_add" ((p3 + p5).val == 8)
  check state "pos_mul" ((p3 * p5).val == 15)
  check state "pos_beq" (p3 == p3)
  check state "pos_neq" (!(p3 == p5))

-- ========================================================================
-- Polynomial tests
-- ========================================================================

def testPoly (state : IO.Ref TestState) : IO Unit := do
  IO.println "--- Polynomials ---"
  -- x + 1
  let p1 : Poly Rat := Poly.mkPoly #[1, 1]
  -- x - 1
  let p2 : Poly Rat := Poly.mkPoly #[-1, 1]
  -- (x+1)(x-1) = x^2 - 1
  let prod := Poly.mul p1 p2
  check state "poly_mul_degree" (prod.degree == some 2)
  check state "poly_mul_coeffs" (prod.coeffs == #[-1, 0, 1])

  -- addition: (x+1) + (x-1) = 2x
  let sum := Poly.add p1 p2
  check state "poly_add" (sum.coeffs == #[0, 2])

  -- subtraction: (x+1) - (x-1) = 2
  let diff := Poly.sub p1 p2
  check state "poly_sub" (diff.coeffs == #[2])

  -- divMod: (x^2 - 1) / (x - 1) = (x + 1, 0)
  let (q, r) := Poly.divMod prod p2
  check state "poly_divmod_quot" (q.coeffs == #[1, 1])
  check state "poly_divmod_rem" (r.isZero)

  -- GCD: gcd(x^2 - 1, x - 1) should be monic x-1 (up to scalar)
  let g := Poly.gcd prod p2
  check state "poly_gcd" (g.coeffs == #[-1, 1])

  -- Zero polynomial
  check state "poly_zero_isZero" ((Poly.zero : Poly Rat).isZero)
  check state "poly_zero_degree" ((Poly.zero : Poly Rat).degree == (none : Option Nat))

  -- eval: (x+1) at x=3 should be 4
  check state "poly_eval" (Poly.eval p1 (3 : Rat) == (4 : Rat))

-- ========================================================================
-- RadExpr tests
-- ========================================================================

def testRadExpr (state : IO.Ref TestState) : IO Unit := do
  IO.println "--- RadExpr ---"
  let e1 : RadExpr Rat := .lit 2
  let e2 : RadExpr Rat := .root 2 (.lit 3)  -- sqrt 3
  check state "radexpr_beq_self" (e1 == e1)
  check state "radexpr_neq" (!(e1 == e2))

  -- Test depth
  check state "radexpr_depth_lit" (RadExpr.depth e1 == 0)
  check state "radexpr_depth_root" (RadExpr.depth e2 == 1)
  let e3 := RadExpr.add e1 e2
  check state "radexpr_depth_add" (RadExpr.depth e3 == 2)

  -- Test size
  check state "radexpr_size_lit" (RadExpr.size e1 == 1)
  check state "radexpr_size_add" (RadExpr.size e3 == 4)

  -- Test radical count
  check state "radexpr_radcount" (RadExpr.radicalCount e3 == 1)

  -- Test notation (scoped)
  let sq3 : RadExpr Rat := .root 2 (.lit 3)
  check state "notation_sqrt" (sq3 == RadExpr.root 2 (.lit (3 : Rat)))

-- ========================================================================
-- Normalization tests
-- ========================================================================

def testNormalize (state : IO.Ref TestState) : IO Unit := do
  IO.println "--- Normalization ---"
  -- 2 + 3 should fold to 5
  let e : RadExpr Rat := RadExpr.add (.lit 2) (.lit 3)
  let n := normalize e
  check state "fold_constants" (n == .lit 5)

  -- 0 * x should fold to 0
  let e2 : RadExpr Rat := RadExpr.mul (.lit 0) (.root 2 (.lit 5))
  let n2 := normalize e2
  check state "mul_zero" (n2 == .lit 0)

  -- 1 * x should fold to x
  let e3 : RadExpr Rat := RadExpr.mul (.lit 1) (.root 2 (.lit 5))
  let n3 := normalize e3
  check state "mul_one" (n3 == .root 2 (.lit 5))

-- ========================================================================
-- NormalForm tests
-- ========================================================================

def testNormalForm (state : IO.Ref TestState) : IO Unit := do
  IO.println "--- NormalForm ---"
  -- toNormExpr of a literal
  let ne := toNormExpr (.lit (3 : Rat))
  check state "nf_literal" (ne.terms.length == 1)

  -- sqrt(2) * sqrt(2) = 2 in NF
  let sq2 : RadExpr Rat := .root 2 (.lit 2)
  let sq2sq := toNormExpr (.mul sq2 sq2)
  let coeff := normCoeff sq2sq
  check state "nf_sqrt2_squared" (coeff == some (2 : Rat))

  -- round-trip: fromNormExpr (toNormExpr (lit 5)) should be lit 5
  let rt := fromNormExpr (toNormExpr (.lit (5 : Rat)))
  check state "nf_roundtrip_lit" (rt == .lit 5)

-- ========================================================================
-- Eval tests
-- ========================================================================

def testEval (state : IO.Ref TestState) : IO Unit := do
  IO.println "--- Eval ---"
  let sq2 : RadExpr Rat := .root 2 (.lit 2)
  let v := evalFloat sq2
  check state "eval_sqrt2" (Float.abs (v - 1.41421356) < 0.0001)

  let neg3 : RadExpr Rat := .neg (.lit 3)
  let vn := evalFloat neg3
  check state "eval_neg" (vn == -3.0)

-- ========================================================================
-- PrimeFactors tests
-- ========================================================================

def testPrimeFactors (state : IO.Ref TestState) : IO Unit := do
  IO.println "--- PrimeFactors ---"
  let f12 : List (Nat × Nat) := factoriseNat 12
  check state "factor_12_len" (f12.length == 2)
  check state "factor_12_first" (f12.head? == some (2, 2))
  check state "factor_1" ((factoriseNat 1 : List (Nat × Nat)).length == 0)
  let f13 : List (Nat × Nat) := factoriseNat 13
  check state "factor_prime" (f13.length == 1 && f13.head? == some (13, 1))
  check state "isPrime_7" (isPrime 7)
  check state "isPrime_9" (!isPrime 9)
  check state "isPrime_2" (isPrime 2)

-- ========================================================================
-- Interval tests
-- ========================================================================

def testInterval (state : IO.Ref TestState) : IO Unit := do
  IO.println "--- Interval ---"
  let iv := Interval.fromRat (3 : Rat)
  check state "iv_point" (iv.lo == (3 : Rat) && iv.hi == (3 : Rat))
  let iv2 := Interval.iadd iv (Interval.fromRat 2)
  check state "iv_add" (iv2.lo == (5 : Rat))
  let sq := Interval.isqrt (Interval.fromRat 4)
  -- sqrt(4) should contain 2
  check state "iv_sqrt_contains_2" (sq.lo ≤ (2 : Rat) && (2 : Rat) ≤ sq.hi)

-- ========================================================================
-- DAG tests
-- ========================================================================

def testDAG (state : IO.Ref TestState) : IO Unit := do
  IO.println "--- DAG ---"
  let e : RadExpr Rat := .add (.root 2 (.lit 2)) (.root 2 (.lit 2))
  let dag := RadDAG.toDAG e
  -- Two sqrt(2) nodes should be shared: 3 nodes total (lit 2, root 2, add)
  check state "dag_sharing" (dag.nodes.size == 3)
  check state "dag_depth" (RadDAG.dagDepth dag == 2)

-- ========================================================================
-- Main
-- ========================================================================

def main : IO Unit := do
  IO.println "=== Surd Lean 4 Tests ==="
  let state ← IO.mkRef (TestState.mk 0 0)

  testRat state
  testPositive state
  testPoly state
  testRadExpr state
  testNormalize state
  testNormalForm state
  testEval state
  testPrimeFactors state
  testInterval state
  testDAG state

  let final ← state.get
  IO.println ""
  IO.println s!"=== Results: {final.passed} passed, {final.failed} failed ==="
  if final.failed > 0 then
    IO.Process.exit 1
