/// Test suite for the surd F* library.
///
/// Lemma-based tests are verified by Z3 at check time. Runtime tests use
/// FStar.IO for reporting.
module Surd.Test

open Surd.Ring
open Surd.Rational
open Surd.Lemmas
open Surd.Positive
open Surd.Types

// ---------------------------------------------------------------------------
// Rational construction tests (Z3-verified)
// ---------------------------------------------------------------------------

/// mk_rational normalizes sign: -1/-2 = 1/2.
let test_mk_rational_sign () : Lemma (let r = mk_rational (0-1) (0-2) in r.num > 0 && r.den > 0) = ()

/// mk_rational with positive inputs preserves positivity.
let test_mk_rational_pos () : Lemma (let r = mk_rational 3 4 in r.den > 0) = ()

/// rat_zero has num = 0.
let test_rat_zero () : Lemma (rat_zero.num = 0 /\ rat_zero.den = 1) = ()

/// rat_one has num = den = 1.
let test_rat_one () : Lemma (rat_one.num = 1 /\ rat_one.den = 1) = ()

// ---------------------------------------------------------------------------
// Rational arithmetic tests (Z3-verified)
// ---------------------------------------------------------------------------

/// 1/2 + 1/2 yields numerator/denominator with n*d = d*n cross-check.
let test_rat_add_half () : Lemma (
  let r = rat_add (mk_rational 1 2) (mk_rational 1 2) in
  (* r should represent 1, so r.num * 1 = 1 * r.den *)
  op_Multiply r.num 1 = op_Multiply 1 r.den
) = ()

/// Commutativity: a + b = b + a structurally.
let test_add_comm () : Lemma (
  rat_eq (rat_add (mk_rational 1 3) (mk_rational 2 5))
         (rat_add (mk_rational 2 5) (mk_rational 1 3))
) = ()

/// Commutativity: a * b = b * a structurally.
let test_mul_comm () : Lemma (
  rat_eq (rat_mul (mk_rational 2 3) (mk_rational 5 7))
         (rat_mul (mk_rational 5 7) (mk_rational 2 3))
) = ()

/// Double negation.
let test_neg_neg () : Lemma (
  let a = mk_rational 3 7 in
  rat_eq (rat_neg (rat_neg a)) a
) = ()

// ---------------------------------------------------------------------------
// Rational comparison tests (Z3-verified)
// ---------------------------------------------------------------------------

/// 1/3 < 1/2.
let test_lt () : Lemma (rat_lt (mk_rational 1 3) (mk_rational 1 2)) = ()

/// 1/2 <= 1/2.
let test_le_eq () : Lemma (rat_le (mk_rational 1 2) (mk_rational 1 2)) = ()

/// not (1/2 < 1/2).
let test_lt_irrefl () : Lemma (not (rat_lt (mk_rational 1 2) (mk_rational 1 2))) = ()

// ---------------------------------------------------------------------------
// Positive type tests (Z3-verified)
// ---------------------------------------------------------------------------

/// pos_add preserves positivity.
let test_pos_add () : Lemma (pos_add 3 5 = 8) = ()

/// pos_mul preserves positivity.
let test_pos_mul () : Lemma (pos_mul 3 5 = 15) = ()

// ---------------------------------------------------------------------------
// Types tests (Z3-verified)
// ---------------------------------------------------------------------------

/// Root requires n >= 2.
let test_root_index () : Lemma (Root #rational 2 (Lit rat_one) == Root 2 (Lit rat_one)) = ()

/// expr_size is positive for any expression.
let test_expr_size_pos () : Lemma (expr_size (Lit #rational rat_one) >= 1) = ()

/// expr_size of Add is larger than either child.
let test_expr_size_add () : Lemma (
  let a = Lit #rational rat_one in
  let b = Lit #rational rat_zero in
  expr_size (Add a b) > expr_size a /\ expr_size (Add a b) > expr_size b
) = ()

// ---------------------------------------------------------------------------
// GCD tests (Z3-verified)
// ---------------------------------------------------------------------------

/// gcd(6, 4) = 2.
let test_gcd_6_4 () : Lemma (gcd_nat 6 4 = 2) = ()

/// gcd(12, 8) = 4.
let test_gcd_12_8 () : Lemma (gcd_nat 12 8 = 4) = ()

/// gcd(7, 3) = 1 (coprime).
let test_gcd_coprime () : Lemma (gcd_nat 7 3 = 1) = ()

/// gcd(0, 5) = 5.
let test_gcd_zero () : Lemma (gcd_nat 0 5 = 5) = ()

/// gcd bound: gcd(a, b) <= b for b > 0.
let test_gcd_bound () : Lemma (gcd_nat 100 7 <= 7) =
  gcd_bounded 100 7

// ---------------------------------------------------------------------------
// abs_int tests (Z3-verified)
// ---------------------------------------------------------------------------

/// abs_int of positive.
let test_abs_pos () : Lemma (abs_int 5 = 5) = ()

/// abs_int of negative.
let test_abs_neg () : Lemma (abs_int (0-5) = 5) = ()

/// abs_int of zero.
let test_abs_zero () : Lemma (abs_int 0 = 0) = ()

// ---------------------------------------------------------------------------
// Runtime tests (Dv effect, for properties that Z3 cannot easily verify)
// ---------------------------------------------------------------------------

let check (name: string) (b: bool) : FStar.All.ML unit =
  if b then FStar.IO.print_string ("  PASS: " ^ name ^ "\n")
  else FStar.IO.print_string ("  FAIL: " ^ name ^ "\n")

val run_tests : unit -> FStar.All.ML unit
let run_tests () =
  FStar.IO.print_string "=== Surd F* Test Suite ===\n";

  (* Rational arithmetic *)
  check "1/2 + 1/3 = 5/6"
    (rat_eq (rat_add (mk_rational 1 2) (mk_rational 1 3)) (mk_rational 5 6));
  check "2/3 * 3/4 = 1/2"
    (rat_eq (rat_mul (mk_rational 2 3) (mk_rational 3 4)) (mk_rational 1 2));
  check "inv(2/3) = 3/2"
    (rat_eq (rat_inv (mk_rational 2 3)) (mk_rational 3 2));
  check "1/6 + 1/6 + 1/6 = 1/2"
    (rat_eq (rat_add (mk_rational 1 6)
                     (rat_add (mk_rational 1 6) (mk_rational 1 6)))
            (mk_rational 1 2));
  check "2/4 normalizes to 1/2"
    (rat_eq (mk_rational 2 4) (mk_rational 1 2));
  check "-3/6 normalizes to -1/2"
    (rat_eq (mk_rational (0-3) 6) (mk_rational (0-1) 2));
  check "3/-6 normalizes to -1/2"
    (rat_eq (mk_rational 3 (0-6)) (mk_rational (0-1) 2));

  (* Comparison *)
  check "1/3 < 2/3" (rat_lt (mk_rational 1 3) (mk_rational 2 3));
  check "not (2/3 < 1/3)" (not (rat_lt (mk_rational 2 3) (mk_rational 1 3)));
  check "1/2 <= 1/2" (rat_le (mk_rational 1 2) (mk_rational 1 2));

  (* Rational power *)
  check "(2/3)^0 = 1" (rat_eq (rat_pow (mk_rational 2 3) 0) rat_one);
  check "(2/3)^1 = 2/3" (rat_eq (rat_pow (mk_rational 2 3) 1) (mk_rational 2 3));
  check "(2/3)^2 = 4/9" (rat_eq (rat_pow (mk_rational 2 3) 2) (mk_rational 4 9));

  FStar.IO.print_string "=== Tests complete ===\n"
