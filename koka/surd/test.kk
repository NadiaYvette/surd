// Test suite for the surd Koka port.
//
// Tests rational arithmetic, polynomial operations (dict-passing and
// effect-based), evaluation (direct and effect-based), normalization,
// and the ring/field effect handlers.
module surd/test

import std/core/undiv
import std/num/float64
import surd/rational
import surd/rad-expr
import surd/ring
import surd/ring-effect
import surd/poly
import surd/normalize
import surd/eval
import surd/eval-effect
import surd/pretty
import surd/dag
import surd/positive
import surd/prime-factors
import surd/normal-form

// ---------------------------------------------------------------
// Test harness using local state via reference
// ---------------------------------------------------------------

pub effect test-state
  fun pass() : ()
  fun fail() : ()
  fun get-passes() : int
  fun get-fails() : int

fun check(name : string, b : bool) : <console,test-state> ()
  if b then
    pass()
    println("  PASS: " ++ name)
  else
    fail()
    println("  FAIL: " ++ name)

fun check-approx(name : string, got : float64, expected : float64, tol : float64) : <console,test-state> ()
  val diff = if got > expected then float64/(-)(got, expected) else float64/(-)(expected, got)
  check(name, diff < tol)

fun section(name : string) : console ()
  println("")
  println("--- " ++ name ++ " ---")

// ---------------------------------------------------------------
// Tests
// ---------------------------------------------------------------

pub fun main() : <console,div,exn> ()
  var passes := 0
  var fails := 0
  with handler
    fun pass() passes := passes + 1
    fun fail() fails := fails + 1
    fun get-passes() passes
    fun get-fails() fails
  println("=== Surd Koka Test Suite ===")

  // ---- Rational arithmetic ----
  section("Rational arithmetic")
  check("rat_zero", surd/rational/zero == Rational(0, 1))
  check("rat_one", surd/rational/one == Rational(1, 1))
  check("rat_add", rational/(+)(rational(1, 2), rational(1, 3)) == rational(5, 6))
  check("rat_sub", rational/(-)(rational(3, 4), rational(1, 4)) == rational(1, 2))
  check("rat_mul", rational/(*)(rational(2, 3), rational(3, 4)) == rational(1, 2))
  check("rat_div", rational/(/)(rational(2, 3), rational(4, 5)) == rational(5, 6))
  check("rat_inv", rational/inv(rational(2, 3)) == rational(3, 2))
  check("rat_neg", rational/negate(rational(3, 7)) == rational(-3, 7))
  check("rat_pow2", rational/pow(rational(2, 3), 2) == rational(4, 9))
  check("rat_pow0", rational/pow(rational(5, 7), 0) == surd/rational/one)
  check("rat_cmp_lt", rational/(<)(rational(1, 3), rational(1, 2)))
  check("rat_cmp_gt", rational/(>)(rational(3, 4), rational(2, 3)))
  check("rat_normalize", rational(6, 9) == rational(2, 3))
  check("rat_neg_denom", rational(1, -3) == rational(-1, 3))
  check("gcd_int", gcd-int(12, 8) == 4)
  check("gcd_int_zero", gcd-int(7, 0) == 7)

  // ---- Positive and prime factors ----
  section("Positive and prime factors")
  check("positive_ok", positive(5).is-just)
  check("positive_zero", positive(0).is-nothing)
  check("positive_neg", positive(-3).is-nothing)
  check("factorise_12", factorise(unsafe-positive(12)) == [(2, 2), (3, 1)])
  check("factorise_1", factorise(unsafe-positive(1)) == [])
  check("is_prime_7", is-prime(7))
  check("is_prime_9", !is-prime(9))

  // ---- Ring effect handlers ----
  section("Ring effect handlers")

  val ring-add-result = with-int-ring
    ring-add(3, 4)
  check("int_ring_add", ring-add-result == 7)

  val ring-mul-result = with-int-ring
    ring-mul(3, 4)
  check("int_ring_mul", ring-mul-result == 12)

  val ring-neg-result = with-int-ring
    ring-neg(5)
  check("int_ring_neg", ring-neg-result == -5)

  val rat-ring-add = with-rational-ring
    ring-add(rational(1, 2), rational(1, 3))
  check("rat_ring_add", rat-ring-add == rational(5, 6))

  val rat-ring-zero = with-rational-ring
    ring-zero()
  check("rat_ring_zero", rat-ring-zero == surd/rational/zero)

  val rat-ring-fromint = with-rational-ring
    ring-from-int(42)
  check("rat_ring_fromint", rat-ring-fromint == rational(42, 1))

  // ---- Field effect handlers ----
  section("Field effect handlers")

  val rat-field-div = with-rational-field
    field-div(rational(2, 3), rational(4, 5))
  check("rat_field_div", rat-field-div == rational(5, 6))

  val rat-field-inv = with-rational-field
    field-inv(rational(3, 7))
  check("rat_field_inv", rat-field-inv == rational(7, 3))

  // ---- RadExpr basics ----
  section("RadExpr basics")

  val two = int-expr(2)
  val three = int-expr(3)
  val s = RAdd(two, three)
  check("rad_node_count", pretend-no-div{ node-count(s) } == 3)
  check("rad_depth", pretend-no-div{ depth(s) } == 1)

  val sq2 = Root(2, int-expr(2))
  val sq3 = Root(2, int-expr(3))
  val expr1 = RAdd(sq2, sq3)
  check("rad_eq_self", pretend-no-div{ rad-eq(expr1, expr1, fn(a : rational, b : rational) rational/(==)(a, b)) })
  check("rad_neq", pretend-no-div{ !rad-eq(sq2, sq3, fn(a : rational, b : rational) rational/(==)(a, b)) })

  // ---- Eval (direct) ----
  section("Eval (direct)")

  val sqrt2-val = eval-double(sq2)
  check-approx("eval_sqrt2", sqrt2-val, 1.4142135623730951, 1.0e-10)

  val sum-val = eval-double(RAdd(int-expr(1), sq2))
  check-approx("eval_1_plus_sqrt2", sum-val, 2.4142135623730951, 1.0e-10)

  val inv-val = eval-double(Inv(int-expr(2)))
  check-approx("eval_inv_2", inv-val, 0.5, 1.0e-10)

  // Complex eval
  val neg1 = int-expr(-1)
  val sqrt_neg1 = Root(2, neg1)
  val zi = eval-complex(sqrt_neg1)
  check-approx("eval_complex_i_re", zi.re, 0.0, 1.0e-10)
  check-approx("eval_complex_i_im", zi.im, 1.0, 1.0e-10)

  // ---- Eval (effect-based) ----
  section("Eval (effect-based)")

  val e-sqrt2 = eval-double-e(sq2)
  check-approx("eval_e_sqrt2", e-sqrt2, 1.4142135623730951, 1.0e-10)

  val e-sum = eval-double-e(RAdd(int-expr(1), sq2))
  check-approx("eval_e_1_plus_sqrt2", e-sum, 2.4142135623730951, 1.0e-10)

  val e-inv = eval-double-e(Inv(int-expr(2)))
  check-approx("eval_e_inv_2", e-inv, 0.5, 1.0e-10)

  val e-cbrt = eval-double-e(Root(3, int-expr(27)))
  check-approx("eval_e_cbrt27", e-cbrt, 3.0, 1.0e-10)

  val e-pow = eval-double-e(Pow(int-expr(3), 4))
  check-approx("eval_e_pow3_4", e-pow, 81.0, 1.0e-10)

  // Complex eval via effect
  val ec-i = eval-complex-e(sqrt_neg1)
  check-approx("eval_e_complex_i_re", ec-i.re, 0.0, 1.0e-10)
  check-approx("eval_e_complex_i_im", ec-i.im, 1.0, 1.0e-10)

  // Verify effect-based and direct agree
  val test-expr = RMul(RAdd(sq2, int-expr(3)), Inv(sq3))
  val direct-val = eval-double(test-expr)
  val effect-val = eval-double-e(test-expr)
  check-approx("eval_effect_matches_direct", effect-val, direct-val, 1.0e-10)

  // ---- Polynomial (dict-passing) ----
  section("Polynomial (dict-passing)")

  val r = rational-ring
  val p1 = mk-poly([rational(1, 1), rational(2, 1), rational(1, 1)], r)  // 1 + 2x + x^2
  val p2 = mk-poly([rational(1, 1), rational(1, 1)], r)  // 1 + x
  check("poly_degree_p1", p1.degree == 2)
  check("poly_degree_p2", p2.degree == 1)
  check("poly_zero_degree", (zero-poly() : poly<rational>).degree == -1)

  val p-sum = add-poly(p1, p2, r)
  check("poly_add", poly/(==)(p-sum, mk-poly([rational(2, 1), rational(3, 1), rational(1, 1)], r), r))

  val p-prod = mul-poly(p2, p2, r)  // (1+x)^2 = 1 + 2x + x^2
  check("poly_mul", poly/(==)(p-prod, p1, r))

  val p-neg = neg-poly(p2, r)
  check("poly_neg", poly/(==)(p-neg, mk-poly([rational(-1, 1), rational(-1, 1)], r), r))

  val p-eval = eval-poly(p1, rational(2, 1), r)  // 1 + 2*2 + 4 = 9
  check("poly_eval", p-eval == rational(9, 1))

  // Division
  val f = rational-field
  val (q, rem) = div-mod-poly(p1, p2, f)
  check("poly_divmod_q", poly/(==)(q, p2, r))
  check("poly_divmod_r", rem.is-zero)

  // Derivative
  val p-diff = diff-poly(p1, r)  // d/dx(1 + 2x + x^2) = 2 + 2x
  check("poly_diff", poly/(==)(p-diff, mk-poly([rational(2, 1), rational(2, 1)], r), r))

  // ---- Polynomial (effect-based) ----
  section("Polynomial (effect-based)")

  val ep-sum = with-rational-ring
    val a = mk-poly-e([rational(1, 1), rational(2, 1)])
    val b = mk-poly-e([rational(3, 1), rational(4, 1)])
    add-poly-e(a, b)
  check("poly_e_add", poly/(==)(ep-sum, mk-poly([rational(4, 1), rational(6, 1)], r), r))

  val ep-mul = with-rational-ring
    val a = mk-poly-e([rational(1, 1), rational(1, 1)])
    val b = mk-poly-e([rational(1, 1), rational(1, 1)])
    mul-poly-e(a, b)
  check("poly_e_mul", poly/(==)(ep-mul, p1, r))

  val ep-neg = with-rational-ring
    neg-poly-e(mk-poly-e([rational(1, 1), rational(2, 1)]))
  check("poly_e_neg", poly/(==)(ep-neg, mk-poly([rational(-1, 1), rational(-2, 1)], r), r))

  val ep-eval = with-rational-ring
    eval-poly-e(mk-poly-e([rational(1, 1), rational(2, 1), rational(1, 1)]), rational(2, 1))
  check("poly_e_eval", ep-eval == rational(9, 1))

  val ep-diff = with-rational-ring
    diff-poly-e(mk-poly-e([rational(1, 1), rational(2, 1), rational(1, 1)]))
  check("poly_e_diff", poly/(==)(ep-diff, mk-poly([rational(2, 1), rational(2, 1)], r), r))

  val ep-show = with-rational-ring
    show-poly-e(mk-poly-e([rational(1, 1), rational(0, 1), rational(3, 1)]))
  check("poly_e_show", ep-show == "1 + 3*x^2")

  // Division via effect
  val ep-divmod = with-rational-ring
    with-rational-field
      div-mod-poly-e(
        mk-poly-e([rational(1, 1), rational(2, 1), rational(1, 1)]),
        mk-poly-e([rational(1, 1), rational(1, 1)])
      )
  check("poly_e_divmod_q", poly/(==)(ep-divmod.fst, p2, r))
  check("poly_e_divmod_r", ep-divmod.snd.is-zero)

  // GCD via effect
  val ep-gcd = with-rational-ring
    with-rational-field
      gcd-poly-e(
        mk-poly-e([rational(1, 1), rational(2, 1), rational(1, 1)]),
        mk-poly-e([rational(-1, 1), rational(0, 1), rational(1, 1)])
      )
  // GCD should be (1+x), monic
  check("poly_e_gcd", poly/(==)(ep-gcd, p2, r))

  // ---- Normalization ----
  section("Normalization")

  // fold-constants: Lit(2) + Lit(3) -> Lit(5)
  val fc = fold-constants(RAdd(int-expr(2), int-expr(3)))
  check("fold_constants_add", pretend-no-div{ rad-eq(fc, int-expr(5), fn(a : rational, b : rational) rational/(==)(a, b)) })

  // fold-constants: Lit(0) * x -> Lit(0)
  val fc2 = fold-constants(RMul(int-expr(0), sq2))
  check("fold_constants_zero_mul", pretend-no-div{ rad-eq(fc2, Lit(surd/rational/zero), fn(a : rational, b : rational) rational/(==)(a, b)) })

  // normalize idempotent: normalize(normalize(e)) == normalize(e)
  val test-n = RAdd(RMul(int-expr(2), sq2), RMul(int-expr(3), sq2))
  val n1 = normalize(test-n)
  val n2 = normalize(n1)
  check("normalize_idempotent", pretend-no-div{ rad-eq(n1, n2, fn(a : rational, b : rational) rational/(==)(a, b)) })

  // normalize collects like terms: 2*sqrt(2) + 3*sqrt(2) -> 5*sqrt(2)
  val expected-n = RMul(Lit(from-int(5)), sq2)
  check("normalize_collect_terms", pretend-no-div{ rad-eq(n1, expected-n, fn(a : rational, b : rational) rational/(==)(a, b)) })

  // simplify powers: sqrt(2)^2 -> 2
  val sp = simplify-powers(Pow(sq2, 2))
  check("simplify_powers_sqrt_sq", pretend-no-div{ rad-eq(sp, int-expr(2), fn(a : rational, b : rational) rational/(==)(a, b)) })

  // extract perfect powers: sqrt(4) -> 2
  val epp = normalize(Root(2, int-expr(4)))
  check("extract_perfect_sqrt4", pretend-no-div{ rad-eq(epp, int-expr(2), fn(a : rational, b : rational) rational/(==)(a, b)) })

  // extract perfect powers: sqrt(8) -> 2*sqrt(2)
  val epp2 = normalize(Root(2, int-expr(8)))
  val expect-epp2 = RMul(Lit(from-int(2)), sq2)
  check("extract_perfect_sqrt8", pretend-no-div{ rad-eq(epp2, expect-epp2, fn(a : rational, b : rational) rational/(==)(a, b)) })

  // ---- Pretty printing ----
  section("Pretty printing")

  val pp-lit = pretty(int-expr(42))
  check("pretty_lit", pp-lit == "42")

  val pp-neg = pretty(Neg(int-expr(3)))
  check("pretty_neg", pp-neg == "-3")

  val pp-sum = pretty(RAdd(int-expr(1), sq2))
  check("pretty_sum", pp-sum == "1 + \x221A2")

  val pp-inv = pretty(Inv(sq2))
  check("pretty_inv", pp-inv == "1/\x221A2")

  // ---- DAG ----
  section("DAG")

  val dag-expr = RAdd(sq2, RMul(sq2, sq3))
  val dag = to-dag(dag-expr)
  check("dag_size", dag-size(dag) >= 5)

  val rt-expr = from-dag(dag)
  val dag-val = eval-double(rt-expr)
  val orig-val = eval-double(dag-expr)
  check-approx("dag_roundtrip_eval", dag-val, orig-val, 1.0e-10)

  // DAG eval complex
  val dag-cval = dag-eval-complex(dag)
  check-approx("dag_eval_complex_re", dag-cval.re, orig-val, 1.0e-10)

  // ---- Normal form ----
  section("Normal form")

  val nf1 = to-norm-expr(sq2)
  val back1 = from-norm-expr(nf1)
  val nf-eval = eval-double(back1)
  check-approx("nf_sqrt2_roundtrip", nf-eval, 1.4142135623730951, 1.0e-10)

  // NF of sqrt(8) should simplify to 2*sqrt(2)
  val nf8 = to-norm-expr(Root(2, int-expr(8)))
  val back8 = from-norm-expr(nf8)
  val nf8-eval = eval-double(back8)
  check-approx("nf_sqrt8", nf8-eval, 2.8284271247461903, 1.0e-10)

  // NF round-trip preserves value
  val complex-expr = RAdd(RMul(Lit(rational(1, 2)), sq2), RMul(Lit(rational(1, 3)), sq3))
  val nf-complex = to-norm-expr(complex-expr)
  val back-complex = from-norm-expr(nf-complex)
  val val-orig = eval-double(complex-expr)
  val val-back = eval-double(back-complex)
  check-approx("nf_roundtrip_value", val-back, val-orig, 1.0e-10)

  // ---- Summary ----
  println("")
  val total-passes = get-passes()
  val total-fails = get-fails()
  println("=== Results: " ++ total-passes.show ++ " passed, " ++ total-fails.show ++ " failed ===")
  if total-fails > 0 then
    println("SOME TESTS FAILED")
  else
    println("ALL TESTS PASSED")
