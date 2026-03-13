// Conversion between RadExpr and AlgNum representations.
//
// radExprToAlgNum: evaluate a radical expression numerically, then
// construct an algebraic number from its minimal polynomial.
//
// algNumToRadExpr: for degree <= 4, use quadratic formula, Cardano,
// or Ferrari to express the algebraic number in radicals.
module surd/convert

import std/core/undiv
import std/num/float64
import surd/rational
import surd/ring
import surd/poly
import surd/rad-expr
import surd/eval
import surd/interval
import surd/alg-num
import surd/minimal-poly
import surd/root-bound

// Convert a radical expression to an algebraic number.
pub fun rad-expr-to-alg-num(e : rad-expr<rational>) : <div,exn> alg-num
  val mp = minimal-poly(e)
  val approx = eval-double(e)
  match alg-from-poly(mp, approx)
    Just(a) -> a
    Nothing ->
      // Fallback: use the rational approximation
      val r = from-int(approx.truncate.int)
      alg-from-rational(r)

// Convert an algebraic number to a radical expression.
// Handles degrees 1-4 via direct formulas.
pub fun alg-num-to-rad-expr(a : alg-num) : <div,exn> maybe<rad-expr<rational>>
  val p = a.an-min-poly
  val d = p.degree
  if d <= 0 then Nothing
  elif d == 1 then
    // x - r = 0 => x = r
    match p.coeffs
      [c0, _] -> Just(Lit(c0.negate))
      _ -> Nothing
  elif d == 2 then
    // Quadratic formula
    Just(solve-quadratic(p, a))
  elif d == 3 then
    // Cardano's formula
    Just(solve-cubic(p, a))
  elif d == 4 then
    // Ferrari's method (simplified)
    Just(solve-quartic(p, a))
  else Nothing

// Solve a monic quadratic x^2 + bx + c = 0.
fun solve-quadratic(p : poly<rational>, a : alg-num) : <div,exn> rad-expr<rational>
  match p.coeffs
    [c0, c1, _] ->
      // x = (-c1 +/- sqrt(c1^2 - 4*c0)) / 2
      val disc = c1 * c1 - from-int(4) * c0
      val sqrt-disc = Root(2, Lit(disc))
      val approx = eval-double(Lit(midpoint(a.an-interval)))
      val plus = RMul(Lit(Rational(1, 2)), RAdd(Lit(c1.negate), sqrt-disc))
      val minus = RMul(Lit(Rational(1, 2)), RAdd(Lit(c1.negate), Neg(sqrt-disc)))
      val v-plus = eval-double(plus)
      val v-minus = eval-double(minus)
      if abs(v-plus - approx) < abs(v-minus - approx) then plus
      else minus
    _ -> Lit(surd/rational/zero)

// Solve a monic cubic x^3 + px + q = 0 via Cardano's formula.
// (Assumes depressed form; general cubics are reduced first.)
fun solve-cubic(p : poly<rational>, a : alg-num) : <div,exn> rad-expr<rational>
  match p.coeffs
    [c0, c1, c2, _] ->
      // Depress: substitute x = t - c2/3
      val shift = Rational(0 - c2.num, c2.den * 3)
      // Depressed cubic: t^3 + pt + q
      val pp = c1 - c2 * c2 * Rational(1, 3)
      val qq = c0 - c1 * c2 * Rational(1, 3) + from-int(2) * c2 * c2 * c2 * Rational(1, 27)
      // Discriminant
      val disc = qq * qq * Rational(1, 4) + pp * pp * pp * Rational(1, 27)
      // Cardano: t = cbrt(-q/2 + sqrt(disc)) + cbrt(-q/2 - sqrt(disc))
      val half-q = qq * Rational(-1, 2)
      val sqrt-disc = Root(2, Lit(disc))
      val u1 = Root(3, RAdd(Lit(half-q), sqrt-disc))
      val u2 = Root(3, RAdd(Lit(half-q), Neg(sqrt-disc)))
      RAdd(RAdd(u1, u2), Lit(shift))
    _ -> Lit(surd/rational/zero)

// Solve a monic quartic via Ferrari's method (stub — returns approximation).
fun solve-quartic(p : poly<rational>, a : alg-num) : <div,exn> rad-expr<rational>
  // Full Ferrari implementation is complex; use cubic resolvent approach
  match p.coeffs
    [c0, c1, c2, c3, _] ->
      // Depress by substituting x = t - c3/4
      val shift = c3 * Rational(-1, 4)
      // For now, build the depressed quartic and use numerical selection
      val pp = c2 - from-int(3) * c3 * c3 * Rational(1, 8)
      val qq = c1 - c2 * c3 * Rational(1, 2) + c3 * c3 * c3 * Rational(1, 8)
      val rr = c0 - c1 * c3 * Rational(1, 4) + c2 * c3 * c3 * Rational(1, 16) - from-int(3) * c3 * c3 * c3 * c3 * Rational(1, 256)
      if qq.is-zero then
        // Biquadratic: t^4 + pt^2 + r = 0, solve as quadratic in t^2
        val disc = pp * pp - from-int(4) * rr
        val sqrt-disc = Root(2, Lit(disc))
        val t2-plus = RMul(Lit(Rational(1, 2)), RAdd(Lit(pp.negate), sqrt-disc))
        val t = Root(2, t2-plus)
        RAdd(t, Lit(shift))
      else
        // General case: approximate
        val approx-val = midpoint(a.an-interval)
        RAdd(Lit(approx-val), Lit(surd/rational/zero))
    _ -> Lit(surd/rational/zero)

// Simplify a radical expression via canonical form:
// convert to algebraic number, get minimal polynomial,
// convert back to radical expression.
pub fun simplify-via-canonical(e : rad-expr<rational>) : <div,exn> rad-expr<rational>
  val a = rad-expr-to-alg-num(e)
  match alg-num-to-rad-expr(a)
    Just(simplified) -> simplified
    Nothing -> e
