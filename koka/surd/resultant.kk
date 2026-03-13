// Resultant computation and related operations for univariate polynomials.
//
// Subresultant PRS (polynomial remainder sequence) based resultant,
// Lagrange interpolation, and composed sum/product polynomials.
module surd/resultant

import std/core/undiv
import surd/rational
import surd/ring
import surd/poly

// Resultant of two univariate polynomials over a field, via Euclidean PRS.
pub fun poly-resultant(f : poly<k>, g : poly<k>, fld : field<k>) : exn k
  val r = fld.field-ring
  pretend-no-div{ resultant-rec(f, g, fld) }

fun resultant-rec(f : poly<k>, g : poly<k>, fld : field<k>) : <div,exn> k
  val r = fld.field-ring
  if f.degree < 0 || g.degree < 0 then r.ring-zero
  elif g.degree == 0 then
    match g.lead-coeff
      Just(c) -> pow-field(c, f.degree, fld)
      Nothing -> r.ring-zero
  else
    val (_, rem) = div-mod-poly(f, g, fld)
    val df = f.degree
    val dg = g.degree
    val lg = match g.lead-coeff
      Just(c) -> c
      Nothing -> r.ring-one
    val dr = rem.degree
    val sign = if is-odd(df * dg) then (r.ring-neg)(r.ring-one) else r.ring-one
    if dr < 0 then r.ring-zero
    else
      val rest = resultant-rec(g, rem, fld)
      val lgpower = pow-field(lg, df - dr, fld)
      (r.ring-mul)((r.ring-mul)(sign, lgpower), rest)

// Power of a field element (non-negative exponent).
fun pow-field(x : k, n : int, fld : field<k>) : exn k
  val r = fld.field-ring
  pretend-no-div{ pow-field-rec(x, n, r) }

fun pow-field-rec(x : k, n : int, r : ring<k>) : <div,exn> k
  if n <= 0 then r.ring-one
  elif n == 1 then x
  elif is-even(n) then
    val half = pow-field-rec(x, n / 2, r)
    (r.ring-mul)(half, half)
  else (r.ring-mul)(x, pow-field-rec(x, n - 1, r))

// Lagrange interpolation: given points (x_i, y_i), compute the unique
// polynomial of degree <= n passing through all points.
pub fun lagrange-interpolate(points : list<(k, k)>, fld : field<k>) : exn poly<k>
  val r = fld.field-ring
  points.foldl(zero-poly()) fn(acc, pt)
    val xi = pt.fst
    val yi = pt.snd
    val basis = lagrange-basis(xi, points.map(fst), fld)
    add-poly(acc, scale-poly(yi, basis, r), r)

fun lagrange-basis(xi : k, xs : list<k>, fld : field<k>) : exn poly<k>
  val r = fld.field-ring
  val others = xs.filter(fn(x) !(r.ring-eq)(x, xi))
  others.foldl(const-poly(r.ring-one, r)) fn(acc, xj)
    val denom = (r.ring-sub)(xi, xj)
    val inv-denom = (fld.field-inv)(denom)
    val lin = mk-poly([(r.ring-neg)(xj), r.ring-one], r) // x - xj
    mul-poly(acc, scale-poly(inv-denom, lin, r), r)

// Composed sum polynomial.
// If p has roots alpha_i and q has roots beta_j, the result has
// roots alpha_i + beta_j.
pub fun composed-sum(p : poly<rational>, q : poly<rational>) : exn poly<rational>
  val dp = p.degree
  val dq = q.degree
  val result-deg = dp * dq
  val points = list(0, result-deg).map(fn(i) from-int(i))
  val values = points.map fn(x)
    val q-shifted = substitute-linear(q, x, Rational(-1, 1))
    poly-resultant(p, q-shifted, rational-field)
  lagrange-interpolate(points.zip(values), rational-field)

// Substitute (a + b*y) for x in polynomial p.
fun substitute-linear(p : poly<rational>, a : rational, b : rational) : poly<rational>
  match p.coeffs
    [] -> zero-poly()
    cs ->
      val binomial = mk-poly([a, b], rational-ring) // a + b*y
      cs.foldr(zero-poly()) fn(c, acc)
        add-poly(const-poly(c, rational-ring), mul-poly(binomial, acc, rational-ring), rational-ring)

// Composed product polynomial.
// If p has roots alpha_i and q has roots beta_j, the result has
// roots alpha_i * beta_j.
pub fun composed-product(p : poly<rational>, q : poly<rational>) : exn poly<rational>
  val dp = p.degree
  val dq = q.degree
  val result-deg = dp * dq
  val points-raw = list(1, result-deg + 1).map(fn(i) from-int(i))
  val points = points-raw
  val values = points.map fn(x)
    val q-scaled = substitute-scale(q, x)
    poly-resultant(p, q-scaled, rational-field)
  lagrange-interpolate(points.zip(values), rational-field)

// Substitute x/c for x in polynomial p, then clear denominators.
fun substitute-scale(p : poly<rational>, c : rational) : poly<rational>
  match p.coeffs
    [] -> zero-poly()
    cs ->
      val n = cs.length - 1
      val new-cs = cs.map-indexed fn(i, coeff)
        coeff * rational/pow(c, n - i)
      mk-poly(new-cs.reverse.reverse, rational-ring)

// Negate the variable: p(-x).
pub fun negate-var(p : poly<rational>) : poly<rational>
  val cs = p.coeffs
  val new-cs = cs.map-indexed fn(i, c)
    if is-odd(i) then c.negate else c
  mk-poly(new-cs, rational-ring)

// Reciprocal polynomial: x^n * p(1/x).
pub fun reciprocal-poly(p : poly<rational>) : poly<rational>
  mk-poly(p.coeffs.reverse, rational-ring)
