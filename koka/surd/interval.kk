// Interval arithmetic with rational endpoints for root isolation
// and numerical evaluation.
module surd/interval

import std/core/undiv
import surd/rational

// A closed interval [lo, hi] with rational endpoints.
pub value struct interval
  lo : rational
  hi : rational

// Midpoint of the interval.
pub fun midpoint(iv : interval) : rational
  val two = from-int(2)
  (iv.lo + iv.hi) * Rational(1, 2)

// Width of the interval.
pub fun width(iv : interval) : rational
  iv.hi - iv.lo

// Does the interval contain the given value?
pub fun contains(iv : interval, x : rational) : bool
  iv.lo <= x && x <= iv.hi

// Do two intervals overlap?
pub fun overlaps(iv1 : interval, iv2 : interval) : bool
  iv1.lo <= iv2.hi && iv2.lo <= iv1.hi

// Bisect an interval into two halves.
pub fun bisect(iv : interval) : (interval, interval)
  val m = midpoint(iv)
  (Interval(iv.lo, m), Interval(m, iv.hi))

// Narrow an interval by bisection, keeping the half where
// the midpoint satisfies the predicate.
pub fun refine(iv : interval, p : (rational) -> bool) : interval
  val (left, right) = bisect(iv)
  val m = midpoint(iv)
  if p(m) then left else right

// Point interval from a rational.
pub fun from-rational(x : rational) : interval
  Interval(x, x)

// ---------------------------------------------------------------
// Interval arithmetic operations
// ---------------------------------------------------------------

// Addition.
pub fun iadd(a : interval, b : interval) : interval
  Interval(a.lo + b.lo, a.hi + b.hi)

// Subtraction.
pub fun isub(a : interval, b : interval) : interval
  Interval(a.lo - b.hi, a.hi - b.lo)

// Negation.
pub fun ineg(iv : interval) : interval
  Interval(iv.hi.negate, iv.lo.negate)

// Multiplication.
pub fun imul(a : interval, b : interval) : interval
  val p1 = a.lo * b.lo
  val p2 = a.lo * b.hi
  val p3 = a.hi * b.lo
  val p4 = a.hi * b.hi
  val lo = min(min(p1, p2), min(p3, p4))
  val hi = max(max(p1, p2), max(p3, p4))
  Interval(lo, hi)

// Inverse of an interval that does not contain zero.
pub fun iinv(iv : interval) : exn interval
  if iv.lo > surd/rational/zero || iv.hi < surd/rational/zero then
    Interval(surd/rational/one / iv.hi, surd/rational/one / iv.lo)
  else throw("iinv: interval contains zero")

// Division.
pub fun idiv(a : interval, b : interval) : exn interval
  imul(a, iinv(b))

// Integer power.
pub fun ipow(iv : interval, n : int) : exn interval
  pretend-no-div{ ipow-rec(iv, n) }

fun ipow-rec(iv : interval, n : int) : <div,exn> interval
  if n == 0 then Interval(surd/rational/one, surd/rational/one)
  elif n < 0 then ipow-rec(iinv(iv), 0 - n)
  elif n.is-odd then
    // Odd power preserves monotonicity
    pretend-no-div{ fold-imul(iv, n) }
  else
    // Even power: result is non-negative
    val plo = rational/pow(iv.lo, n)
    val phi = rational/pow(iv.hi, n)
    if iv.lo <= surd/rational/zero && surd/rational/zero <= iv.hi then
      Interval(surd/rational/zero, max(plo, phi))
    else Interval(min(plo, phi), max(plo, phi))

fun fold-imul(iv : interval, n : int) : div interval
  if n <= 1 then iv
  else imul(iv, fold-imul(iv, n - 1))

// Absolute value of an interval.
pub fun iabs(iv : interval) : interval
  if iv.lo >= surd/rational/zero then iv
  elif iv.hi <= surd/rational/zero then Interval(iv.hi.negate, iv.lo.negate)
  else Interval(surd/rational/zero, max(iv.lo.negate, iv.hi))

// Strictly positive?
pub fun strictly-positive(iv : interval) : bool
  iv.lo > surd/rational/zero

// Strictly negative?
pub fun strictly-negative(iv : interval) : bool
  iv.hi < surd/rational/zero

// Contains zero?
pub fun contains-zero(iv : interval) : bool
  iv.lo <= surd/rational/zero && iv.hi >= surd/rational/zero

// ---------------------------------------------------------------
// Nth root via bisection on rational intervals
// ---------------------------------------------------------------

// Find a rational lower bound r such that r^n <= a, via bisection.
fun nth-root-lower(n : int, a : rational) : rational
  if a == surd/rational/zero then surd/rational/zero
  else
    val upper = max(a, surd/rational/one)
    pretend-no-div{ bisect-down(n, a, surd/rational/zero, upper, 60) }

// Find a rational upper bound r such that r^n >= a, via bisection.
fun nth-root-upper(n : int, a : rational) : rational
  if a == surd/rational/zero then surd/rational/zero
  else
    val upper = max(a, surd/rational/one) + surd/rational/one
    pretend-no-div{ bisect-up(n, a, surd/rational/zero, upper, 60) }

fun bisect-down(n : int, a : rational, lo : rational, hi : rational, iters : int) : div rational
  if iters == 0 then lo
  else
    val mid = (lo + hi) * Rational(1, 2)
    if rational/pow(mid, n) <= a then bisect-down(n, a, mid, hi, iters - 1)
    else bisect-down(n, a, lo, mid, iters - 1)

fun bisect-up(n : int, a : rational, lo : rational, hi : rational, iters : int) : div rational
  if iters == 0 then hi
  else
    val mid = (lo + hi) * Rational(1, 2)
    if rational/pow(mid, n) >= a then bisect-up(n, a, lo, mid, iters - 1)
    else bisect-up(n, a, mid, hi, iters - 1)

// Interval enclosure of the square root of a non-negative interval.
pub fun isqrt(iv : interval) : exn interval
  if iv.lo < surd/rational/zero then throw("isqrt: negative interval")
  elif iv.hi == surd/rational/zero then Interval(surd/rational/zero, surd/rational/zero)
  else Interval(nth-root-lower(2, iv.lo), nth-root-upper(2, iv.hi))

// Interval enclosure of the nth root of an interval.
pub fun inth(n : int, iv : interval) : exn interval
  if n <= 0 then throw("inth: non-positive root index")
  elif n == 1 then iv
  elif n == 2 then isqrt(iv)
  elif n.is-even && iv.lo < surd/rational/zero then throw("inth: even root of negative interval")
  elif iv.hi == surd/rational/zero && iv.lo == surd/rational/zero then
    Interval(surd/rational/zero, surd/rational/zero)
  elif n.is-odd && iv.hi < surd/rational/zero then
    // Odd root of entirely negative interval: negate, root, negate
    val neg-iv = Interval(iv.hi.negate, iv.lo.negate)
    val root-neg = Interval(nth-root-lower(n, neg-iv.lo), nth-root-upper(n, neg-iv.hi))
    Interval(root-neg.hi.negate, root-neg.lo.negate)
  elif n.is-odd && iv.lo < surd/rational/zero then
    // Odd root spanning zero
    val neg-part = nth-root-upper(n, iv.lo.negate)
    val pos-part = nth-root-upper(n, iv.hi)
    Interval(neg-part.negate, pos-part)
  else Interval(nth-root-lower(n, iv.lo), nth-root-upper(n, iv.hi))

// ---------------------------------------------------------------
// Complex intervals (rectangular: real part x imaginary part)
// ---------------------------------------------------------------

pub struct complex-interval
  ci-real : interval
  ci-imag : interval

pub fun ci-from-rational(r : rational) : complex-interval
  Complex-interval(from-rational(r), from-rational(surd/rational/zero))

pub fun ci-from-real(r : interval) : complex-interval
  Complex-interval(r, from-rational(surd/rational/zero))

pub fun ci-add(a : complex-interval, b : complex-interval) : complex-interval
  Complex-interval(iadd(a.ci-real, b.ci-real), iadd(a.ci-imag, b.ci-imag))

pub fun ci-sub(a : complex-interval, b : complex-interval) : complex-interval
  Complex-interval(isub(a.ci-real, b.ci-real), isub(a.ci-imag, b.ci-imag))

pub fun ci-neg(z : complex-interval) : complex-interval
  Complex-interval(ineg(z.ci-real), ineg(z.ci-imag))

// (a + bi)(c + di) = (ac - bd) + (ad + bc)i
pub fun ci-mul(a : complex-interval, b : complex-interval) : complex-interval
  Complex-interval(
    isub(imul(a.ci-real, b.ci-real), imul(a.ci-imag, b.ci-imag)),
    iadd(imul(a.ci-real, b.ci-imag), imul(a.ci-imag, b.ci-real))
  )

// 1/(a + bi) = (a - bi)/(a^2 + b^2)
pub fun ci-inv(z : complex-interval) : exn complex-interval
  val mag-sq = iadd(imul(z.ci-real, z.ci-real), imul(z.ci-imag, z.ci-imag))
  Complex-interval(
    idiv(z.ci-real, mag-sq),
    idiv(ineg(z.ci-imag), mag-sq)
  )

pub fun ci-pow(z : complex-interval, n : int) : exn complex-interval
  pretend-no-div{ ci-pow-rec(z, n) }

fun ci-pow-rec(z : complex-interval, n : int) : <div,exn> complex-interval
  if n == 0 then ci-from-rational(surd/rational/one)
  elif n == 1 then z
  elif n < 0 then ci-pow-rec(ci-inv(z), 0 - n)
  elif n.is-even then
    val half = ci-pow-rec(z, n / 2)
    ci-mul(half, half)
  else ci-mul(z, ci-pow-rec(z, n - 1))

// |z|^2 = re^2 + im^2 as an interval.
pub fun ci-magnitude-sq(z : complex-interval) : interval
  iadd(imul(z.ci-real, z.ci-real), imul(z.ci-imag, z.ci-imag))

pub fun ci-real-part(z : complex-interval) : interval
  z.ci-real

pub fun ci-imag-part(z : complex-interval) : interval
  z.ci-imag

// Show an interval.
pub fun interval/show(iv : interval) : string
  "[" ++ iv.lo.show ++ ", " ++ iv.hi.show ++ "]"

// Show a complex interval.
pub fun complex-interval/show(z : complex-interval) : string
  z.ci-real.show ++ " + " ++ z.ci-imag.show ++ "i"
