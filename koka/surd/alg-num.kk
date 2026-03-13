// Algebraic number type: a real algebraic number represented as
// (minimal polynomial, isolating interval).
//
// Arithmetic is done via resultant-based composed polynomials.
module surd/alg-num

import std/core/undiv
import std/num/float64
import surd/rational
import surd/ring
import surd/poly
import surd/interval
import surd/root-bound
import surd/root-isolation
import surd/resultant
import surd/factoring

// A real algebraic number: its monic irreducible minimal polynomial
// over Q and an isolating interval pinpointing which root it is.
pub struct alg-num
  an-min-poly : poly<rational>
  an-interval : interval

// Embed a rational number as an algebraic number.
pub fun alg-from-rational(r : rational) : alg-num
  Alg-num(
    an-min-poly = mk-poly([r.negate, surd/rational/one], rational-ring), // x - r
    an-interval = Interval(r, r)
  )

// Construct an algebraic number from a polynomial and an approximate
// real value (used to pick the right root).
pub fun alg-from-poly(p : poly<rational>, approx : float64) : exn maybe<alg-num>
  val mp = monic-poly(p, rational-field)
  val roots = isolate-real-roots(mp)
  match pick-closest-root(roots, approx)
    Just(ii) -> Just(Alg-num(ii.ii-poly, ii.ii-interval))
    Nothing -> Nothing

fun pick-closest-root(roots : list<isolating-interval>, target : float64) : maybe<isolating-interval>
  match roots
    [] -> Nothing
    [r] -> Just(r)
    Cons(r, rest) ->
      val d = interval-dist(r.ii-interval, target)
      pick-loop(rest, r, d, target)

fun pick-loop(rs : list<isolating-interval>, best : isolating-interval,
              best-dist : float64, target : float64) : maybe<isolating-interval>
  match rs
    [] -> Just(best)
    Cons(r, rest) ->
      val d = interval-dist(r.ii-interval, target)
      if d < best-dist then pick-loop(rest, r, d, target)
      else pick-loop(rest, best, best-dist, target)

fun interval-dist(iv : interval, target : float64) : float64
  val mid = midpoint(iv)
  abs(mid.num.float64 / mid.den.float64 - target)

// Get the minimal polynomial.
pub fun alg-min-poly(a : alg-num) : poly<rational>
  a.an-min-poly

// Get an approximate rational value.
pub fun alg-approx(a : alg-num) : rational
  midpoint(a.an-interval)

// Negation: negate(alpha) has minpoly p(-x).
pub fun alg-neg(a : alg-num) : exn alg-num
  val neg-p = negate-var(a.an-min-poly)
  val neg-iv = Interval(a.an-interval.hi.negate, a.an-interval.lo.negate)
  val mp = monic-poly(neg-p, rational-field)
  // Find the right factor
  val factors = factor-square-free(mp)
  val approx = midpoint(neg-iv).num.float64 / midpoint(neg-iv).den.float64
  val best = pick-closest(approx, factors)
  Alg-num(best, neg-iv)

// Addition via composed sum.
pub fun alg-add(a : alg-num, b : alg-num) : exn alg-num
  val cs = composed-sum(a.an-min-poly, b.an-min-poly)
  val iv = iadd(a.an-interval, b.an-interval)
  val approx = midpoint(iv).num.float64 / midpoint(iv).den.float64
  val factors = factor-square-free(monic-poly(cs, rational-field))
  val best = pick-closest(approx, factors)
  Alg-num(best, iv)

// Multiplication via composed product.
pub fun alg-mul(a : alg-num, b : alg-num) : exn alg-num
  val cp = composed-product(a.an-min-poly, b.an-min-poly)
  val iv = imul(a.an-interval, b.an-interval)
  val approx = midpoint(iv).num.float64 / midpoint(iv).den.float64
  val factors = factor-square-free(monic-poly(cp, rational-field))
  val best = pick-closest(approx, factors)
  Alg-num(best, iv)

// Equality: same minimal polynomial and overlapping intervals (after refinement).
pub fun alg-eq(a : alg-num, b : alg-num) : exn bool
  if !(poly/(==)(a.an-min-poly, b.an-min-poly, rational-ring)) then False
  else
    // Refine both intervals and check overlap
    val a' = refine-root(Rational(1, 1000000), Isolating-interval(a.an-min-poly, a.an-interval))
    val b' = refine-root(Rational(1, 1000000), Isolating-interval(b.an-min-poly, b.an-interval))
    overlaps(a'.ii-interval, b'.ii-interval)

// Comparison.
pub fun alg-compare(a : alg-num, b : alg-num) : exn order
  if alg-eq(a, b) then Eq
  else
    val ma = midpoint(a.an-interval)
    val mb = midpoint(b.an-interval)
    rational/cmp(ma, mb)

// Show an algebraic number.
pub fun alg-show(a : alg-num) : string
  "AlgNum(" ++ poly/show(a.an-min-poly, rational-ring) ++ ", " ++ a.an-interval.show ++ ")"
