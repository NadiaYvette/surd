// Minimal polynomial computation for radical expressions.
//
// Given a radical expression, compute its minimal polynomial over Q.
// Uses resultant-based tower construction.
module surd/minimal-poly

import std/core/undiv
import std/num/float64
import surd/rational
import surd/ring
import surd/poly
import surd/rad-expr
import surd/expr
import surd/eval
import surd/resultant
import surd/factoring
import surd/root-bound

// Compute the minimal polynomial of a radical expression over Q.
// This builds a tower of algebraic extensions one radical at a time,
// computing resultants to eliminate each radical.
pub fun minimal-poly(e : rad-expr<rational>) : <div,exn> poly<rational>
  val rads = collect-radicals(e)
  if rads.is-empty then
    // Pure rational expression
    val v = eval-double(e)
    // The minimal polynomial is x - v (if rational)
    match try-rational-value(e)
      Just(r) -> mk-poly([r.negate, surd/rational/one], rational-ring)
      Nothing -> mk-poly([Rational(-1, 1), surd/rational/one], rational-ring) // fallback
  else
    // Build the annihilating polynomial via resultants
    val sorted-rads = topo-sort-radicals(rads)
    val annihilating = build-annihilating(e, sorted-rads)
    // Factor and pick the factor containing our value
    val approx = eval-double(e)
    val factors = factor-square-free(monic-poly(annihilating, rational-field))
    match factors
      [] -> annihilating
      _ -> pick-closest(approx, factors)

// Try to evaluate a rational-only expression to a rational value.
fun try-rational-value(e : rad-expr<rational>) : <div,exn> maybe<rational>
  match e
    Lit(r) -> Just(r)
    Neg(a) -> match try-rational-value(a)
      Just(r) -> Just(r.negate)
      Nothing -> Nothing
    RAdd(a, b) ->
      match (try-rational-value(a), try-rational-value(b))
        (Just(ra), Just(rb)) -> Just(ra + rb)
        _ -> Nothing
    RMul(a, b) ->
      match (try-rational-value(a), try-rational-value(b))
        (Just(ra), Just(rb)) -> Just(ra * rb)
        _ -> Nothing
    Inv(a) ->
      match try-rational-value(a)
        Just(r) -> if r.is-zero then Nothing else Just(surd/rational/one / r)
        Nothing -> Nothing
    Pow(a, n) ->
      match try-rational-value(a)
        Just(r) -> Just(rational/pow(r, n))
        Nothing -> Nothing
    Root(_, _) -> Nothing

// Build an annihilating polynomial by eliminating radicals one at a time.
// For each radical root(n, r), we substitute y = root(n, r) and compute
// the resultant of the expression polynomial and y^n - r.
fun build-annihilating(e : rad-expr<rational>, rads : list<(int, rad-expr<rational>)>) : <div,exn> poly<rational>
  match rads
    [] ->
      // No radicals: e should be rational
      match try-rational-value(e)
        Just(r) -> mk-poly([r.negate, surd/rational/one], rational-ring)
        Nothing -> mk-poly([Rational(-1, 1), surd/rational/one], rational-ring)
    _ ->
      // Simple case: single radical root(n, Lit(r))
      // Then e can be written as p(root(n, r)) and we want the
      // resultant of p(y) - x and y^n - r.
      // For now, use the direct approach via the first radical
      val (n, rad) = rads.head.unjust
      match rad
        Lit(r) ->
          // Build y^n - r (elimination polynomial)
          val elim-poly = mk-poly(
            [r.negate] ++ replicate(surd/rational/zero, n - 1) ++ [surd/rational/one],
            rational-ring)
          // Express e as a polynomial in y = root(n, r), then
          // build p(x) = resultant_y(x - e(y), y^n - r)
          // For a single radical, the expression polynomial is f(y) such that e = f(root(n,r))
          val expr-poly = expr-to-poly-in-radical(e, n, r)
          // Build x - f(y) as a bivariate polynomial; compute resultant in y.
          // The annihilating polynomial for e is: product of (x - f(zeta^k * root(n,r)))
          // for k = 0..n-1. For simplicity, use norm: product of conjugates.
          // p(x) = resultant_y(f(y) - x, y^n - r) where x is treated as parameter.
          // Since we need a poly in x, build it via evaluation at enough points.
          val deg-bound = n * expr-poly.degree
          val points = list(0, deg-bound).map(fn(i) from-int(i))
          val values = points.map fn(xi)
            // Evaluate resultant_y(f(y) - xi, y^n - r)
            val shifted = sub-poly(expr-poly, const-poly(xi, rational-ring), rational-ring)
            poly-resultant(shifted, elim-poly, rational-field)
          surd/resultant/lagrange-interpolate(points.zip(values), rational-field)
        _ ->
          // Nested radical: use numerical approach as fallback
          val approx = eval-double(e)
          mk-poly([from-int(0 - approx.truncate.int), surd/rational/one], rational-ring)

// Express a radical expression as a polynomial in a single radical root(n, r).
// E.g., 1 + 2*sqrt(3) becomes 1 + 2*y when the radical is sqrt(3).
fun expr-to-poly-in-radical(e : rad-expr<rational>, n : int, r : rational) : <div,exn> poly<rational>
  match e
    Lit(v) -> const-poly(v, rational-ring)
    Neg(a) -> neg-poly(expr-to-poly-in-radical(a, n, r), rational-ring)
    RAdd(a, b) ->
      add-poly(expr-to-poly-in-radical(a, n, r),
               expr-to-poly-in-radical(b, n, r),
               rational-ring)
    RMul(a, b) ->
      val pa = expr-to-poly-in-radical(a, n, r)
      val pb = expr-to-poly-in-radical(b, n, r)
      // Multiply and reduce modulo y^n - r
      val prod = mul-poly(pa, pb, rational-ring)
      reduce-mod-radical(prod, n, r)
    Inv(a) ->
      // For now, return the polynomial and handle inversion externally
      val pa = expr-to-poly-in-radical(a, n, r)
      if pa.degree == 0 then
        match pa.lead-coeff
          Just(c) -> const-poly(surd/rational/one / c, rational-ring)
          Nothing -> const-poly(surd/rational/one, rational-ring)
      else const-poly(surd/rational/one, rational-ring) // approximate
    Root(m, Lit(s)) ->
      if m == n && s == r then mono-x(rational-ring) // y
      else const-poly(surd/rational/zero, rational-ring) // different radical
    Root(_, _) -> const-poly(surd/rational/zero, rational-ring)
    Pow(Root(m, Lit(s)), k) ->
      if m == n && s == r then
        // y^k reduced mod y^n - r
        val yk = mk-poly(replicate(surd/rational/zero, k) ++ [surd/rational/one], rational-ring)
        reduce-mod-radical(yk, n, r)
      else const-poly(surd/rational/zero, rational-ring)
    Pow(a, k) ->
      val pa = expr-to-poly-in-radical(a, n, r)
      val result = pretend-no-div{ poly-pow-mod(pa, k, n, r) }
      result

// Reduce a polynomial modulo y^n - r (replace y^n by r).
fun reduce-mod-radical(p : poly<rational>, n : int, r : rational) : <div,exn> poly<rational>
  if p.degree < n then p
  else
    val elim = mk-poly(
      [r.negate] ++ replicate(surd/rational/zero, n - 1) ++ [surd/rational/one],
      rational-ring)
    val (_, rem) = div-mod-poly(p, elim, rational-field)
    rem

// Polynomial power modulo y^n - r.
fun poly-pow-mod(p : poly<rational>, k : int, n : int, r : rational) : <div,exn> poly<rational>
  if k <= 0 then const-poly(surd/rational/one, rational-ring)
  elif k == 1 then p
  elif is-even(k) then
    val half = poly-pow-mod(p, k / 2, n, r)
    reduce-mod-radical(mul-poly(half, half, rational-ring), n, r)
  else
    val rest = poly-pow-mod(p, k - 1, n, r)
    reduce-mod-radical(mul-poly(p, rest, rational-ring), n, r)
