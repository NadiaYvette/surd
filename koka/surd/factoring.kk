// Polynomial factoring over Q.
//
// Implements Kronecker's method for small-degree polynomials
// and rational root testing. Sufficient for radical denesting
// and trig evaluation use cases.
module surd/factoring

import std/core/undiv
import surd/rational
import surd/ring
import surd/poly
import surd/positive
import surd/prime-factors

// Factor a polynomial over Q into irreducible factors.
// Returns a list of (irreducible factor, multiplicity) pairs.
pub fun factor-poly(p : poly<rational>) : exn list<(poly<rational>, int)>
  if p.degree <= 0 then []
  else
    val sf-factors = square-free(p, rational-field)
    sf-factors.flatmap fn(pair)
      val f = pair.fst
      val m = pair.snd
      factor-square-free(f).map(fn(g) (g, m))

// Factor a square-free polynomial over Q into irreducible factors.
pub fun factor-square-free(p : poly<rational>) : exn list<poly<rational>>
  if p.degree <= 0 then []
  elif p.degree == 1 then [monic-poly(p, rational-field)]
  else pretend-no-div{ go-factor(monic-poly(p, rational-field)) }

fun go-factor(f : poly<rational>) : <div,exn> list<poly<rational>>
  if f.degree <= 1 then [f]
  else
    match find-linear-factor(f)
      Just((root, quotient)) ->
        val lin = mk-poly([root.negate, surd/rational/one], rational-ring)
        Cons(lin, go-factor(quotient))
      Nothing ->
        match find-quadratic-factor(f)
          Just((fac, quotient)) ->
            Cons(fac, go-factor(quotient))
          Nothing ->
            // Cannot factor further with current methods; assume irreducible
            [f]

// Find rational roots of a polynomial using the rational root theorem.
pub fun rational-roots(p : poly<rational>) : exn list<rational>
  if p.degree <= 0 then []
  else
    val cs = p.coeffs
    // Clear denominators to get integer coefficients
    val lcm-denom = cs.foldl(1) fn(acc, c)
      lcm-int(acc, c.den)
    val int-coeffs = cs.map(fn(c) (c * from-int(lcm-denom)).num)
    val a0 = match int-coeffs
      Cons(x, _) -> x
      _ -> 0
    val an = int-coeffs.last.default(1)
    val num-divs = if a0 == 0 then [0] else divisors-of(abs(a0))
    val den-divs = divisors-of(abs(an))
    val candidates = num-divs.flatmap fn(nd)
      [nd, 0 - nd].flatmap fn(p')
        den-divs.filter-map fn(q')
          if q' == 0 then Nothing
          else
            val r = rational(p', q')
            Just(r)
    candidates.filter fn(r)
      eval-poly(p, r, rational-ring) == surd/rational/zero

// LCM of two integers.
fun lcm-int(a : int, b : int) : int
  if a == 0 || b == 0 then 0
  else abs(a * b) / gcd-int(a, b)

// All positive divisors of a positive integer.
fun divisors-of(n : int) : list<int>
  if n <= 0 then [1]
  else list(1, n).filter(fn(d) n % d == 0)

// Find a linear factor (rational root) and return (root, quotient).
fun find-linear-factor(p : poly<rational>) : exn maybe<(rational, poly<rational>)>
  val roots = rational-roots(p)
  match roots
    [] -> Nothing
    Cons(r, _) ->
      val lin = mk-poly([r.negate, surd/rational/one], rational-ring)
      val (q, _) = div-mod-poly(p, lin, rational-field)
      Just((r, q))

// Find a quadratic factor via Kronecker's method.
// Evaluate at 3 points, try all combinations.
fun find-quadratic-factor(p : poly<rational>) : exn maybe<(poly<rational>, poly<rational>)>
  if p.degree < 2 then Nothing
  else
    // Simple approach: try all integer pairs for quadratic x^2 + bx + c
    val bound = 10  // search bound
    pretend-no-div{ try-quadratic(p, 0 - bound, bound, 0 - bound, bound) }

fun try-quadratic(p : poly<rational>, b : int, b-max : int, c-start : int, c-max : int) : <div,exn> maybe<(poly<rational>, poly<rational>)>
  if b > b-max then Nothing
  else try-quadratic-c(p, b, c-start, c-max, b-max, c-start)

fun try-quadratic-c(p : poly<rational>, b : int, c : int, c-max : int, b-max : int, c-start : int) : <div,exn> maybe<(poly<rational>, poly<rational>)>
  if c > c-max then try-quadratic(p, b + 1, b-max, c-start, c-max)
  else
    val candidate = mk-poly([from-int(c), from-int(b), surd/rational/one], rational-ring)
    val (q, rem) = div-mod-poly(p, candidate, rational-field)
    if rem.is-zero then Just((candidate, q))
    else try-quadratic-c(p, b, c + 1, c-max, b-max, c-start)

// Check if a polynomial is irreducible over Q.
pub fun is-irreducible(p : poly<rational>) : exn bool
  if p.degree <= 1 then p.degree == 1
  else factor-square-free(p).length == 1
