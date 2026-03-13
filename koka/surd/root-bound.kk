// Root bounding and approximate root finding for rational polynomials.
//
// Cauchy's bound and bisection-based root finding.
module surd/root-bound

import std/core/undiv
import std/num/float64
import surd/rational
import surd/ring
import surd/poly

// Cauchy's root bound: all roots have |r| <= 1 + max|c_i/c_n|.
pub fun root-bound(p : poly<rational>) : exn rational
  match p.coeffs
    [] -> surd/rational/zero
    cs ->
      val lc = cs.last.unjust
      if lc.is-zero then throw("root-bound: zero leading coefficient")
      else
        val ratios = pretend-no-div{ cs.list-init.map(fn(c) rational/abs(c / lc)) }
        val mx = ratios.foldl(surd/rational/zero, fn(a, b) rational/max(a, b))
        surd/rational/one + mx

// Simple list init (all but last).
fun list-init(xs : list<a>) : div list<a>
  match xs
    [] -> []
    [_] -> []
    Cons(x, rest) -> Cons(x, list-init(rest))

// Find approximate real roots of a polynomial by scanning for sign changes.
pub fun approx-roots(p : poly<rational>) : exn list<rational>
  if p.degree <= 0 then []
  elif p.degree == 1 then
    match p.coeffs
      [a, b] -> [Rational(0,1) - a / b]
      _ -> []
  else
    val bound = root-bound(p)
    val lo-int = rational/floor(bound.negate * from-int(10))
    val hi-int = rational/ceiling(bound * from-int(10))
    val pts = list(lo-int, hi-int).map(fn(i) from-int(i) * Rational(1, 10))
    val signs = pts.map(fn(x) (x, rational/signum(eval-poly(p, x, rational-ring))))
    val changes = find-sign-changes(signs)
    val roots = changes.map(fn(pair) bisect-root(p, pair.fst, pair.snd, 50))
    if roots.is-empty then [surd/rational/zero] else roots

// Find consecutive points with sign changes.
fun find-sign-changes(signs : list<(rational, rational)>) : list<(rational, rational)>
  match signs
    [] -> []
    [_] -> []
    Cons((x1, s1), rest) -> match rest
      Cons((x2, s2), _) ->
        val tail = find-sign-changes(rest)
        if s1 != s2 && !(s1.is-zero) && !(s2.is-zero) then
          Cons((x1, x2), tail)
        else tail
      _ -> []

// Bisection root finding.
pub fun bisect-root(p : poly<rational>, lo : rational, hi : rational, iters : int) : exn rational
  pretend-no-div{ bisect-root-rec(p, lo, hi, iters) }

fun bisect-root-rec(p : poly<rational>, lo : rational, hi : rational, n : int) : <div,exn> rational
  if n <= 0 then lo
  else
    val mid = (lo + hi) * Rational(1, 2)
    val fmid = eval-poly(p, mid, rational-ring)
    val flo = eval-poly(p, lo, rational-ring)
    if fmid.is-zero then mid
    elif rational/signum(fmid) == rational/signum(flo) then bisect-root-rec(p, mid, hi, n - 1)
    else bisect-root-rec(p, lo, mid, n - 1)

// Pick the polynomial factor whose numerical root is closest to target.
pub fun pick-closest(target : float64, factors : list<poly<rational>>) : exn poly<rational>
  match factors
    [] -> throw("pick-closest: empty factor list")
    [f] -> f
    Cons(f, rest) ->
      val best-dist = min-root-dist(f, target)
      pick-closest-loop(rest, f, best-dist, target)

fun pick-closest-loop(fs : list<poly<rational>>, best : poly<rational>,
                      best-dist : float64, target : float64) : exn poly<rational>
  match fs
    [] -> best
    Cons(f, rest) ->
      val d = min-root-dist(f, target)
      if d < best-dist then pick-closest-loop(rest, f, d, target)
      else pick-closest-loop(rest, best, best-dist, target)

fun min-root-dist(f : poly<rational>, target : float64) : exn float64
  val roots = approx-roots(f)
  val dists = roots.map(fn(r) abs(r.num.float64 / r.den.float64 - target))
  dists.foldl(1.0e300, fn(a, b) min(a, b))
