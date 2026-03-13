// Real root isolation for polynomials with rational coefficients.
//
// Uses Sturm's theorem with bisection to separate and refine roots
// of square-free polynomials.
module surd/root-isolation

import std/core/undiv
import surd/rational
import surd/ring
import surd/poly
import surd/interval
import surd/root-bound

// An isolating interval for a real root of a polynomial.
pub struct isolating-interval
  ii-poly : poly<rational>
  ii-interval : interval

// Isolate all real roots of a polynomial.
// Returns isolating intervals ordered by increasing root value.
pub fun isolate-real-roots(p : poly<rational>) : exn list<isolating-interval>
  if p.degree <= 0 then []
  elif p.degree == 1 then
    match p.coeffs
      [a, b] ->
        val r = surd/rational/zero - a / b
        [Isolating-interval(monic-poly(p, rational-field), Interval(r, r))]
      _ -> []
  else
    val p' = monic-poly(p, rational-field)
    val bound = root-bound(p')
    val iv = Interval(bound.negate, bound)
    pretend-no-div{ isolate-in(p', iv).map(fn(i) Isolating-interval(p', i)) }

// Isolate roots within a given interval using Sturm's theorem and bisection.
fun isolate-in(p : poly<rational>, iv : interval) : <div,exn> list<interval>
  val sc = sturm-count(p, iv.lo, iv.hi)
  if sc == 0 then []
  elif sc == 1 then [iv]
  else
    val m = midpoint(iv)
    if eval-poly(p, m, rational-ring) == surd/rational/zero then
      Cons(Interval(m, m), isolate-in(p, Interval(iv.lo, m)) ++ isolate-in(p, Interval(m, iv.hi)))
    else
      isolate-in(p, Interval(iv.lo, m)) ++ isolate-in(p, Interval(m, iv.hi))

// Sturm sequence for a polynomial.
fun sturm-sequence(p : poly<rational>) : exn list<poly<rational>>
  val p' = diff-poly(p, rational-ring)
  pretend-no-div{ sturm-seq-build([p, p']) }

fun sturm-seq-build(seq : list<poly<rational>>) : <div,exn> list<poly<rational>>
  match seq
    [] -> []
    [_] -> seq
    _ ->
      val p0 = seq.drop(seq.length - 2).head.unjust
      val p1 = seq.last.unjust
      if p1.is-zero then seq
      else
        val (_, rem) = div-mod-poly(p0, p1, rational-field)
        val neg-rem = neg-poly(rem, rational-ring)
        if neg-rem.is-zero then seq
        else sturm-seq-build(seq ++ [neg-rem])

// Count sign changes in a sequence of values.
fun sign-changes(values : list<rational>) : int
  val non-zero = values.filter(fn(v) !v.is-zero)
  pretend-no-div{ count-sign-changes(non-zero) }

fun count-sign-changes(xs : list<rational>) : div int
  match xs
    [] -> 0
    [_] -> 0
    Cons(a, rest) -> match rest
      Cons(b, _) ->
        val change = if (a.is-pos && b.is-neg) || (a.is-neg && b.is-pos) then 1 else 0
        change + count-sign-changes(rest)
      _ -> 0

// Count the number of real roots in the interval (a, b) using Sturm's theorem.
pub fun sturm-count(p : poly<rational>, a : rational, b : rational) : exn int
  val seq = sturm-sequence(p)
  val values-a = seq.map(fn(s) eval-poly(s, a, rational-ring))
  val values-b = seq.map(fn(s) eval-poly(s, b, rational-ring))
  val sa = sign-changes(values-a)
  val sb = sign-changes(values-b)
  abs(sa - sb)

// Refine an isolating interval until its width is less than epsilon.
pub fun refine-root(eps : rational, ii : isolating-interval) : exn isolating-interval
  val iv = pretend-no-div{ refine-interval-loop(ii.ii-poly, eps, ii.ii-interval) }
  Isolating-interval(ii.ii-poly, iv)

fun refine-interval-loop(p : poly<rational>, eps : rational, iv : interval) : <div,exn> interval
  if width(iv) < eps then iv
  else
    val m = midpoint(iv)
    val fm = eval-poly(p, m, rational-ring)
    if fm.is-zero then Interval(m, m)
    else
      val flo = eval-poly(p, iv.lo, rational-ring)
      if (flo.is-pos && fm.is-neg) || (flo.is-neg && fm.is-pos) then
        refine-interval-loop(p, eps, Interval(iv.lo, m))
      else
        refine-interval-loop(p, eps, Interval(m, iv.hi))

// Check if a rational is a root within the interval.
pub fun root-in-interval(ii : isolating-interval) : maybe<rational>
  val iv = ii.ii-interval
  if iv.lo == iv.hi && eval-poly(ii.ii-poly, iv.lo, rational-ring) == surd/rational/zero then
    Just(iv.lo)
  else Nothing
