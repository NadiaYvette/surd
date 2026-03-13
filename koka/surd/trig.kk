// Exact symbolic evaluation of trigonometric functions at
// rational multiples of pi.
//
// Every trig value at a rational multiple of pi can be expressed in
// radicals. The primary entry points are cos-exact and sin-exact.
module surd/trig

import std/core/undiv
import surd/rational
import surd/rad-expr
import surd/normalize
import surd/eval
import surd/expr
import surd/dag
import surd/normal-form
import surd/root-of-unity
import surd/trig-galois
import surd/pretty
import surd/latex

// Result of exact trig evaluation.
pub type trig-result
  // Exact radical expression.
  Radical(expr : rad-expr<rational>)
  // Minimal polynomial fallback.
  MinPolyResult(coefficients : list<rational>)

// Compute cos(p*pi/q) exactly.
// Reduces to cos(2*pi/n) via standard identities, then uses
// the root-of-unity machinery.
pub fun cos-exact(p : int, q : int) : <div,exn> trig-result
  if q <= 0 then throw("cos-exact: non-positive denominator")
  else
    val g = gcd-int(abs(p), q)
    val p' = p / g
    val q' = q / g
    cos-reduced(p', q')

// Compute sin(p*pi/q) exactly.
pub fun sin-exact(p : int, q : int) : <div,exn> trig-result
  if q <= 0 then throw("sin-exact: non-positive denominator")
  else
    val g = gcd-int(abs(p), q)
    val p' = p / g
    val q' = q / g
    sin-reduced(p', q')

// Compute tan(p*pi/q) exactly, as sin/cos.
pub fun tan-exact(p : int, q : int) : <div,exn> maybe<trig-result>
  match (sin-exact(p, q), cos-exact(p, q))
    (Radical(s), Radical(c)) ->
      Just(Radical(RMul(s, Inv(c))))
    _ -> Nothing

fun cos-reduced(p : int, q : int) : <div,exn> trig-result
  val p' = p % (2 * q)
  val p'' = if p' < 0 then p' + 2 * q else p'
  cos-in-range(p'', q)

fun sin-reduced(p : int, q : int) : <div,exn> trig-result
  val p' = p % (2 * q)
  val p'' = if p' < 0 then p' + 2 * q else p'
  val positive = p'' >= 0 && p'' <= q
  if p'' == 0 || p'' == q then Radical(Lit(surd/rational/zero))
  else
    // Fall back to +/-sqrt(1 - cos^2)
    match cos-exact(p, q)
      Radical(c) ->
        val sin2 = RAdd(Lit(surd/rational/one), Neg(RMul(c, c)))
        val sin-expr = Root(2, sin2)
        val signed = if positive then sin-expr else Neg(sin-expr)
        Radical(safe-denest-and-normalize(signed))
      other -> other

// cos(p*pi/q) where 0 <= p <= 2q (angle in [0, 2*pi]).
fun cos-in-range(p : int, q : int) : <div,exn> trig-result
  if p == 0 then Radical(Lit(surd/rational/one))
  elif 2 * p == q then Radical(Lit(surd/rational/zero))
  elif p == q then Radical(Lit(Rational(-1, 1)))
  elif 2 * p == 3 * q then Radical(Lit(surd/rational/zero))
  elif 2 * p > q && p < q then
    // cos(pi - x) = -cos(x)
    match cos-in-range(q - p, q)
      Radical(e) -> Radical(Neg(e))
      other -> other
  elif p > q && 2 * p < 3 * q then
    // cos(pi + x) = -cos(x)
    match cos-in-range(p - q, q)
      Radical(e) -> Radical(Neg(e))
      other -> other
  elif 2 * p >= 3 * q then
    // cos(2pi - x) = cos(x)
    cos-in-range(2 * q - p, q)
  else cos-first-quadrant(p, q)

fun cos-first-quadrant(p : int, q : int) : <div,exn> trig-result
  val g = gcd-int(p, 2 * q)
  val n = (2 * q) / g
  val k = p / g
  if k == 1 then
    match cos-of-unity(n)
      Just(e) -> Radical(safe-denest-and-normalize(e))
      Nothing -> MinPolyResult([])
  else
    match cos-of-unity(n)
      Just(base) ->
        val cheb = chebyshev(k, base)
        Radical(safe-denest-and-normalize(cheb))
      Nothing -> MinPolyResult([])

// Chebyshev polynomial evaluation: T_k(x) computed symbolically.
// T_0(x) = 1, T_1(x) = x, T_{n+1}(x) = 2x*T_n(x) - T_{n-1}(x)
pub fun chebyshev(k : int, x : rad-expr<rational>) : <div,exn> rad-expr<rational>
  if k == 0 then Lit(surd/rational/one)
  elif k == 1 then x
  else pretend-no-div{ cheb-loop(2, Lit(surd/rational/one), x, k, x) }

fun cheb-loop(n : int, t0 : rad-expr<rational>, t1 : rad-expr<rational>,
              k : int, x : rad-expr<rational>) : <div,exn> rad-expr<rational>
  if n > k then t1
  else
    val t2 = RAdd(RMul(RMul(Lit(from-int(2)), x), t1), Neg(t0))
    cheb-loop(n + 1, t1, t2, k, x)

// Try denesting followed by normalization.
// Three tiers based on expression size.
fun safe-denest-and-normalize(e : rad-expr<rational>) : <div,exn> rad-expr<rational>
  val dag = to-dag(e)
  val d = dag-depth(dag)
  val s = dag-size(dag)
  if d > 50 || s > 5000 then
    from-dag(dag-fold-constants(dag))
  elif d <= 20 && s <= 500 then
    val folded = normalize(from-dag(dag-fold-constants(dag)))
    folded
  else
    from-dag(dag-fold-constants(dag))

// Simplify a trig result for display.
pub fun simplify-trig-result(r : trig-result) : <div,exn> trig-result
  match r
    Radical(e) ->
      // NF round-trip for canonicalization
      val nrads = collect-radicals(e).length
      if nrads > 5 then Radical(e)
      else
        val simplified = from-norm-expr(to-norm-expr(e))
        Radical(simplified)
    other -> other

// Show a trig result.
pub fun trig-result/show(r : trig-result) : div string
  match r
    Radical(e) -> "Radical(" ++ rad-rational/show(e) ++ ")"
    MinPolyResult(cs) -> "MinPoly(" ++ cs.map(fn(c) c.show).join(", ") ++ ")"

// Pretty-print a trig result.
pub fun trig-result/pretty(r : trig-result) : div string
  match r
    Radical(e) -> surd/pretty/pretty(e)
    MinPolyResult(_) -> "[minimal polynomial]"

// LaTeX-render a trig result.
pub fun trig-result/latex(r : trig-result) : div string
  match r
    Radical(e) -> surd/latex/latex(e)
    MinPolyResult(_) -> "\\text{[minimal polynomial]}"
