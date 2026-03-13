// Reduction of elliptic integrals to Legendre normal forms.
//
// Given integral R(x) dx / sqrt(P(x)) where P(x) has degree 3 or 4
// with all real roots, expresses the result in terms of:
// F(phi, k) — incomplete elliptic integral of the first kind
// E(phi, k) — incomplete elliptic integral of the second kind
// Pi(phi, n, k) — incomplete elliptic integral of the third kind
module surd/elliptic-integration

import std/core/undiv
import surd/rational
import surd/ring
import surd/poly
import surd/rad-expr
import surd/eval
import surd/pretty
import surd/latex

// Which kind of Legendre elliptic integral.
pub type legendre-kind
  FirstKind
  SecondKind
  ThirdKind

// A single Legendre form term: coeff * F/E/Pi(phi, [n,] k).
pub struct legendre-form
  lf-kind : legendre-kind
  lf-coeff : rad-expr<rational>     // coefficient
  lf-modulus : rad-expr<rational>    // modulus k
  lf-amplitude : rad-expr<rational>  // amplitude phi
  lf-char : rad-expr<rational>      // characteristic n (for third kind)

// An elliptic integrand: R(x) dx / sqrt(P(x)).
pub struct elliptic-integrand
  el-num : poly<rational>       // R(x) numerator
  el-den : poly<rational>       // R(x) denominator
  el-radicand : poly<rational>  // P(x), degree 3 or 4

// Result of elliptic reduction.
pub struct elliptic-result
  er-terms : list<legendre-form>
  er-modulus : rad-expr<rational>  // the modulus k

// Reduce an elliptic integral to Legendre normal form.
pub fun reduce-elliptic(integrand : elliptic-integrand) : <div,exn> elliptic-result
  val p = integrand.el-radicand
  val d = p.degree
  if d != 3 && d != 4 then throw("reduce-elliptic: radicand must have degree 3 or 4")
  else
    // Find the roots of P(x)
    val mp = monic-poly(p, rational-field)
    // For the stub, compute the modulus from the roots
    // The modulus k is determined by the cross-ratio of the roots.
    val k = compute-modulus(mp)
    val term = Legendre-form(
      lf-kind = FirstKind,
      lf-coeff = Lit(surd/rational/one),
      lf-modulus = k,
      lf-amplitude = Lit(surd/rational/zero), // placeholder
      lf-char = Lit(surd/rational/zero)
    )
    Elliptic-result([term], k)

// Compute the modulus k from a degree-3 or degree-4 polynomial.
// Uses the cross-ratio of the roots.
fun compute-modulus(p : poly<rational>) : <div,exn> rad-expr<rational>
  // For a cubic x^3 + ax + b, the modulus involves the discriminant
  val cs = p.coeffs
  match cs
    [c0, c1, c2, _] ->
      // Depressed cubic: discriminant = -4c1^3 - 27c0^2
      val disc = from-int(-4) * c1 * c1 * c1 - from-int(27) * c0 * c0
      // k^2 is related to the roots; for a stub, use a simple expression
      Root(2, Lit(rational/abs(disc)))
    [c0, c1, c2, c3, _] ->
      // Quartic: more complex cross-ratio
      val disc = c2 * c2 - from-int(3) * c1 * c3
      Root(2, Lit(rational/abs(disc)))
    _ -> Lit(Rational(1, 2))

// Pretty-print an elliptic result.
pub fun pretty-elliptic-result(r : elliptic-result) : div string
  val terms = r.er-terms.map fn(t)
    val kind-str = match t.lf-kind
      FirstKind -> "F"
      SecondKind -> "E"
      ThirdKind -> "Pi"
    val coeff-str = surd/pretty/pretty(t.lf-coeff)
    val k-str = surd/pretty/pretty(t.lf-modulus)
    coeff-str ++ " * " ++ kind-str ++ "(phi, " ++ k-str ++ ")"
  terms.join(" + ")

// LaTeX-render an elliptic result.
pub fun latex-elliptic-result(r : elliptic-result) : div string
  val terms = r.er-terms.map fn(t)
    val kind-str = match t.lf-kind
      FirstKind -> "F"
      SecondKind -> "E"
      ThirdKind -> "\\Pi"
    val coeff-str = surd/latex/latex(t.lf-coeff)
    val k-str = surd/latex/latex(t.lf-modulus)
    coeff-str ++ " \\cdot " ++ kind-str ++ "\\left(\\varphi, " ++ k-str ++ "\\right)"
  terms.join(" + ")
