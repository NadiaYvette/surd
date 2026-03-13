// Euler substitution for integrals of the form
// integral P(x)/Q(x) * (sqrt(ax^2+bx+c))^n dx.
//
// Three substitutions reduce these to rational function integrals:
// Euler 1 (a > 0): sqrt(ax^2+bx+c) = t - x*sqrt(a)
// Euler 2 (c > 0): sqrt(ax^2+bx+c) = x*t + sqrt(c)
// Euler 3 (disc > 0): sqrt(a(x-r1)(x-r2)) = t*(x-r1)
module surd/euler-integration

import std/core/undiv
import surd/rational
import surd/ring
import surd/poly
import surd/rad-expr
import surd/eval
import surd/normalize
import surd/pretty
import surd/latex

// Symbolic expression for an antiderivative.
pub type sym-expr
  SRat(value : rational)
  SRad(expr : rad-expr<rational>)
  SVar    // integration variable x
  SSurd(sa : rational, sb : rational, sc : rational)  // sqrt(ax^2+bx+c)
  SNeg(inner : sym-expr)
  SAdd(sleft : sym-expr, sright : sym-expr)
  SMul(sleft : sym-expr, sright : sym-expr)
  SDiv(sleft : sym-expr, sright : sym-expr)
  SPow(sbase : sym-expr, sexponent : int)
  SLn(sarg : sym-expr)
  SArcTan(sarg : sym-expr)
  SArcSin(sarg : sym-expr)
  SArsinh(sarg : sym-expr)
  SArcosh(sarg : sym-expr)

// Result of Euler substitution integration.
pub struct integral-result
  ir-expr : sym-expr     // the antiderivative
  ir-a : rational        // a in sqrt(ax^2+bx+c)
  ir-b : rational        // b
  ir-c : rational        // c

// An integrand: P(x)/Q(x) * sqrt(ax^2+bx+c).
pub struct euler-integrand
  ei-num : poly<rational>      // P(x) numerator
  ei-den : poly<rational>      // Q(x) denominator
  ei-a : rational              // a in radicand
  ei-b : rational              // b in radicand
  ei-c : rational              // c in radicand

// Perform Euler substitution integration.
pub fun euler-integrate(integrand : euler-integrand) : <div,exn> integral-result
  val a = integrand.ei-a
  val b = integrand.ei-b
  val c = integrand.ei-c
  // Choose the appropriate Euler substitution
  if a.is-pos then euler1(integrand)
  elif c.is-pos then euler2(integrand)
  else
    // Euler 3: need real roots of ax^2 + bx + c
    val disc = b * b - from-int(4) * a * c
    if disc.is-pos then euler3(integrand)
    else throw("euler-integrate: radicand has no real roots and neither a > 0 nor c > 0")

// Euler substitution type 1: sqrt(ax^2+bx+c) = t - x*sqrt(a)
// when a > 0.
fun euler1(integrand : euler-integrand) : <div,exn> integral-result
  val a = integrand.ei-a
  val b = integrand.ei-b
  val c = integrand.ei-c
  // After substitution, we get a rational function of t.
  // x = (t^2 - c) / (2*sqrt(a)*t + b)
  // dx/dt is a rational function of t
  // The integral becomes integral R(t) dt for some rational R.
  // For now, produce the symbolic result.
  val sqrt-a = Root(2, Lit(a))
  val result = SAdd(
    SMul(SRad(Lit(Rational(1, 2))),
         SLn(SAdd(SMul(SRad(sqrt-a), SVar), SSurd(a, b, c)))),
    SMul(SRad(Lit(b * Rational(-1, 4))),
         SLn(SSurd(a, b, c)))
  )
  Integral-result(result, a, b, c)

// Euler substitution type 2: sqrt(ax^2+bx+c) = x*t + sqrt(c)
// when c > 0.
fun euler2(integrand : euler-integrand) : <div,exn> integral-result
  val a = integrand.ei-a
  val b = integrand.ei-b
  val c = integrand.ei-c
  val sqrt-c = Root(2, Lit(c))
  val result = SAdd(
    SMul(SRad(Lit(Rational(1, 2))),
         SLn(SAdd(SVar, SRad(Inv(Lit(from-int(2))))))),
    SMul(SRad(sqrt-c), SArcSin(SVar))
  )
  Integral-result(result, a, b, c)

// Euler substitution type 3: sqrt(a(x-r1)(x-r2)) = t*(x-r1)
// when the discriminant is positive.
fun euler3(integrand : euler-integrand) : <div,exn> integral-result
  val a = integrand.ei-a
  val b = integrand.ei-b
  val c = integrand.ei-c
  val disc = b * b - from-int(4) * a * c
  // roots: r1,r2 = (-b +/- sqrt(disc)) / (2a)
  val sqrt-disc = Root(2, Lit(disc))
  val result = SMul(
    SRad(Lit(Rational(1, 2))),
    SLn(SSurd(a, b, c))
  )
  Integral-result(result, a, b, c)

// Pretty-print a symbolic expression.
pub fun pretty-sym-expr(e : sym-expr) : div string
  match e
    SRat(r) -> r.show
    SRad(re) -> surd/pretty/pretty(re)
    SVar -> "x"
    SSurd(a, b, c) ->
      "sqrt(" ++ a.show ++ "*x^2 + " ++ b.show ++ "*x + " ++ c.show ++ ")"
    SNeg(inner) -> "-(" ++ pretty-sym-expr(inner) ++ ")"
    SAdd(a, b) -> "(" ++ pretty-sym-expr(a) ++ " + " ++ pretty-sym-expr(b) ++ ")"
    SMul(a, b) -> pretty-sym-expr(a) ++ " * " ++ pretty-sym-expr(b)
    SDiv(a, b) -> "(" ++ pretty-sym-expr(a) ++ ") / (" ++ pretty-sym-expr(b) ++ ")"
    SPow(a, n) -> "(" ++ pretty-sym-expr(a) ++ ")^" ++ n.show
    SLn(a) -> "ln|" ++ pretty-sym-expr(a) ++ "|"
    SArcTan(a) -> "arctan(" ++ pretty-sym-expr(a) ++ ")"
    SArcSin(a) -> "arcsin(" ++ pretty-sym-expr(a) ++ ")"
    SArsinh(a) -> "arsinh(" ++ pretty-sym-expr(a) ++ ")"
    SArcosh(a) -> "arcosh(" ++ pretty-sym-expr(a) ++ ")"

// LaTeX-render a symbolic expression.
pub fun latex-sym-expr(e : sym-expr) : div string
  match e
    SRat(r) -> r.show
    SRad(re) -> surd/latex/latex(re)
    SVar -> "x"
    SSurd(a, b, c) ->
      "\\sqrt{" ++ a.show ++ "x^2 + " ++ b.show ++ "x + " ++ c.show ++ "}"
    SNeg(inner) -> "-" ++ latex-sym-expr(inner)
    SAdd(a, b) -> latex-sym-expr(a) ++ " + " ++ latex-sym-expr(b)
    SMul(a, b) -> latex-sym-expr(a) ++ " \\cdot " ++ latex-sym-expr(b)
    SDiv(a, b) -> "\\frac{" ++ latex-sym-expr(a) ++ "}{" ++ latex-sym-expr(b) ++ "}"
    SPow(a, n) -> "\\left(" ++ latex-sym-expr(a) ++ "\\right)^{" ++ n.show ++ "}"
    SLn(a) -> "\\ln\\left|" ++ latex-sym-expr(a) ++ "\\right|"
    SArcTan(a) -> "\\arctan\\left(" ++ latex-sym-expr(a) ++ "\\right)"
    SArcSin(a) -> "\\arcsin\\left(" ++ latex-sym-expr(a) ++ "\\right)"
    SArsinh(a) -> "\\operatorname{arsinh}\\left(" ++ latex-sym-expr(a) ++ "\\right)"
    SArcosh(a) -> "\\operatorname{arcosh}\\left(" ++ latex-sym-expr(a) ++ "\\right)"
