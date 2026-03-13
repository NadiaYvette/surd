// Demo: reduction of elliptic integrals to Legendre normal forms.
//
// Shows how integrals of the form integral R(x) dx / sqrt(P(x))
// are reduced to standard elliptic integrals F, E, Pi.
module surd/demo/elliptic-integral

import surd/rational
import surd/ring
import surd/poly
import surd/rad-expr
import surd/elliptic-integration
import surd/pretty

pub fun main() : <console,div,exn> ()
  println("=" ++ repeat-char('=', 60))
  println("  Elliptic Integral Reduction Examples")
  println("=" ++ repeat-char('=', 60))
  println("")

  // Example 1: integral dx / sqrt(x^3 - x)
  // Cubic radicand: P(x) = x^3 - x = x(x-1)(x+1)
  println("Example 1: integral dx / sqrt(x^3 - x)")
  val radicand1 = mk-poly([surd/rational/zero, Rational(-1, 1), surd/rational/zero, surd/rational/one], rational-ring)
  // P(x) = -x + x^3 = x^3 - x
  val integrand1 = Elliptic-integrand(
    el-num = const-poly(surd/rational/one, rational-ring),
    el-den = const-poly(surd/rational/one, rational-ring),
    el-radicand = radicand1
  )
  val result1 = reduce-elliptic(integrand1)
  println("  Modulus k = " ++ surd/pretty/pretty(result1.er-modulus))
  println("  Reduction: " ++ pretty-elliptic-result(result1))
  println("")

  // Example 2: integral dx / sqrt(x^4 - 1)
  // Quartic radicand: P(x) = x^4 - 1
  println("Example 2: integral dx / sqrt(x^4 - 1)")
  val radicand2 = mk-poly([Rational(-1, 1), surd/rational/zero, surd/rational/zero, surd/rational/zero, surd/rational/one], rational-ring)
  val integrand2 = Elliptic-integrand(
    el-num = const-poly(surd/rational/one, rational-ring),
    el-den = const-poly(surd/rational/one, rational-ring),
    el-radicand = radicand2
  )
  val result2 = reduce-elliptic(integrand2)
  println("  Modulus k = " ++ surd/pretty/pretty(result2.er-modulus))
  println("  Reduction: " ++ pretty-elliptic-result(result2))
  println("")

  // Example 3: integral dx / sqrt((1-x^2)(1-k^2*x^2)) with k=1/2
  // This is already in Legendre normal form for F(phi, k)
  println("Example 3: integral dx / sqrt((1-x^2)(1-x^2/4))")
  // Radicand: (1-x^2)(1-x^2/4) = 1 - 5x^2/4 + x^4/4
  val radicand3 = mk-poly([surd/rational/one, surd/rational/zero, Rational(-5, 4), surd/rational/zero, Rational(1, 4)], rational-ring)
  val integrand3 = Elliptic-integrand(
    el-num = const-poly(surd/rational/one, rational-ring),
    el-den = const-poly(surd/rational/one, rational-ring),
    el-radicand = radicand3
  )
  val result3 = reduce-elliptic(integrand3)
  println("  Modulus k = " ++ surd/pretty/pretty(result3.er-modulus))
  println("  Reduction: " ++ pretty-elliptic-result(result3))
  println("")

  println("Done.")

fun repeat-char(c : char, n : int) : string
  list(1, n).map(fn(_) c.string).join("")
