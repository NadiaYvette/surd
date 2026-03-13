// Demo: Euler substitution integration examples.
//
// Computes antiderivatives of integrals involving square roots
// of quadratics using Euler's three substitutions.
module surd/demo/euler-integral

import surd/rational
import surd/ring
import surd/poly
import surd/rad-expr
import surd/euler-integration
import surd/pretty

pub fun main() : <console,div,exn> ()
  println(repeat-char('=', 60))
  println("  Euler Substitution Integration Examples")
  println(repeat-char('=', 60))
  println("")

  // Example 1: integral dx / sqrt(x^2 + 1)
  // = ln|x + sqrt(x^2+1)| + C  (Euler 1, a=1 > 0)
  println("Example 1: integral dx / sqrt(x^2 + 1)")
  val integrand1 = Euler-integrand(
    ei-num = const-poly(surd/rational/one, rational-ring),
    ei-den = const-poly(surd/rational/one, rational-ring),
    ei-a = surd/rational/one,
    ei-b = surd/rational/zero,
    ei-c = surd/rational/one
  )
  val result1 = euler-integrate(integrand1)
  println("  Antiderivative: " ++ pretty-sym-expr(result1.ir-expr))
  println("  Radicand: sqrt(" ++ result1.ir-a.show ++ "*x^2 + " ++
          result1.ir-b.show ++ "*x + " ++ result1.ir-c.show ++ ")")
  println("")

  // Example 2: integral dx / sqrt(1 - x^2)
  // = arcsin(x) + C  (Euler 2, c=1 > 0)
  println("Example 2: integral dx / sqrt(1 - x^2)")
  val integrand2 = Euler-integrand(
    ei-num = const-poly(surd/rational/one, rational-ring),
    ei-den = const-poly(surd/rational/one, rational-ring),
    ei-a = Rational(-1, 1),
    ei-b = surd/rational/zero,
    ei-c = surd/rational/one
  )
  val result2 = euler-integrate(integrand2)
  println("  Antiderivative: " ++ pretty-sym-expr(result2.ir-expr))
  println("")

  // Example 3: integral dx / sqrt(2x^2 + 3x + 1)
  // Euler 1 since a=2 > 0
  println("Example 3: integral dx / sqrt(2x^2 + 3x + 1)")
  val integrand3 = Euler-integrand(
    ei-num = const-poly(surd/rational/one, rational-ring),
    ei-den = const-poly(surd/rational/one, rational-ring),
    ei-a = from-int(2),
    ei-b = from-int(3),
    ei-c = surd/rational/one
  )
  val result3 = euler-integrate(integrand3)
  println("  Antiderivative: " ++ pretty-sym-expr(result3.ir-expr))
  println("")

  // Example 4: integral x dx / sqrt(4 + x^2)
  println("Example 4: integral x dx / sqrt(4 + x^2)")
  val integrand4 = Euler-integrand(
    ei-num = mk-poly([surd/rational/zero, surd/rational/one], rational-ring), // x
    ei-den = const-poly(surd/rational/one, rational-ring),
    ei-a = surd/rational/one,
    ei-b = surd/rational/zero,
    ei-c = from-int(4)
  )
  val result4 = euler-integrate(integrand4)
  println("  Antiderivative: " ++ pretty-sym-expr(result4.ir-expr))
  println("")

  println("Done.")

fun pad-right(s : string, n : int) : string
  val len = s.count
  if len >= n then s
  else s ++ repeat-char(' ', n - len)

fun repeat-char(c : char, n : int) : string
  list(1, n).map(fn(_) c.string).join("")
