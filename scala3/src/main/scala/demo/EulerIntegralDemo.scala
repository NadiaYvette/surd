package demo

import surd.*
import surd.EulerIntegration.*

/** Demo: Euler substitution integrals.
  *
  * Computes antiderivatives of integral P(x)/Q(x) * (sqrt(ax^2+bx+c))^n dx
  * using Euler's three substitutions.
  */
object EulerIntegralDemo:
  given Field[Rational] = summon[Field[Rational]]

  final case class Example(
      name: String,
      integrand: EulerIntegrand,
      known: String
  )

  val examples: Vector[Example] = Vector(
    Example(
      name = "integral dx / sqrt(x^2+1)",
      integrand = EulerIntegrand(
        p = Poly.const(Rational.one),
        q = Poly.const(Rational.one),
        sqrtPow = -1,
        a = Rational.one,
        b = Rational.zero,
        c = Rational.one
      ),
      known = "ln|x + sqrt(x^2+1)|"
    ),
    Example(
      name = "integral dx / sqrt(x^2-1)",
      integrand = EulerIntegrand(
        p = Poly.const(Rational.one),
        q = Poly.const(Rational.one),
        sqrtPow = -1,
        a = Rational.one,
        b = Rational.zero,
        c = -Rational.one
      ),
      known = "ln|x + sqrt(x^2-1)|"
    ),
    Example(
      name = "integral dx / sqrt(x^2+2x+2)",
      integrand = EulerIntegrand(
        p = Poly.const(Rational.one),
        q = Poly.const(Rational.one),
        sqrtPow = -1,
        a = Rational.one,
        b = Rational(2),
        c = Rational(2)
      ),
      known = "ln|x + 1 + sqrt(x^2+2x+2)|"
    ),
    Example(
      name = "integral x dx / sqrt(x^2+1)",
      integrand = EulerIntegrand(
        p = Poly(Vector(Rational.zero, Rational.one)),
        q = Poly.const(Rational.one),
        sqrtPow = -1,
        a = Rational.one,
        b = Rational.zero,
        c = Rational.one
      ),
      known = "sqrt(x^2+1)"
    ),
    Example(
      name = "integral dx / sqrt(4x^2+1)",
      integrand = EulerIntegrand(
        p = Poly.const(Rational.one),
        q = Poly.const(Rational.one),
        sqrtPow = -1,
        a = Rational(4),
        b = Rational.zero,
        c = Rational.one
      ),
      known = "(1/2)*ln|2x + sqrt(4x^2+1)|"
    ),
    Example(
      name = "integral sqrt(x^2+1) dx",
      integrand = EulerIntegrand(
        p = Poly.const(Rational.one),
        q = Poly.const(Rational.one),
        sqrtPow = 1,
        a = Rational.one,
        b = Rational.zero,
        c = Rational.one
      ),
      known = "(x*sqrt(x^2+1) + ln|x + sqrt(x^2+1)|) / 2"
    ),
    Example(
      name = "integral dx / sqrt(1-x^2)  [Euler 2]",
      integrand = EulerIntegrand(
        p = Poly.const(Rational.one),
        q = Poly.const(Rational.one),
        sqrtPow = -1,
        a = -Rational.one,
        b = Rational.zero,
        c = Rational.one
      ),
      known = "arcsin(x)"
    ),
    Example(
      name = "integral dx / sqrt(x^2-3x+2)  [Euler 3]",
      integrand = EulerIntegrand(
        p = Poly.const(Rational.one),
        q = Poly.const(Rational.one),
        sqrtPow = -1,
        a = Rational.one,
        b = Rational(-3),
        c = Rational(2)
      ),
      known = "ln|x - 3/2 + sqrt(x^2-3x+2)|"
    )
  )

  def main(args: Array[String]): Unit =
    println("Euler Substitution Integrals")
    println("=" * 60)
    println()

    for ex <- examples do
      println(s"--- ${ex.name} ---")
      eulerIntegrate(ex.integrand) match
        case None =>
          println("  (no rational Euler substitution found)")
        case Some(result) =>
          println(s"  = ${prettySymExpr(result.expr)}")
          println(s"  Substitution type: ${result.substitutionType}")
          println(s"  Known: ${ex.known}")
      println()
