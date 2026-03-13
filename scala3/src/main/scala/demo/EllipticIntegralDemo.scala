package demo

import surd.*
import surd.EllipticIntegration.*

/** Demo: elliptic integral reduction to Legendre normal forms.
  *
  * Shows how integral R(x) dx / sqrt(P(x)) with P degree 3 or 4
  * reduces to F(phi,k), E(phi,k), and Pi(phi,n,k) with exact radical modulus k.
  */
object EllipticIntegralDemo:
  given Field[Rational] = summon[Field[Rational]]

  final case class Example(
      name: String,
      integrand: EllipticIntegrand,
      notes: String
  )

  val examples: Vector[Example] = Vector(
    Example(
      name = "integral dx / sqrt(x^3 - x)",
      integrand = EllipticIntegrand(
        num = Poly.const(Rational.one),
        den = Poly.const(Rational.one),
        radicand = Poly(Vector(Rational.zero, -Rational.one, Rational.zero, Rational.one))
      ),
      notes = "roots 1, 0, -1; k^2 = 1/2"
    ),
    Example(
      name = "integral dx / sqrt(x^3 - 1)",
      integrand = EllipticIntegrand(
        num = Poly.const(Rational.one),
        den = Poly.const(Rational.one),
        radicand = Poly(Vector(-Rational.one, Rational.zero, Rational.zero, Rational.one))
      ),
      notes = "one rational root at 1, two complex -- should fail"
    ),
    Example(
      name = "integral dx / sqrt((1-x^2)(1-x^2/2))",
      integrand = EllipticIntegrand(
        num = Poly.const(Rational.one),
        den = Poly.const(Rational.one),
        radicand = Poly(Vector(
          Rational.one, Rational.zero,
          Rational(-3, 2), Rational.zero,
          Rational(1, 2)
        ))
      ),
      notes = "already Legendre-like; roots +/-1, +/-sqrt(2); k^2 = 1/2"
    ),
    Example(
      name = "integral dx / sqrt(4x^3 - 4x)",
      integrand = EllipticIntegrand(
        num = Poly.const(Rational.one),
        den = Poly.const(Rational.one),
        radicand = Poly(Vector(Rational.zero, Rational(-4), Rational.zero, Rational(4)))
      ),
      notes = "same roots as #1, leading coeff 4; k^2 = 1/2"
    ),
    Example(
      name = "integral dx / sqrt((x^2-1)(x^2-4))",
      integrand = EllipticIntegrand(
        num = Poly.const(Rational.one),
        den = Poly.const(Rational.one),
        radicand = Poly(Vector(
          Rational(4), Rational.zero,
          Rational(-5), Rational.zero,
          Rational.one
        ))
      ),
      notes = "roots 2, 1, -1, -2; quartic"
    ),
    Example(
      name = "integral dx / ((x-3)*sqrt(x^3 - x))",
      integrand = EllipticIntegrand(
        num = Poly.const(Rational.one),
        den = Poly(Vector(Rational(-3), Rational.one)),
        radicand = Poly(Vector(Rational.zero, -Rational.one, Rational.zero, Rational.one))
      ),
      notes = "Pi form: pole at x=3"
    ),
    Example(
      name = "integral dx / sqrt(x^3 - 7x + 6)",
      integrand = EllipticIntegrand(
        num = Poly.const(Rational.one),
        den = Poly.const(Rational.one),
        radicand = Poly(Vector(Rational(6), Rational(-7), Rational.zero, Rational.one))
      ),
      notes = "roots 2, 1, -3; factors (x-1)(x-2)(x+3)"
    )
  )

  def main(args: Array[String]): Unit =
    println("Elliptic Integral Reduction to Legendre Normal Form")
    println("=" * 60)
    println()

    println("--- Legendre form ---")
    println()

    for ex <- examples do
      println(s"--- ${ex.name} ---")
      println(s"  (${ex.notes})")
      reduceElliptic(jacobi = false, ex.integrand) match
        case None =>
          println("  (cannot reduce: not all roots are real, or degree unsupported)")
        case Some(result) =>
          println(prettyEllipticResult(result))
      println()

    println()
    println("--- Jacobi inverse form ---")
    println()

    for ex <- Vector(examples(0), examples(4)) do
      println(s"--- ${ex.name} ---")
      println(s"  (${ex.notes})")
      reduceElliptic(jacobi = true, ex.integrand) match
        case None =>
          println("  (cannot reduce)")
        case Some(result) =>
          println(prettyEllipticResult(result))
      println()
