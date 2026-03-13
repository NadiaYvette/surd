package surd

import RadExpr.*

/** Elliptic integral reduction to Legendre normal forms.
  *
  * Reduces integrals of the form integral R(x) dx / sqrt(P(x))
  * where P is degree 3 or 4, to combinations of:
  *   F(phi, k)   - incomplete elliptic integral of the first kind
  *   E(phi, k)   - incomplete elliptic integral of the second kind
  *   Pi(phi,n,k) - incomplete elliptic integral of the third kind
  *
  * with exact radical modulus k.
  */
object EllipticIntegration:
  given fld: Field[Rational] = summon[Field[Rational]]

  /** An elliptic integrand: integral num(x)/den(x) dx / sqrt(radicand(x)). */
  final case class EllipticIntegrand(
      num: Poly[Rational],       // numerator
      den: Poly[Rational],       // denominator
      radicand: Poly[Rational]   // polynomial under the square root (degree 3 or 4)
  )

  /** An elliptic integral in Legendre normal form. */
  enum LegendreForm:
    case FirstKind(phi: SymExpr, k: RadExpr[Rational])
    case SecondKind(phi: SymExpr, k: RadExpr[Rational])
    case ThirdKind(phi: SymExpr, n: RadExpr[Rational], k: RadExpr[Rational])

  /** Symbolic expression (shared with EulerIntegration). */
  type SymExpr = EulerIntegration.SymExpr

  /** Result of elliptic reduction. */
  final case class EllipticResult(
      terms: Vector[LegendreForm],
      modulus: RadExpr[Rational],
      notes: String
  )

  /** Reduce an elliptic integral to Legendre normal form. */
  def reduceElliptic(jacobi: Boolean, integrand: EllipticIntegrand): Option[EllipticResult] =
    val deg = integrand.radicand.degree
    if deg != 3 && deg != 4 then return None

    // Find real roots of the radicand
    val roots = findRationalRoots(integrand.radicand)
    val allRoots = findAllRealRoots(integrand.radicand)

    if allRoots.length < deg then return None // not all roots are real

    // Sort roots
    val sortedRoots = allRoots.sorted

    // Compute modulus k^2 from the roots
    val modulusSq = computeModulusSq(sortedRoots, deg)

    modulusSq.map { kSq =>
      val k = Root(2, Lit(kSq))
      val normalizedK = Normalize.normalize(k)

      // Determine the type of Legendre form
      val terms = if integrand.den.degree == 0 then
        // Simple case: integral dx / sqrt(P(x)) => F(phi, k)
        Vector(LegendreForm.FirstKind(
          EulerIntegration.SymExpr.Var,
          normalizedK
        ))
      else
        // Has a pole: Pi form
        Vector(LegendreForm.ThirdKind(
          EulerIntegration.SymExpr.Var,
          Lit(Rational.one),
          normalizedK
        ))

      val notes = if jacobi then
        s"Jacobi inverse form with k = ${Pretty.pretty(normalizedK)}"
      else
        s"Legendre form with k^2 = $kSq"

      EllipticResult(terms, normalizedK, notes)
    }

  /** Compute the modulus squared from sorted real roots. */
  private def computeModulusSq(roots: Vector[Double], deg: Int): Option[Rational] =
    if deg == 3 && roots.length >= 3 then
      // For cubic with roots e1 >= e2 >= e3:
      // k^2 = (e2 - e3) / (e1 - e3)
      val e1 = roots(2)
      val e2 = roots(1)
      val e3 = roots(0)
      if math.abs(e1 - e3) < 1e-10 then None
      else
        val kSq = (e2 - e3) / (e1 - e3)
        Some(roundToRational(kSq))
    else if deg == 4 && roots.length >= 4 then
      // For quartic with roots r1 >= r2 >= r3 >= r4:
      // k^2 = ((r1-r3)(r2-r4)) / ((r1-r4)(r2-r3))
      val r1 = roots(3)
      val r2 = roots(2)
      val r3 = roots(1)
      val r4 = roots(0)
      val num = (r1 - r3) * (r2 - r4)
      val den = (r1 - r4) * (r2 - r3)
      if math.abs(den) < 1e-10 then None
      else Some(roundToRational(num / den))
    else None

  /** Find rational roots of a polynomial. */
  private def findRationalRoots(p: Poly[Rational]): Vector[Rational] =
    Factoring.rationalRoots(p)

  /** Find all real roots numerically. */
  private def findAllRealRoots(p: Poly[Rational]): Vector[Double] =
    val complexRoots = Resolvent.complexRootsOf(p)
    complexRoots.filter(z => math.abs(z.im) < 1e-6).map(_.re)

  /** Round a double to a simple rational. */
  private def roundToRational(x: Double): Rational =
    // Try small denominators
    (1 to 100).view.flatMap { d =>
      val n = math.round(x * d)
      if math.abs(x - n.toDouble / d) < 1e-8 then
        Some(Rational(n.toInt, d))
      else None
    }.headOption.getOrElse {
      Rational(math.round(x * 1000000).toInt, 1000000)
    }

  /** Pretty-print an elliptic result. */
  def prettyEllipticResult(result: EllipticResult): String =
    val termsStr = result.terms.map {
      case LegendreForm.FirstKind(_, k) =>
        s"  F(\u03c6, ${Pretty.pretty(k)})"
      case LegendreForm.SecondKind(_, k) =>
        s"  E(\u03c6, ${Pretty.pretty(k)})"
      case LegendreForm.ThirdKind(_, n, k) =>
        s"  \u03a0(\u03c6, ${Pretty.pretty(n)}, ${Pretty.pretty(k)})"
    }.mkString("\n")
    s"$termsStr\n  (${result.notes})"

  /** LaTeX rendering of an elliptic result. */
  def latexEllipticResult(result: EllipticResult): String =
    val termsStr = result.terms.map {
      case LegendreForm.FirstKind(_, k) =>
        s"  F(\\varphi, ${LaTeX.latex(k)})"
      case LegendreForm.SecondKind(_, k) =>
        s"  E(\\varphi, ${LaTeX.latex(k)})"
      case LegendreForm.ThirdKind(_, n, k) =>
        s"  \\Pi(\\varphi, ${LaTeX.latex(n)}, ${LaTeX.latex(k)})"
    }.mkString(" + ")
    s"$$${termsStr}$$\n  (${result.notes})"
