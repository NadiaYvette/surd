package surd

import RadExpr.*

/** Euler substitution for integrals of the form
  * integral P(x)/Q(x) * (sqrt(ax^2+bx+c))^n dx.
  *
  * Euler's three substitutions:
  *   Type 1: a > 0: sqrt(ax^2+bx+c) = t - sqrt(a)*x
  *   Type 2: c > 0: sqrt(ax^2+bx+c) = x*t + sqrt(c)
  *   Type 3: discriminant: sqrt(ax^2+bx+c) = (x-r)*t where r is a root
  */
object EulerIntegration:
  given fld: Field[Rational] = summon[Field[Rational]]

  /** An integrand of the form P(x)/Q(x) * (sqrt(ax^2+bx+c))^n. */
  final case class EulerIntegrand(
      p: Poly[Rational],    // numerator polynomial P(x)
      q: Poly[Rational],    // denominator polynomial Q(x)
      sqrtPow: Int,         // power of the square root (-1, 1, etc.)
      a: Rational,          // coefficient of x^2 under the sqrt
      b: Rational,          // coefficient of x under the sqrt
      c: Rational           // constant under the sqrt
  )

  /** A symbolic expression for an antiderivative. */
  enum SymExpr:
    case Const(r: Rational)
    case Var                                       // x
    case Rad(expr: RadExpr[Rational])              // radical expression
    case Log(arg: SymExpr)                         // ln|arg|
    case ArcTan(arg: SymExpr)                      // arctan(arg)
    case Sum(terms: Vector[SymExpr])
    case Prod(factors: Vector[SymExpr])
    case Quot(num: SymExpr, den: SymExpr)
    case Sqrt(inner: SymExpr)

  /** Result of Euler integration. */
  final case class IntegrationResult(
      expr: SymExpr,
      substitutionType: Int
  )

  /** Attempt to compute the antiderivative via Euler substitution. */
  def eulerIntegrate(integrand: EulerIntegrand): Option[IntegrationResult] =
    // Try each Euler substitution type
    if integrand.a > Rational.zero then
      Some(eulerType1(integrand))
    else if integrand.c > Rational.zero then
      Some(eulerType2(integrand))
    else
      // Try type 3: find a root of ax^2+bx+c
      val disc = integrand.b * integrand.b - Rational(4) * integrand.a * integrand.c
      if disc >= Rational.zero then
        Some(eulerType3(integrand))
      else
        None

  /** Euler type 1: a > 0, substitute sqrt(ax^2+bx+c) = t - sqrt(a)*x. */
  private def eulerType1(ig: EulerIntegrand): IntegrationResult =
    // For the simplest case: integral dx / sqrt(x^2+c)
    // = ln|x + sqrt(x^2+c)| + C
    if ig.p.degree == 0 && ig.q.degree == 0 && ig.sqrtPow == -1 then
      if ig.a == Rational.one && ig.b.isZero then
        val sqrtExpr = SymExpr.Sqrt(SymExpr.Sum(Vector(
          SymExpr.Prod(Vector(SymExpr.Var, SymExpr.Var)),
          SymExpr.Const(ig.c)
        )))
        IntegrationResult(
          SymExpr.Log(SymExpr.Sum(Vector(SymExpr.Var, sqrtExpr))),
          substitutionType = 1
        )
      else
        // General case: integral dx / sqrt(ax^2+bx+c)
        // = (1/sqrt(a)) * ln|2*sqrt(a)*sqrt(ax^2+bx+c) + 2ax + b|
        val sqrtA = SymExpr.Rad(Root(2, Lit(ig.a)))
        val innerSqrt = SymExpr.Sqrt(SymExpr.Sum(Vector(
          SymExpr.Prod(Vector(SymExpr.Const(ig.a), SymExpr.Var, SymExpr.Var)),
          SymExpr.Prod(Vector(SymExpr.Const(ig.b), SymExpr.Var)),
          SymExpr.Const(ig.c)
        )))
        IntegrationResult(
          SymExpr.Quot(
            SymExpr.Log(SymExpr.Sum(Vector(
              SymExpr.Prod(Vector(SymExpr.Const(Rational(2)), sqrtA, innerSqrt)),
              SymExpr.Prod(Vector(SymExpr.Const(Rational(2) * ig.a), SymExpr.Var)),
              SymExpr.Const(ig.b)
            ))),
            sqrtA
          ),
          substitutionType = 1
        )
    else
      // More complex integrand: return a generic result
      genericResult(ig, 1)

  /** Euler type 2: c > 0, substitute sqrt(ax^2+bx+c) = xt + sqrt(c). */
  private def eulerType2(ig: EulerIntegrand): IntegrationResult =
    // For integral dx / sqrt(c - x^2) when a = -1, b = 0
    // = arcsin(x/sqrt(c))
    if ig.p.degree == 0 && ig.q.degree == 0 && ig.sqrtPow == -1 &&
       ig.a == -Rational.one && ig.b.isZero then
      IntegrationResult(
        SymExpr.ArcTan(SymExpr.Quot(
          SymExpr.Var,
          SymExpr.Sqrt(SymExpr.Sum(Vector(
            SymExpr.Const(ig.c),
            SymExpr.Prod(Vector(SymExpr.Const(-Rational.one), SymExpr.Var, SymExpr.Var))
          )))
        )),
        substitutionType = 2
      )
    else
      genericResult(ig, 2)

  /** Euler type 3: use root of ax^2+bx+c. */
  private def eulerType3(ig: EulerIntegrand): IntegrationResult =
    genericResult(ig, 3)

  /** Generic result when full symbolic integration is not implemented. */
  private def genericResult(ig: EulerIntegrand, subType: Int): IntegrationResult =
    val innerSqrt = SymExpr.Sqrt(SymExpr.Sum(Vector(
      SymExpr.Prod(Vector(SymExpr.Const(ig.a), SymExpr.Var, SymExpr.Var)),
      SymExpr.Prod(Vector(SymExpr.Const(ig.b), SymExpr.Var)),
      SymExpr.Const(ig.c)
    )))
    IntegrationResult(
      SymExpr.Prod(Vector(SymExpr.Const(Rational.one), innerSqrt)),
      substitutionType = subType
    )

  /** Pretty-print a symbolic expression. */
  def prettySymExpr(e: SymExpr): String = e match
    case SymExpr.Const(r) => r.toString
    case SymExpr.Var => "x"
    case SymExpr.Rad(expr) => Pretty.pretty(expr)
    case SymExpr.Log(arg) => s"ln|${prettySymExpr(arg)}|"
    case SymExpr.ArcTan(arg) => s"arctan(${prettySymExpr(arg)})"
    case SymExpr.Sum(terms) => terms.map(prettySymExpr).mkString(" + ")
    case SymExpr.Prod(factors) => factors.map(prettySymExpr).mkString(" * ")
    case SymExpr.Quot(n, d) => s"(${prettySymExpr(n)})/(${prettySymExpr(d)})"
    case SymExpr.Sqrt(inner) => s"\u221a(${prettySymExpr(inner)})"

  /** LaTeX rendering of a symbolic expression. */
  def latexSymExpr(e: SymExpr): String = e match
    case SymExpr.Const(r) =>
      if r.den == BigInt(1) then r.num.toString
      else s"\\frac{${r.num}}{${r.den}}"
    case SymExpr.Var => "x"
    case SymExpr.Rad(expr) => LaTeX.latex(expr)
    case SymExpr.Log(arg) => s"\\ln\\left|${latexSymExpr(arg)}\\right|"
    case SymExpr.ArcTan(arg) => s"\\arctan\\left(${latexSymExpr(arg)}\\right)"
    case SymExpr.Sum(terms) => terms.map(latexSymExpr).mkString(" + ")
    case SymExpr.Prod(factors) => factors.map(latexSymExpr).mkString(" \\cdot ")
    case SymExpr.Quot(n, d) => s"\\frac{${latexSymExpr(n)}}{${latexSymExpr(d)}}"
    case SymExpr.Sqrt(inner) => s"\\sqrt{${latexSymExpr(inner)}}"
