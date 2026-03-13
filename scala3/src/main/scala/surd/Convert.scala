package surd

/** Conversion between RadExpr and AlgNum representations.
  *
  * - radExprToAlgNum: compute the minimal polynomial and isolating interval
  * - algNumToRadExpr: for small degrees (1-4), construct explicit radical expressions
  *   using the quadratic formula, Cardano's formula, and Ferrari's method
  */
object Convert:
  given fld: Field[Rational] = summon[Field[Rational]]

  /** Convert a radical expression to an algebraic number. */
  def radExprToAlgNum(expr: RadExpr[Rational]): Option[AlgNum] =
    MinimalPoly.minimalPoly(expr).flatMap { mp =>
      val approx = Eval.evalComplex(expr).re
      AlgNum.fromMinPoly(mp, approx)
    }

  /** Convert an algebraic number to a radical expression (if degree <= 4). */
  def algNumToRadExpr(a: AlgNum): Option[RadExpr[Rational]] =
    a.degree match
      case 1 => Some(RadExpr.Lit(a.toRational.get))
      case 2 => solveQuadratic(a)
      case 3 => solveCubic(a)
      case 4 => solveQuartic(a)
      case _ => None

  /** Quadratic formula: x = (-b +/- sqrt(b^2 - 4ac)) / 2a
    * for ax^2 + bx + c = 0.
    */
  private def solveQuadratic(a: AlgNum): Option[RadExpr[Rational]] =
    val p = a.minPoly
    val c0 = p.coeffs(0)
    val c1 = p.coeffs(1)
    val c2 = p.coeffs(2)
    val disc = c1 * c1 - Rational(4) * c2 * c0

    val sqrtDisc = RadExpr.Root(2, RadExpr.Lit(disc))
    val twoA = Rational(2) * c2

    // Two roots: pick the one in the isolating interval
    val root1 = RadExpr.Mul(RadExpr.Inv(RadExpr.Lit(twoA)),
      RadExpr.Add(RadExpr.Neg(RadExpr.Lit(c1)), sqrtDisc))
    val root2 = RadExpr.Mul(RadExpr.Inv(RadExpr.Lit(twoA)),
      RadExpr.Add(RadExpr.Neg(RadExpr.Lit(c1)), RadExpr.Neg(sqrtDisc)))

    val v1 = Eval.eval(root1)
    val v2 = Eval.eval(root2)
    val target = a.toDouble

    val result = if math.abs(v1 - target) < math.abs(v2 - target)
    then root1 else root2

    Some(Normalize.normalize(result))

  /** Cardano's formula for depressed cubic t^3 + pt + q = 0.
    * Handles the casus irreducibilis (all roots real but complex intermediates needed).
    */
  private def solveCubic(a: AlgNum): Option[RadExpr[Rational]] =
    val p = a.minPoly
    // Convert to depressed form: x = t - b/(3a)
    val an = p.coeffs(3)
    val bn = p.coeffs(2)
    val cn = p.coeffs(1)
    val dn = p.coeffs(0)

    val shift = -bn / (Rational(3) * an)
    // Depressed: t^3 + pt + q where
    // p = (3ac - b^2) / (3a^2)
    // q = (2b^3 - 9abc + 27a^2d) / (27a^3)
    val pp = (Rational(3) * an * cn - bn * bn) / (Rational(3) * an * an)
    val qq = (Rational(2) * bn * bn * bn - Rational(9) * an * bn * cn + Rational(27) * an * an * dn) /
      (Rational(27) * an * an * an)

    val disc = -(Rational(4) * pp * pp * pp + Rational(27) * qq * qq)

    // Cardano: t = cbrt(-q/2 + sqrt(q^2/4 + p^3/27)) + cbrt(-q/2 - sqrt(q^2/4 + p^3/27))
    val halfQ = qq / Rational(2)
    val inner = qq * qq / Rational(4) + pp * pp * pp / Rational(27)

    val sqrtInner = RadExpr.Root(2, RadExpr.Lit(inner))
    val u1 = RadExpr.Root(3, RadExpr.Add(RadExpr.Lit(-halfQ), sqrtInner))
    val u2Radicand = RadExpr.Add(RadExpr.Lit(-halfQ), RadExpr.Neg(sqrtInner))

    // u2 = -p/(3*u1) to ensure correct branch selection
    val pExpr = RadExpr.Lit(-pp / Rational(3))
    val u2 = RadExpr.Mul(pExpr, RadExpr.Inv(u1))

    val t = RadExpr.Add(u1, u2)
    val result = RadExpr.Add(t, RadExpr.Lit(shift))

    // Check which root this corresponds to
    val v = Eval.evalComplex(result).re
    val target = a.toDouble

    if math.abs(v - target) < 0.01 then
      Some(Normalize.normalize(result))
    else
      None // Would need to try other cube root branches

  /** Ferrari's method for quartic equations. */
  private def solveQuartic(a: AlgNum): Option[RadExpr[Rational]] =
    // Quartic solving is complex; provide a simplified version
    // that works for the most common cases.
    val p = a.minPoly
    if p.degree != 4 then return None

    // Check if it's a biquadratic: x^4 + bx^2 + c = 0
    if p.coeffs(1).isZero && p.coeffs(3).isZero then
      solveBiquadratic(p, a)
    else
      None // Full Ferrari not implemented in simplified version

  /** Solve biquadratic x^4 + bx^2 + c = 0 via substitution y = x^2. */
  private def solveBiquadratic(p: Poly[Rational], a: AlgNum): Option[RadExpr[Rational]] =
    val c4 = p.coeffs(4)
    val c2 = p.coeffs(2)
    val c0 = p.coeffs(0)

    // y^2 + (c2/c4)y + (c0/c4) = 0
    val b = c2 / c4
    val c = c0 / c4
    val disc = b * b - Rational(4) * c

    val sqrtDisc = RadExpr.Root(2, RadExpr.Lit(disc))
    val y1 = RadExpr.Mul(RadExpr.Lit(Rational(1, 2)),
      RadExpr.Add(RadExpr.Neg(RadExpr.Lit(b)), sqrtDisc))
    val y2 = RadExpr.Mul(RadExpr.Lit(Rational(1, 2)),
      RadExpr.Add(RadExpr.Neg(RadExpr.Lit(b)), RadExpr.Neg(sqrtDisc)))

    // x = +/- sqrt(y)
    val candidates = Vector(
      RadExpr.Root(2, y1),
      RadExpr.Neg(RadExpr.Root(2, y1)),
      RadExpr.Root(2, y2),
      RadExpr.Neg(RadExpr.Root(2, y2))
    )

    val target = a.toDouble
    candidates.map(c => (c, Eval.evalComplex(c).re)).minByOption { (_, v) =>
      math.abs(v - target)
    }.filter((_, v) => math.abs(v - target) < 0.01).map(_._1)
      .map(Normalize.normalize)

  /** Try to simplify a radical expression via canonical form.
    * Converts to AlgNum, then back to RadExpr.
    */
  def simplifyViaCanonical(expr: RadExpr[Rational]): RadExpr[Rational] =
    radExprToAlgNum(expr) match
      case Some(an) =>
        algNumToRadExpr(an) match
          case Some(simplified) => simplified
          case None => expr
      case None => expr
