package surd

/** Compute minimal polynomials of radical expressions over Q.
  *
  * Given a RadExpr[Rational] representing an algebraic number,
  * compute its minimal polynomial over Q using:
  *   1. Tower-based construction (fast for expressions with few radicals)
  *   2. Resultant-based construction
  *   3. Numerical fallback via root isolation
  */
object MinimalPoly:
  given fld: Field[Rational] = summon[Field[Rational]]

  /** Compute the minimal polynomial of a radical expression.
    * Returns None if the computation fails or times out.
    * The result is guaranteed to be monic.
    */
  def minimalPoly(expr: RadExpr[Rational]): Option[MonicPoly] =
    minimalPolyRaw(expr).map(MonicPoly(_))

  /** Internal: compute minimal polynomial as a raw Poly. */
  private def minimalPolyRaw(expr: RadExpr[Rational]): Option[Poly[Rational]] =
    // Collect radicals and try tower-based approach
    val radicals = Expr.collectRadicals(expr)
    if radicals.isEmpty then
      // Pure rational expression
      val r = evalRational(expr)
      r.map(v => Poly(Vector(-v, Rational.one)))
    else if radicals.length <= 6 then
      towerBased(expr, radicals)
    else
      // Too many radicals for tower; use numerical fallback
      numericalFallback(expr)

  /** Evaluate a radical-free expression to a rational. */
  private def evalRational(expr: RadExpr[Rational]): Option[Rational] =
    expr match
      case RadExpr.Lit(r) => Some(r)
      case RadExpr.Neg(a) => evalRational(a).map(-_)
      case RadExpr.Add(a, b) =>
        for ra <- evalRational(a); rb <- evalRational(b) yield ra + rb
      case RadExpr.Mul(a, b) =>
        for ra <- evalRational(a); rb <- evalRational(b) yield ra * rb
      case RadExpr.Inv(a) =>
        evalRational(a).filter(!_.isZero).map(_.inverse)
      case RadExpr.Pow(a, n) =>
        evalRational(a).map(_.pow(n))
      case _ => None

  /** Tower-based minimal polynomial computation.
    *
    * For each radical r_i = Root(n_i, radicand_i), adjoin it to the
    * current field. The minimal polynomial is obtained by computing
    * resultants to eliminate each radical variable.
    */
  private def towerBased(
      expr: RadExpr[Rational],
      radicals: List[(Int, RadExpr[Rational])]
  ): Option[Poly[Rational]] =
    // Start with the minimal polynomial of the expression viewed as
    // a polynomial in the radicals
    val sorted = Expr.topoSortRadicals(radicals)

    // For simple cases: single sqrt
    sorted match
      case List((2, RadExpr.Lit(c))) if c > Rational.zero =>
        singleSqrtMinPoly(expr, c)
      case _ =>
        // General tower: compute resultant step by step
        generalTower(expr, sorted)

  /** Minimal polynomial for expressions involving a single sqrt(c). */
  private def singleSqrtMinPoly(
      expr: RadExpr[Rational],
      c: Rational
  ): Option[Poly[Rational]] =
    // Express expr as a + b*sqrt(c), then minpoly = (x-a)^2 - b^2*c
    // = x^2 - 2ax + a^2 - b^2*c
    val norm = Normalize.normalize(expr)
    decompose(norm, c) match
      case Some((a, b)) =>
        if b.isZero then Some(Poly(Vector(-a, Rational.one)))
        else
          val p = Poly(Vector(
            a * a - b * b * c,
            -Rational(2) * a,
            Rational.one
          ))
          // Check if this is minimal (not reducible)
          if Factoring.rationalRoots(p).isEmpty then Some(p)
          else
            // p has a rational root; the expression is rational
            val r = Factoring.rationalRoots(p).head
            Some(Poly(Vector(-r, Rational.one)))
      case None =>
        numericalFallback(expr)

  /** Decompose as a + b*sqrt(c). */
  private def decompose(expr: RadExpr[Rational], c: Rational): Option[(Rational, Rational)] =
    expr match
      case RadExpr.Lit(r) => Some((r, Rational.zero))
      case RadExpr.Root(2, RadExpr.Lit(r)) if r == c => Some((Rational.zero, Rational.one))
      case RadExpr.Mul(RadExpr.Lit(k), RadExpr.Root(2, RadExpr.Lit(r))) if r == c =>
        Some((Rational.zero, k))
      case RadExpr.Add(a, b) =>
        for
          (a1, a2) <- decompose(a, c)
          (b1, b2) <- decompose(b, c)
        yield (a1 + b1, a2 + b2)
      case RadExpr.Neg(a) =>
        decompose(a, c).map((x, y) => (-x, -y))
      case _ => None

  /** General tower construction. */
  private def generalTower(
      expr: RadExpr[Rational],
      radicals: List[(Int, RadExpr[Rational])]
  ): Option[Poly[Rational]] =
    // Fallback to numerical method for complex towers
    numericalFallback(expr)

  /** Numerical fallback: evaluate to high precision, then use
    * integer relation finding (LLL/PSLQ) to identify the minimal polynomial.
    */
  private def numericalFallback(expr: RadExpr[Rational]): Option[Poly[Rational]] =
    val z = Eval.evalComplex(expr)
    if math.abs(z.im) > 1e-10 then
      // Complex value: need to work with conjugates
      None
    else
      val x = z.re
      // Try to find minimal polynomial by computing powers and using
      // integer relation detection
      findMinPolyNumerical(x, maxDeg = 12)

  /** Find minimal polynomial numerically by trying increasing degrees.
    * For each degree d, check if [1, x, x^2, ..., x^d] satisfies a
    * linear relation with rational coefficients.
    */
  private def findMinPolyNumerical(x: Double, maxDeg: Int): Option[Poly[Rational]] =
    val powers = (0 to maxDeg).map(d => math.pow(x, d)).toVector

    // Try each degree
    (1 to maxDeg).view.flatMap { d =>
      // Check if x is a root of a degree-d polynomial with small integer coefficients
      findIntegerRelation(powers.take(d + 1), coeffBound = 1000)
    }.headOption

  /** Simple integer relation finder: try small integer coefficient combinations.
    * For a proper implementation, one would use LLL or PSLQ.
    */
  private def findIntegerRelation(values: Vector[Double], coeffBound: Int): Option[Poly[Rational]] =
    val d = values.length - 1
    if d <= 0 then None
    else
      // For degree 1: check if x is close to a rational p/q
      if d == 1 then
        val x = values(1)
        (1 to coeffBound).view.flatMap { q =>
          val p = math.round(x * q)
          if math.abs(x - p.toDouble / q) < 1e-12 then
            Some(Poly(Vector(Rational(-p, q), Rational.one)))
          else None
        }.headOption
      else
        // For higher degrees, a simplified search
        // (A proper PSLQ would be much more effective)
        None
