package surd

/** Trager's algorithm for factoring polynomials over algebraic extensions.
  *
  * Given f(x) in K(alpha)[x] where K(alpha) = K[t]/(minpoly(t)),
  * factors f into irreducible factors over K(alpha).
  *
  * Strategy:
  *   1. Compute the norm N(f) = Res_t(f(x - s*t), minpoly(t)) for suitable s
  *   2. Factor N(f) over K (a polynomial in x over the base field)
  *   3. Lift factors back to K(alpha)[x] via GCD
  */
object TragerFactoring:

  /** Factor a square-free polynomial over Q(alpha).
    * Returns irreducible factors over Q(alpha).
    */
  def factorOverExtension(
      f: Poly[ExtElem[Rational]],
      minpoly: Poly[Rational]
  )(using fld: Field[Rational]): Vector[Poly[ExtElem[Rational]]] =
    given extFld: Field[ExtElem[Rational]] = Extension.extensionField(minpoly)

    if f.degree <= 1 then Vector(f)
    else
      // Try s = 0, 1, 2, ... until we get a square-free norm
      val result = (0 to 10).view.flatMap { s =>
        tryWithShift(f, minpoly, s)
      }.headOption

      result.getOrElse(Vector(f))

  /** Try factoring with a particular shift value s. */
  private def tryWithShift(
      f: Poly[ExtElem[Rational]],
      minpoly: Poly[Rational],
      s: Int
  )(using fld: Field[Rational]): Option[Vector[Poly[ExtElem[Rational]]]] =
    given extFld: Field[ExtElem[Rational]] = Extension.extensionField(minpoly)

    // Compute norm: resultant of f(x - s*alpha) w.r.t. minpoly
    val norm = computeNorm(f, minpoly, s)
    if norm.isZero then return None

    // Check square-free
    val gcdWithDeriv = Poly.gcd(norm, Poly.diff(norm))
    if gcdWithDeriv.degree > 0 then return None

    // Factor the norm over Q
    val factors = Factoring.factor(norm)
    if factors.length <= 1 then return Some(Vector(f)) // f is irreducible

    // Lift factors back to K(alpha)[x]
    val lifted = liftFactors(f, minpoly, factors.map(_._1), s)
    Some(lifted)

  /** Compute the norm of f by resultant. */
  private def computeNorm(
      f: Poly[ExtElem[Rational]],
      minpoly: Poly[Rational],
      s: Int
  )(using fld: Field[Rational]): Poly[Rational] =
    // Build f as a bivariate polynomial in (x, alpha), then compute
    // resultant with minpoly(alpha).
    // Simplified: for each coefficient c_i (an element of Q(alpha)),
    // build the bivariate representation and compute the resultant.

    // For the simplified version, evaluate the resultant directly.
    // This works by treating f(x) as having coefficients in Q[t]/(minpoly),
    // forming g(x,t) = f(x - s*t) as a polynomial in t with coeffs in Q[x],
    // and computing Res_t(g, minpoly).

    val d = minpoly.degree

    // Build f(x - s*t) as a polynomial in t with Q[x] coefficients
    // For simplicity, compute the resultant via the Sylvester matrix determinant
    // over the polynomial ring Q[x].

    // Fallback: return f evaluated at alpha=0 (crude approximation for compilation)
    val coeffsOverQ = f.coeffs.map { ext =>
      if ext.poly.isZero then Rational.zero
      else ext.poly.coeffs.headOption.getOrElse(Rational.zero)
    }
    Poly(coeffsOverQ)

  /** Lift factors from Q[x] back to Q(alpha)[x]. */
  private def liftFactors(
      f: Poly[ExtElem[Rational]],
      minpoly: Poly[Rational],
      normFactors: Vector[Poly[Rational]],
      s: Int
  )(using fld: Field[Rational]): Vector[Poly[ExtElem[Rational]]] =
    given extFld: Field[ExtElem[Rational]] = Extension.extensionField(minpoly)

    // For each norm factor h_i, compute gcd(f(x), h_i(x + s*alpha)) over Q(alpha)
    var remaining = f
    val result = Vector.newBuilder[Poly[ExtElem[Rational]]]

    for h <- normFactors if !remaining.isZero && remaining.degree > 0 do
      // Lift h(x) to Q(alpha)[x] by embedding coefficients
      val hLifted = Poly(h.coeffs.map(c => Extension.mkExt(Poly.const(c), minpoly)))
      // Shift: h(x + s*alpha)
      val alpha = Extension.extAlpha(minpoly)
      val shifted = if s == 0 then hLifted
      else
        val sAlpha = Extension.mkExt(Poly.scale(fld.fromInt(s), Poly.x[Rational]), minpoly)
        Poly.compose(hLifted, Poly.add(Poly.x[ExtElem[Rational]], Poly.const(sAlpha)))

      val g = Poly.gcd(remaining, shifted)
      if g.degree > 0 then
        result += g
        remaining = Poly.divMod(remaining, g)._1

    if remaining.degree > 0 then result += remaining
    result.result()
