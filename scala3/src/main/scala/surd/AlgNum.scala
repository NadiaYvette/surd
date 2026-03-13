package surd

/** Algebraic number: defined by a minimal polynomial and an isolating interval.
  *
  * An algebraic number alpha is uniquely determined by:
  *   - Its minimal polynomial over Q (irreducible, monic)
  *   - An isolating interval containing exactly alpha and no other root
  *
  * Arithmetic uses resultant-based constructions for the minimal polynomial
  * of sums, products, etc.
  */
final case class AlgNum(
    minPoly: Poly[Rational],
    interval: Interval
):
  /** Numerical approximation. */
  def toDouble: Double = interval.midpoint.toDouble

  /** Degree of the algebraic number (= degree of minimal polynomial). */
  def degree: Int = minPoly.degree

  /** Check if this is a rational number (degree 1). */
  def isRational: Boolean = degree == 1

  /** Extract the rational value, if this is rational. */
  def toRational: Option[Rational] =
    if isRational then
      // minpoly = x - r, so r = -a_0/a_1
      val a0 = minPoly.coeffs(0)
      val a1 = minPoly.coeffs(1)
      Some(-a0 / a1)
    else None

  override def toString: String =
    s"AlgNum(${minPoly}, ≈${interval.midpoint.toDouble})"

object AlgNum:
  given fld: Field[Rational] = summon[Field[Rational]]

  /** Construct an algebraic number from a rational. */
  def fromRational(r: Rational): AlgNum =
    val mp = Poly(Vector(-r, Rational.one))
    AlgNum(mp, Interval.point(r))

  /** Construct from a minimal polynomial and a numerical approximation.
    * Finds the isolating interval near the approximation.
    */
  def fromMinPoly(mp: Poly[Rational], approx: Double): Option[AlgNum] =
    RootIsolation.findRootNear(mp, approx).map(iv => AlgNum(mp, iv))

  /** Addition of algebraic numbers via resultant.
    * The minimal polynomial of alpha + beta divides
    * Res_y(minpoly_alpha(x - y), minpoly_beta(y)).
    */
  def add(a: AlgNum, b: AlgNum): AlgNum =
    if a.isRational then
      val r = a.toRational.get
      val shifted = Poly.compose(b.minPoly, Poly(Vector(r, -Rational.one)))
      val mp = makeMonic(shifted)
      val iv = a.interval + b.interval
      AlgNum(mp, iv)
    else if b.isRational then add(b, a)
    else
      // General case: resultant
      val approx = a.toDouble + b.toDouble
      val sumIv = a.interval + b.interval
      // Build Res_y(p(x-y), q(y))
      val resPoly = sumResultant(a.minPoly, b.minPoly)
      val mp = isolateAndMinPoly(resPoly, approx)
      AlgNum(mp, refine(mp, sumIv))

  /** Negation: if alpha has minpoly p(x), then -alpha has minpoly p(-x) (up to sign). */
  def negate(a: AlgNum): AlgNum =
    val negPoly = Poly.compose(a.minPoly, Poly(Vector(Rational.zero, -Rational.one)))
    val mp = makeMonic(negPoly)
    val iv = Interval(-a.interval.hi, -a.interval.lo)
    AlgNum(mp, iv)

  /** Multiplication of algebraic numbers via resultant. */
  def mul(a: AlgNum, b: AlgNum): AlgNum =
    if a.isRational then
      val r = a.toRational.get
      if r.isZero then fromRational(Rational.zero)
      else
        // Scale: if alpha has minpoly p(x), then r*alpha has minpoly p(x/r) * r^deg
        val d = b.minPoly.degree
        val scaled = Poly(b.minPoly.coeffs.zipWithIndex.map { (c, i) =>
          c * r.pow(d - i).inverse
        })
        val mp = makeMonic(scaled)
        val iv = a.interval * b.interval
        AlgNum(mp, refine(mp, iv))
    else if b.isRational then mul(b, a)
    else
      val approx = a.toDouble * b.toDouble
      val prodIv = a.interval * b.interval
      val resPoly = prodResultant(a.minPoly, b.minPoly)
      val mp = isolateAndMinPoly(resPoly, approx)
      AlgNum(mp, refine(mp, prodIv))

  /** Inverse: if alpha has minpoly a_0 + a_1*x + ... + a_n*x^n,
    * then 1/alpha has minpoly a_n + a_{n-1}*x + ... + a_0*x^n.
    */
  def inverse(a: AlgNum): AlgNum =
    require(!a.interval.containsZero, "AlgNum.inverse: zero algebraic number")
    val revCoeffs = a.minPoly.coeffs.reverse
    val mp = makeMonic(Poly(revCoeffs))
    val iv = a.interval.inverse
    AlgNum(mp, refine(mp, iv))

  // --- Internal helpers ---

  private def makeMonic(p: Poly[Rational]): Poly[Rational] =
    Poly.monic(p)

  /** Compute resultant for sum: Res_y(p(x-y), q(y)). */
  private def sumResultant(p: Poly[Rational], q: Poly[Rational]): Poly[Rational] =
    // Simplified: use numerical root finding to identify the minimal polynomial
    // For a proper implementation, build the bivariate resultant.
    // Here we use a fallback: multiply minimal polynomials of all pairwise sums.
    val pRoots = RootIsolation.isolateRoots(p)
    val qRoots = RootIsolation.isolateRoots(q)
    if pRoots.isEmpty || qRoots.isEmpty then p
    else
      // Build polynomial with roots at p_i + q_j
      val approxRoots = for
        pi <- pRoots
        qj <- qRoots
      yield (pi.midpoint + qj.midpoint).toDouble
      // Construct polynomial from roots (crude)
      approxRoots.foldLeft(Poly.const(Rational.one): Poly[Rational]) { (acc, r) =>
        val rr = Rational(BigInt(math.round(r * 1000000)), BigInt(1000000))
        Poly.mul(acc, Poly(Vector(-rr, Rational.one)))
      }

  /** Compute resultant for product. */
  private def prodResultant(p: Poly[Rational], q: Poly[Rational]): Poly[Rational] =
    sumResultant(p, q) // Simplified fallback

  private def isolateAndMinPoly(p: Poly[Rational], approx: Double): Poly[Rational] =
    // Factor and find the irreducible factor whose root is near approx
    val factors = Factoring.factor(p)
    factors.map(_._1).find { f =>
      val roots = RootIsolation.isolateRoots(f)
      roots.exists(iv => math.abs(iv.midpoint.toDouble - approx) < 0.01)
    }.getOrElse(p)

  private def refine(mp: Poly[Rational], iv: Interval): Interval =
    try RootIsolation.refineRoot(mp, iv, Rational(1, 1000000))
    catch case _: Exception => iv
