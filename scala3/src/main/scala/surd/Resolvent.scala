package surd

/** Resolvent polynomials for Galois group computation.
  *
  * Computes resolvents numerically: roots of f are approximated in C
  * via the Aberth-Ehrlich method, invariants are evaluated on permuted
  * roots, and the resulting polynomial is reconstructed with rational
  * coefficients.
  */
object Resolvent:
  import Eval.Complex
  given fld: Field[Rational] = summon[Field[Rational]]

  /** Approximate all complex roots of a polynomial over Q
    * using the Aberth-Ehrlich simultaneous iteration method.
    */
  def complexRootsOf(f: Poly[Rational]): Vector[Complex] =
    if f.isZero || f.degree <= 0 then Vector.empty
    else if f.degree == 1 then
      val r = -f.coeffs(0) / f.coeffs(1)
      Vector(Complex.fromDouble(r.toDouble))
    else if f.degree == 2 then
      quadraticRoots(f)
    else
      aberthEhrlich(f, maxIter = 100)

  /** Quadratic formula. */
  private def quadraticRoots(f: Poly[Rational]): Vector[Complex] =
    val a = f.coeffs(2).toDouble
    val b = f.coeffs(1).toDouble
    val c = f.coeffs(0).toDouble
    val disc = b * b - 4 * a * c
    if disc >= 0 then
      val sd = math.sqrt(disc)
      Vector(
        Complex((-b + sd) / (2 * a), 0),
        Complex((-b - sd) / (2 * a), 0)
      )
    else
      val sd = math.sqrt(-disc)
      Vector(
        Complex(-b / (2 * a), sd / (2 * a)),
        Complex(-b / (2 * a), -sd / (2 * a))
      )

  /** Aberth-Ehrlich simultaneous iteration. */
  private def aberthEhrlich(f: Poly[Rational], maxIter: Int): Vector[Complex] =
    val n = f.degree
    // Initial guesses: points on a circle
    val bound = RootBound.cauchyBound(f).toDouble
    val z = Array.tabulate(n) { k =>
      val angle = 2 * math.Pi * k / n + 0.1
      Complex(bound * 0.9 * math.cos(angle), bound * 0.9 * math.sin(angle))
    }

    for _ <- 0 until maxIter do
      var converged = true
      for i <- 0 until n do
        val fz = evalPoly(f, z(i))
        val fpz = evalPolyDeriv(f, z(i))
        if fz.magnitude > 1e-14 then
          converged = false
          val ratio = fz / fpz
          val sum = (0 until n).filter(_ != i).foldLeft(Complex.zero) { (acc, j) =>
            acc + (z(i) - z(j)).inverse
          }
          val denom = Complex.one - ratio * sum
          z(i) = z(i) - ratio / denom
      if converged then return z.toVector

    z.toVector

  /** Evaluate polynomial at a complex point. */
  private def evalPoly(f: Poly[Rational], z: Complex): Complex =
    f.coeffs.foldRight(Complex.zero) { (c, acc) =>
      Complex.fromDouble(c.toDouble) + z * acc
    }

  /** Evaluate derivative at a complex point. */
  private def evalPolyDeriv(f: Poly[Rational], z: Complex): Complex =
    val fp = Poly.diff(f)
    evalPoly(fp, z)

  /** Compute the discriminant of a polynomial. */
  def discriminantOf(f: Poly[Rational]): Rational =
    if f.degree <= 1 then Rational.one
    else
      val roots = complexRootsOf(f)
      val n = roots.length
      var disc = Complex.one
      for i <- 0 until n; j <- i + 1 until n do
        disc = disc * (roots(i) - roots(j))
      val discSq = disc * disc
      // Round to rational
      val lc = f.coeffs.last
      val sign = if n % 4 == 2 || n % 4 == 3 then -Rational.one else Rational.one
      val scaled = discSq.re * math.pow(lc.toDouble, 2 * n - 2)
      roundToRational(scaled, denomBound = 10000) * sign

  /** Check if a rational is a perfect square. */
  def isSquareRational(r: Rational): Boolean =
    if r < Rational.zero then false
    else if r.isZero then true
    else
      val numSq = isSquareBigInt(r.num)
      val denSq = isSquareBigInt(r.den)
      numSq && denSq

  private def isSquareBigInt(n: BigInt): Boolean =
    if n < 0 then false
    else
      val s = BigInt(math.sqrt(n.toDouble).toLong)
      Vector(s - 1, s, s + 1).exists(k => k >= 0 && k * k == n)

  /** Check if a polynomial has a rational root. */
  def hasRationalRoot(f: Poly[Rational]): Boolean =
    Factoring.rationalRoots(f).nonEmpty

  /** Compute the sextic resolvent for degree-5 Galois group identification.
    * The F20-invariant theta = sum_{i=0}^4 x_i^2 * (x_{i+1}*x_{i+4} + x_{i+2}*x_{i+3})
    * (indices mod 5).
    */
  def sexticResolvent(f: Poly[Rational]): Poly[Rational] =
    val roots = complexRootsOf(f)
    if roots.length != 5 then return Poly.zero[Rational]

    // Generate all 6 coset representatives of Stab(theta) in S5
    // Stab(theta) = F20, |S5/F20| = 6
    val cosets = sixCosetReps5

    val thetaValues = cosets.map { sigma =>
      val permRoots = sigma.images.map(roots(_))
      f20Invariant(permRoots)
    }

    // Build resolvent polynomial from roots
    reconstructPolynomial(thetaValues)

  /** The F20-invariant evaluated on permuted roots. */
  private def f20Invariant(x: Vector[Complex]): Complex =
    (0 until 5).foldLeft(Complex.zero) { (acc, i) =>
      val xi = x(i)
      val prod1 = x((i + 1) % 5) * x((i + 4) % 5)
      val prod2 = x((i + 2) % 5) * x((i + 3) % 5)
      acc + xi * xi * (prod1 + prod2)
    }

  /** Six coset representatives of F20 in S5. */
  private def sixCosetReps5: Vector[Perm] =
    Vector(
      Perm.identity(5),
      Perm.transposition(5, 0, 1),
      Perm.transposition(5, 0, 2),
      Perm.transposition(5, 0, 3),
      Perm.transposition(5, 0, 4),
      Perm.cycle(5, 0, 1, 2)
    )

  /** Reconstruct a polynomial from its complex roots.
    * Coefficients are rounded to rationals.
    */
  private def reconstructPolynomial(roots: Vector[Complex]): Poly[Rational] =
    var result: Vector[Complex] = Vector(Complex.one)
    for r <- roots do
      // Multiply by (x - r)
      val newResult = Array.fill(result.length + 1)(Complex.zero)
      for i <- result.indices do
        newResult(i + 1) = newResult(i + 1) + result(i)
        newResult(i) = newResult(i) - result(i) * r
      result = newResult.toVector

    // Round to rationals
    val coeffs = result.map(z => roundToRational(z.re, denomBound = 1000000))
    Poly(coeffs)

  /** Round a double to a rational with bounded denominator. */
  private def roundToRational(x: Double, denomBound: Long): Rational =
    if x.isNaN || x.isInfinite then Rational.zero
    else
      // Try small denominators first
      (1L to math.min(denomBound, 10000L)).view.flatMap { d =>
        val n = math.round(x * d)
        if math.abs(x - n.toDouble / d) < 1e-8 then
          Some(Rational(BigInt(n), BigInt(d)))
        else None
      }.headOption.getOrElse {
        val n = math.round(x * denomBound)
        Rational(BigInt(n), BigInt(denomBound))
      }
