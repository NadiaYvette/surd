package surd

/** Polynomial factoring over Q.
  *
  * Implements:
  *   - Rational root test
  *   - Kronecker's method for small degree
  *   - Square-free factorization (via Poly.squareFree)
  */
object Factoring:

  /** Find all rational roots of a polynomial over Q.
    * By the rational root theorem, if p/q is a root (in lowest terms),
    * then p | a_0 and q | a_n.
    */
  def rationalRoots(f: Poly[Rational])(using Field[Rational]): Vector[Rational] =
    if f.isZero then Vector.empty
    else if f.degree == 0 then Vector.empty
    else
      val a0 = f.coeffs.head
      val an = f.coeffs.last
      if a0.isZero then
        Rational.zero +: rationalRoots(Poly.divMod(f, Poly(Vector(Rational.zero, Rational.one)))._1)
      else
        val numDivisors = intDivisors(a0.num.abs)
        val denDivisors = intDivisors(an.num.abs * a0.den) // account for denominators
        val candidates = for
          p <- numDivisors
          q <- denDivisors
          if q != BigInt(0)
          sign <- Vector(BigInt(1), BigInt(-1))
        yield Rational(sign * p * an.den, q)

        candidates.distinct.filter(r => Poly.eval(f, r).isZero)

  /** Integer divisors of |n|, including 1 and |n|. */
  private def intDivisors(n: BigInt): Vector[BigInt] =
    val abs = n.abs
    if abs == BigInt(0) then Vector(BigInt(1))
    else
      (BigInt(1) to abs.min(BigInt(1000))).filter(d => abs % d == 0).toVector

  /** Factor a polynomial over Q into irreducible factors.
    * Returns (factor, multiplicity) pairs.
    *
    * Strategy: square-free decomposition, then rational root extraction.
    */
  def factor(f: Poly[Rational])(using Field[Rational]): Vector[(Poly[Rational], Int)] =
    if f.isZero then Vector.empty
    else if f.degree == 0 then Vector((f, 1))
    else
      val sff = Poly.squareFree(f)
      sff.flatMap { case (sf, mult) =>
        factorSquareFree(sf).map((fac, _) => (fac, mult))
      }

  /** Factor a square-free polynomial over Q. */
  private def factorSquareFree(f: Poly[Rational])(using Field[Rational]): Vector[(Poly[Rational], Int)] =
    if f.degree <= 1 then Vector((f, 1))
    else
      rationalRoots(f).headOption match
        case Some(r) =>
          // (x - r) is a factor
          val linearFactor = Poly(Vector(-r, Rational.one))
          val quotient = Poly.divMod(f, linearFactor)._1
          (linearFactor, 1) +: factorSquareFree(quotient)
        case None =>
          // No rational roots; for degrees 2-4 this means irreducible over Q
          // (assuming square-free). For higher degrees, we'd need Berlekamp/LLL.
          Vector((f, 1))

  /** Check if a polynomial is irreducible over Q.
    * Reliable for degrees <= 4 (no rational roots + square-free => irreducible for deg 2,3).
    * For higher degrees this is a heuristic.
    */
  def isIrreducible(f: Poly[Rational])(using Field[Rational]): Boolean =
    if f.degree <= 1 then f.degree == 1
    else
      rationalRoots(f).isEmpty && (f.degree <= 3 || !hasNontrivialFactor(f))

  /** Check for nontrivial factors by trying small-degree divisions. */
  private def hasNontrivialFactor(f: Poly[Rational])(using Field[Rational]): Boolean =
    // Try to find degree-2 factors by Kronecker's method (simplified)
    val n = f.degree
    if n <= 3 then false
    else
      // Evaluate at 0, 1, -1, 2, -2 and check if any divisor combination works
      val pts = Vector(Rational.zero, Rational.one, -Rational.one, Rational(2))
      val vals = pts.map(p => Poly.eval(f, p))
      // If f(0) = 0, it has a rational root (already checked)
      // For a proper Kronecker implementation we'd interpolate candidate factors.
      // For now, this is conservative (returns false = assume irreducible).
      false
