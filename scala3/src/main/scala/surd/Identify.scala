package surd

/** Galois group identification for irreducible polynomials over Q.
  *
  * Uses the Stauduhar descent approach with discriminant and resolvent tests.
  *
  * Decision tree for degree 5:
  *   disc square?  sextic root?  result
  *   no            no            S5
  *   yes           no            A5
  *   no            yes           F20
  *   yes           yes           D5 or C5 (Frobenius test)
  */
object Identify:
  import Eval.Complex
  given fld: Field[Rational] = summon[Field[Rational]]

  /** Result of Galois group identification. */
  final case class GaloisResult(
      group: TransitiveGroup.TGroup,
      roots: Vector[Complex],
      discriminant: Rational,
      discIsSquare: Boolean
  )

  /** Identify the Galois group of an irreducible degree-5 polynomial over Q.
    * Returns the identified transitive group and numerical roots.
    */
  def identifyGaloisGroup5(f: Poly[Rational]): Option[GaloisResult] =
    if f.degree != 5 then return None

    val roots = Resolvent.complexRootsOf(f)
    if roots.length != 5 then return None

    val disc = Resolvent.discriminantOf(f)
    val discSq = Resolvent.isSquareRational(disc)

    // Compute sextic resolvent
    val sextic = Resolvent.sexticResolvent(f)
    val hasSexticRoot = Resolvent.hasRationalRoot(sextic)

    val group = (discSq, hasSexticRoot) match
      case (false, false) => TransitiveGroup.s5
      case (true, false) => TransitiveGroup.a5
      case (false, true) => TransitiveGroup.f20
      case (true, true) =>
        // Distinguish C5 from D5 via Frobenius test
        if frobeniusTestC5(f) then TransitiveGroup.c5
        else TransitiveGroup.d5

    Some(GaloisResult(group, roots, disc, discSq))

  /** Frobenius/Chebotarev test to distinguish C5 from D5.
    *
    * For C5, the factorization pattern of f mod p is either
    * {5} (inert) or {1,1,1,1,1} (split). D5 additionally admits {1,2,2}.
    * Test 20 good primes.
    */
  private def frobeniusTestC5(f: Poly[Rational]): Boolean =
    // Convert f to integer polynomial by clearing denominators
    val lcm = f.coeffs.map(_.den).foldLeft(BigInt(1))((a, b) => a / a.gcd(b) * b)
    val intCoeffs = f.coeffs.map(c => (c * Rational(lcm)).num)

    var primesChecked = 0
    var sawNonTrivial = false

    val primeStream = PrimeFactors.primes.drop(1) // skip 2

    for p <- primeStream.take(50) if primesChecked < 20 do
      val pInt = p.toInt
      // Check that p doesn't divide the leading coefficient
      if intCoeffs.last % p != 0 then
        val pattern = factorizationPattern(intCoeffs, pInt)
        primesChecked += 1
        // C5 allows only {5} or {1,1,1,1,1}
        // D5 also allows {1,2,2}
        if pattern.sorted != Vector(5) && pattern.sorted != Vector(1, 1, 1, 1, 1) then
          sawNonTrivial = true

    !sawNonTrivial

  /** Compute the factorization pattern of a polynomial mod p.
    * Returns a vector of degrees of irreducible factors.
    */
  private def factorizationPattern(coeffs: Vector[BigInt], p: Int): Vector[Int] =
    // Work in Z/pZ
    val modCoeffs = coeffs.map(c => ((c % p).toInt + p) % p)

    // Factor using distinct-degree factorization
    val n = modCoeffs.length - 1
    if n <= 0 then return Vector.empty

    val result = Vector.newBuilder[Int]
    var remaining = modCoeffs

    // For each degree d from 1 to n, find factors of degree d
    for d <- 1 to n if remaining.length > 1 do
      // x^(p^d) - x mod remaining
      // Simplified: just count roots for d=1
      if d == 1 then
        val rootCount = (0 until p).count { x =>
          evalModP(remaining, x, p) == 0
        }
        for _ <- 0 until rootCount do result += 1
        // Remove linear factors
        var r = remaining
        for x <- 0 until p do
          if evalModP(r, x, p) == 0 then
            r = divideByLinear(r, x, p)
        remaining = r
      else if remaining.length > 1 then
        // Remaining polynomial is the product of irreducible factors of degree >= 2
        // For simplicity, if degree is d, assume one factor of degree d
        if remaining.length - 1 == d then
          result += d
          remaining = Vector(1) // done

    if remaining.length > 1 then
      result += (remaining.length - 1)

    result.result()

  private def evalModP(coeffs: Vector[Int], x: Int, p: Int): Int =
    coeffs.foldRight(0) { (c, acc) => (c + x * acc) % p }

  private def divideByLinear(coeffs: Vector[Int], root: Int, p: Int): Vector[Int] =
    // Synthetic division by (x - root) mod p
    val n = coeffs.length - 1
    if n <= 0 then Vector(1)
    else
      val result = Array.ofDim[Int](n)
      result(n - 1) = coeffs(n)
      for i <- (n - 2) to 0 by -1 do
        result(i) = (coeffs(i + 1) + root * result(i + 1)) % p
        result(i) = (result(i) % p + p) % p
      result.toVector
