package surd

/** Galois group identification for irreducible polynomials over Q.
  *
  * Uses the Stauduhar descent approach with discriminant and resolvent tests.
  *
  * For degree 5, uses the optimised decision tree:
  *   disc square?  sextic root?  result
  *   no            no            S5
  *   yes           no            A5
  *   no            yes           F20
  *   yes           yes           D5 or C5 (Frobenius test)
  *
  * For other prime degrees p, uses generalized Frobenius/Chebotarev:
  *   1. Factor f mod small primes to collect cycle patterns.
  *   2. If any pattern is not AGL(1,p)-compatible, group is A_p or S_p.
  *   3. Otherwise, the minimum stabiliser order d = lcm of non-trivial
  *      cycle lengths determines the group Z/p ⋊ Z/d.
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

  /** Identify the Galois group of an irreducible polynomial over Q.
    * Dispatches to degree-specific or prime-degree general method.
    */
  def identifyGaloisGroup(f: Poly[Rational]): Option[GaloisResult] =
    if f.degree == 5 then identifyGaloisGroup5(f)
    else if f.degree >= 3 && PrimeFactors.isPrime(f.degree) then identifyGaloisGroupPrime(f)
    else None

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
        if frobeniusTestCyclic(f, 5) then TransitiveGroup.c5
        else TransitiveGroup.d5

    Some(GaloisResult(group, roots, disc, discSq))

  /** Identify the Galois group of a prime-degree polynomial via
    * generalized Frobenius/Chebotarev descent.
    *
    * Strategy:
    * 1. Compute discriminant. If perfect square, group <= A_p.
    * 2. Collect factorisation patterns mod small primes.
    * 3. AGL(1,p) patterns: [p], [1,...,1], or [1, k, k, ...k].
    *    Non-AGL pattern means A_p or S_p.
    * 4. Within AGL: min stabiliser d = lcm of non-trivial cycle lengths.
    */
  private def identifyGaloisGroupPrime(f: Poly[Rational]): Option[GaloisResult] =
    val n = f.degree
    val p = n.toLong
    val roots = Resolvent.complexRootsOf(f)
    if roots.length != n then return None

    val disc = Resolvent.discriminantOf(f)
    val discSq = Resolvent.isSquareRational(disc)
    val groups = TransitiveGroup.transGroupsOfDegree(n)

    // Convert to integer polynomial
    val lcmDen = f.coeffs.map(_.den).foldLeft(BigInt(1))((a, b) => a / a.gcd(b) * b)
    val intCoeffs = f.coeffs.map(c => (c * Rational(lcmDen)).num)
    val lc = intCoeffs.last
    val discN = disc.num
    val discD = disc.den

    // Collect factorisation patterns mod small test primes
    def goodPrime(pr: Int): Boolean =
      val prB = BigInt(pr)
      lc % prB != 0 && discN % prB != 0 && discD % prB != 0

    val testPrimes = primeStream.filter(goodPrime).take(50).toVector
    val patterns = testPrimes.map { pr =>
      val modCoeffs = intCoeffs.map(c => ((c % pr).toInt + pr) % pr)
      factorizationPatternFull(modCoeffs, pr).sorted
    }

    // Check if a pattern is consistent with AGL(1,p)
    def isAGLPattern(pat: Vector[Int]): Boolean =
      pat == Vector(n) ||                              // translation: [p]
      pat == Vector.fill(n)(1) ||                      // identity: [1,...,1]
      (pat.length >= 2 &&                              // non-translation: [1, k, k, ...]
        pat.min == 1 &&
        pat.count(_ == 1) == 1 && {
          val ks = pat.filter(_ != 1)
          ks.forall(_ == ks.head)
        })

    val insideAGL = patterns.forall(isAGLPattern)

    if !insideAGL then
      // Not in AGL(1,p): either A_p or S_p
      val finalGroup = if discSq then
        groups.find(_.name == s"A$n").getOrElse(groups.last)
      else
        groups.last // S_p
      Some(GaloisResult(finalGroup, roots, disc, discSq))
    else
      // Inside AGL(1,p): find minimum d from observed cycle lengths
      val nonTrivCycleLengths = patterns.flatMap { pat =>
        if pat == Vector(n) || pat == Vector.fill(n)(1) then Vector.empty
        else pat.filter(_ != 1).map(_.toLong)
      }
      val minD = if nonTrivCycleLengths.isEmpty then 1L
                 else nonTrivCycleLengths.foldLeft(1L)(lcm)

      // Find the smallest d >= minD that divides p-1
      val divs = TransitiveGroup.divisors(p - 1)
      val groupD = divs.find(_ >= minD).getOrElse(p - 1)

      val finalGroup = groups.find(_.order == p * groupD).getOrElse {
        groups.find(_.name == s"AGL(1,$n)").getOrElse(groups.last)
      }
      Some(GaloisResult(finalGroup, roots, disc, discSq))

  /** Frobenius/Chebotarev test: is the Galois group cyclic (Z/p)?
    *
    * For Z/p, factorisation patterns mod primes are either
    * {p} (inert) or {1,1,...,1} (split). Any other pattern rules out cyclic.
    */
  private def frobeniusTestCyclic(f: Poly[Rational], p: Int): Boolean =
    val lcmDen = f.coeffs.map(_.den).foldLeft(BigInt(1))((a, b) => a / a.gcd(b) * b)
    val intCoeffs = f.coeffs.map(c => (c * Rational(lcmDen)).num)

    var primesChecked = 0
    var sawNonTrivial = false

    for pr <- primeStream.take(50) if primesChecked < 20 do
      if intCoeffs.last % pr != 0 then
        val modCoeffs = intCoeffs.map(c => ((c % pr).toInt + pr) % pr)
        val pattern = factorizationPatternFull(modCoeffs, pr).sorted
        primesChecked += 1
        if pattern != Vector(p) && pattern != Vector.fill(p)(1) then
          sawNonTrivial = true

    !sawNonTrivial

  /** Compute the factorization pattern of a polynomial mod p using
    * distinct-degree factorization.
    *
    * For each degree d, compute gcd(x^{p^d} - x, f) in F_p[x].
    * Each non-trivial GCD factor of degree g contributes g/d
    * irreducible factors of degree d.
    */
  private def factorizationPatternFull(coeffs: Vector[Int], p: Int): Vector[Int] =
    val pL = p.toLong
    val fcs = coeffs.map(_.toLong)
    val trimmed = fpTrim(fcs)
    if fpDeg(trimmed) <= 0 then return Vector.empty

    val degs = Vector.newBuilder[Int]
    var remaining = trimmed
    var h: Vector[Long] = Vector(0L, 1L) // h = x

    var d = 1
    while fpDeg(remaining) > 0 && d <= fpDeg(remaining) do
      // h <- h^p mod remaining
      h = fpPowMod(h, pL, remaining, pL)
      // g = gcd(h - x, remaining)
      val hx = fpSub(h, Vector(0L, 1L), pL)
      val g = fpGcd(hx, remaining, pL)
      val gd = fpDeg(g)
      if gd > 0 then
        val nf = gd / d
        for _ <- 0 until nf do degs += d
        remaining = fpDiv(remaining, g, pL)
      d += 1

    if fpDeg(remaining) > 0 then
      degs += fpDeg(remaining)

    degs.result()

  // -----------------------------------------------------------------------
  // F_p polynomial arithmetic
  // -----------------------------------------------------------------------

  private def fpTrim(cs: Vector[Long]): Vector[Long] =
    val trimmed = cs.reverse.dropWhile(_ == 0L).reverse
    if trimmed.isEmpty then Vector(0L) else trimmed

  private def fpDeg(cs: Vector[Long]): Int =
    val trimmed = fpTrim(cs)
    if trimmed == Vector(0L) then -1 else trimmed.length - 1

  private def fpAdd(a: Vector[Long], b: Vector[Long], p: Long): Vector[Long] =
    val n = math.max(a.length, b.length)
    val aP = a.padTo(n, 0L)
    val bP = b.padTo(n, 0L)
    fpTrim(aP.zip(bP).map((x, y) => (x + y) % p))

  private def fpSub(a: Vector[Long], b: Vector[Long], p: Long): Vector[Long] =
    val n = math.max(a.length, b.length)
    val aP = a.padTo(n, 0L)
    val bP = b.padTo(n, 0L)
    fpTrim(aP.zip(bP).map((x, y) => ((x - y) % p + p) % p))

  private def fpMul(a: Vector[Long], b: Vector[Long], p: Long): Vector[Long] =
    if a.isEmpty || b.isEmpty then Vector(0L)
    else
      val result = Array.fill(a.length + b.length - 1)(0L)
      for i <- a.indices; j <- b.indices do
        result(i + j) = (result(i + j) + a(i) * b(j)) % p
      fpTrim(result.toVector)

  private def fpMod(a: Vector[Long], b: Vector[Long], p: Long): Vector[Long] =
    val da = fpDeg(a)
    val db = fpDeg(b)
    if da < db then fpTrim(a.map(x => ((x % p) + p) % p))
    else if db < 0 then throw ArithmeticException("fpMod: division by zero")
    else
      val tb = fpTrim(b)
      val lcBInv = fpInv(tb.last, p)
      val ta = fpTrim(a)
      val shift = da - db
      val lcA = ta.last
      val fac = (lcA * lcBInv) % p
      val sub = ta.indices.map { i =>
        val bi = if i >= shift && (i - shift) < tb.length then tb(i - shift) else 0L
        ((ta(i) - fac * bi) % p + p) % p
      }.toVector
      fpMod(fpTrim(sub), tb, p)

  private def fpDiv(a: Vector[Long], b: Vector[Long], p: Long): Vector[Long] =
    val db = fpDeg(b)
    val tb = fpTrim(b)
    val lcBInv = fpInv(tb.last, p)
    def go(q: Vector[Long], r: Vector[Long]): Vector[Long] =
      if fpDeg(r) < db then fpTrim(q)
      else
        val tr = fpTrim(r)
        val dr = fpDeg(r)
        val shift = dr - db
        val fac = (tr.last * lcBInv) % p
        val qTerm = Vector.fill(shift)(0L) :+ fac
        val qP = fpAdd(q, qTerm, p)
        val sub = tr.indices.map { i =>
          val bi = if i >= shift && (i - shift) < tb.length then (fac * tb(i - shift)) % p else 0L
          ((tr(i) - bi) % p + p) % p
        }.toVector
        go(qP, fpTrim(sub))
    go(Vector(0L), fpTrim(a))

  private def fpGcd(a: Vector[Long], b: Vector[Long], p: Long): Vector[Long] =
    if fpDeg(b) < 0 then fpMakeMonic(fpTrim(a), p)
    else fpGcd(b, fpMod(a, b, p), p)

  private def fpMakeMonic(cs: Vector[Long], p: Long): Vector[Long] =
    if cs.isEmpty || fpDeg(cs) < 0 then cs
    else
      val lcInv = fpInv(cs.last, p)
      cs.map(c => (c * lcInv) % p)

  private def fpPowMod(base: Vector[Long], expo: Long, modulus: Vector[Long], p: Long): Vector[Long] =
    def go(res: Vector[Long], b: Vector[Long], e: Long): Vector[Long] =
      if e == 0 then res
      else
        val resP = if e % 2 == 1 then fpMod(fpMul(res, b, p), modulus, p) else res
        val bP = fpMod(fpMul(b, b, p), modulus, p)
        go(resP, bP, e / 2)
    go(Vector(1L), base, expo)

  private def fpInv(a: Long, m: Long): Long =
    val (_, x, _) = eGcd(a, m)
    ((x % m) + m) % m

  private def eGcd(a: Long, b: Long): (Long, Long, Long) =
    if a == 0 then (b, 0L, 1L)
    else
      val (g, x, y) = eGcd(b % a, a)
      (g, y - (b / a) * x, x)

  // -----------------------------------------------------------------------
  // Helpers
  // -----------------------------------------------------------------------

  /** Small odd primes for Frobenius/Chebotarev test. */
  private lazy val primeStream: LazyList[Int] =
    PrimeFactors.primes.drop(1).map(_.toInt) // skip 2

  private def lcm(a: Long, b: Long): Long =
    if a == 0 || b == 0 then 0L
    else (a / gcd(a, b)) * b

  private def gcd(a: Long, b: Long): Long =
    if b == 0 then a.abs else gcd(b, a % b)

  private def evalModP(coeffs: Vector[Int], x: Int, p: Int): Int =
    coeffs.foldRight(0) { (c, acc) => (c + x * acc) % p }

  private def divideByLinear(coeffs: Vector[Int], root: Int, p: Int): Vector[Int] =
    val n = coeffs.length - 1
    if n <= 0 then Vector(1)
    else
      val result = Array.ofDim[Int](n)
      result(n - 1) = coeffs(n)
      for i <- (n - 2) to 0 by -1 do
        result(i) = (coeffs(i + 1) + root * result(i + 1)) % p
        result(i) = (result(i) % p + p) % p
      result.toVector
