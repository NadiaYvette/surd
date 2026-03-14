package surd

import RadExpr.*

/** Radical tower construction for solvable polynomials.
  *
  * Given an identified Galois group (solvable), construct explicit
  * radical expressions for the roots by descending through the
  * composition series.
  *
  * The algorithm uses Lagrange resolvents and DFT:
  *
  * 1. Depress: shift to eliminate the second-highest term.
  * 2. Cyclic ordering: permute numerical roots so the Galois generator
  *    acts as the n-cycle (0 1 2 ... n-1).
  * 3. Lagrange resolvents: R_j = sum_{k} omega_n^{jk} alpha_k.
  * 4. DFT: compute d_s from R_j^n.
  * 5. Coefficient matching: dispatch based on Galois group structure.
  * 6. Reconstruct R_j^n as RadExpr, take nth root.
  * 7. Branch selection via numerical comparison.
  * 8. Inverse DFT: recover roots.
  * 9. Un-depress and match to original root ordering.
  */
object RadicalTower:
  import Eval.Complex
  given fld: Field[Rational] = summon[Field[Rational]]

  /** Solve a polynomial via radical tower construction.
    * Returns radical expressions for all roots.
    *
    * For degree 5, uses the specialised quintic pipeline.
    * For other prime degrees, uses the generalised prime-degree solver.
    */
  def solveViaTower(galResult: Identify.GaloisResult, f: Poly[Rational]): Option[Vector[RadExpr[Rational]]] =
    val group = galResult.group
    if !group.solvable then return None
    val n = f.degree
    if n == 5 then solveSolvable(group, f, galResult.roots)
    else if n >= 3 && PrimeFactors.isPrime(n) then solveSolvable(group, f, galResult.roots)
    else None

  // -----------------------------------------------------------------------
  // Core solver (unified for all prime degrees)
  // -----------------------------------------------------------------------

  /** Core algorithm for solvable polynomials of prime degree n.
    *
    * Implements the full 9-step Lagrange resolvent pipeline.
    */
  private def solveSolvable(
      tg: TransitiveGroup.TGroup,
      f: Poly[Rational],
      numRoots: Vector[Complex]
  ): Option[Vector[RadExpr[Rational]]] =
    val n = f.degree

    // Step 1: Depress (make monic and eliminate x^{n-1} term)
    val lc = f.coeffs.last
    val monicCs = f.coeffs.map(_ / lc)
    val an1 = monicCs(n - 1)
    val shiftVal = -(an1 / Rational(n))
    val depRoots = numRoots.map(r => r + Complex.fromDouble(shiftVal.toDouble))

    // Step 2: Find cyclic ordering
    val ordering = findCyclicOrdering(depRoots, n, tg)
    ordering match
      case None => return None
      case _ =>

    val ord = ordering.get
    val orderedRoots = ord.map(i => depRoots(i))

    // Step 3: Lagrange resolvents R_j = sum_k omega_n^{jk} alpha_k
    val rjVals = (0 until n).map { j =>
      (0 until n).map(k => omegaNC(n, j * k) * orderedRoots(k)).reduce(_ + _)
    }.toVector
    val rjPows = rjVals.map(_.pow(n))

    // Step 4: DFT: d_s = (1/n) sum_j omega_n^{-js} R_j^n
    val dVals = (0 until n).map { s =>
      val sum = (0 until n).map { j =>
        omegaNC(n, n - (j * s) % n) * rjPows(j)
      }.reduce(_ + _)
      Complex(sum.re / n, sum.im / n)
    }.toVector

    // Step 5: Match d_s to radical expressions
    val dExprs = matchDs(tg, dVals, n)
    dExprs match
      case None => return None
      case _ =>

    val ds = dExprs.get

    // Step 6: R_j^n = sum_s d_s * omega_n^{js}
    val omExpr = omegaNExpr(n)
    val rjPowExprs = (1 until n).map { j =>
      (0 until n).map { s =>
        Mul(ds(s), omegaPowNExpr(n, omExpr, (j * s) % n))
      }.reduce[RadExpr[Rational]](Add(_, _))
    }.toVector

    // Step 7: Branch selection: R_j = omega_n^{b_j} * nth_root(R_j^n)
    val rjExprs = (1 until n).map { j =>
      selectBranchN(n, omExpr, rjPowExprs(j - 1), rjVals(j))
    }.toVector

    // R_0 = sum of depressed roots = 0
    val allR: Vector[RadExpr[Rational]] = Lit(Rational.zero) +: rjExprs

    // Step 8: Inverse DFT: alpha_k = (1/n) sum_j omega_n^{-jk} R_j
    val rootExprs = (0 until n).map { k =>
      val inner = (0 until n).map { j =>
        Mul(omegaPowNExpr(n, omExpr, (n - (j * k) % n) % n), allR(j))
      }.reduce[RadExpr[Rational]](Add(_, _))
      dagFold(Mul(Inv(Lit(Rational(n))), inner))
    }.toVector

    // Step 9: Un-depress and match to original root ordering
    val finalExprs = rootExprs.map(e => dagFold(Add(e, Lit(shiftVal))))
    Some(matchToOriginal(finalExprs, numRoots))

  // -----------------------------------------------------------------------
  // Cyclic ordering
  // -----------------------------------------------------------------------

  /** Find a cyclic ordering of the n numerical roots such that the
    * Galois generator acts as the cyclic shift (0 1 2 ... n-1).
    *
    * For n <= 8, brute-forces all (n-1)! orderings.
    * For larger n, tests (n-1) rotation candidates.
    */
  private def findCyclicOrdering(
      roots: Vector[Complex],
      n: Int,
      tg: TransitiveGroup.TGroup
  ): Option[Vector[Int]] =
    if n <= 8 then findCyclicOrderingBruteForce(roots, n, tg)
    else findCyclicOrderingByRotation(roots, n, tg)

  private def findCyclicOrderingBruteForce(
      roots: Vector[Complex],
      n: Int,
      tg: TransitiveGroup.TGroup
  ): Option[Vector[Int]] =
    val rest = (1 until n).toVector
    val orderings = rest.permutations.map(p => 0 +: p).toVector
    val scored = orderings.map(o => (o, scoreOrdering(roots, o, n, tg)))
    val sorted = scored.sortBy(_._2)
    sorted match
      case (bestO, bestScore) +: (_, secondScore) +: _ =>
        if bestScore < 2.0 * n && bestScore < 0.5 * secondScore then Some(bestO)
        else if bestScore < 5.0 * n then Some(bestO)
        else None
      case (bestO, bestScore) +: _ =>
        if bestScore < 5.0 * n then Some(bestO) else None
      case _ => None

  /** For large n, find cyclic ordering by testing which assignment of
    * root 1 (given root 0 is fixed) produces the best DFT score.
    *
    * Once alpha_0 and alpha_1 are fixed, builds the full ordering
    * greedily by nearest-neighbour stepping.
    */
  private def findCyclicOrderingByRotation(
      roots: Vector[Complex],
      n: Int,
      tg: TransitiveGroup.TGroup
  ): Option[Vector[Int]] =
    val candidates = (1 until n).map { next =>
      val ordering = buildOrdering(roots, n, 0, next)
      (ordering, scoreOrdering(roots, ordering, n, tg))
    }.filter(_._1.length == n)
    val sorted = candidates.sortBy(_._2)
    sorted.headOption match
      case Some((bestO, bestScore)) if bestScore < 5.0 * n => Some(bestO)
      case _ => None

  /** Build a cyclic ordering by fixing root 0 at position 0 and root
    * `next` at position 1, then greedily assigning subsequent positions
    * by nearest-neighbour stepping.
    */
  private def buildOrdering(roots: Vector[Complex], n: Int, start: Int, next: Int): Vector[Int] =
    val step = roots(next) - roots(start)
    val builder = Vector.newBuilder[Int]
    builder += start
    builder += next
    var used = Set(start, next)
    var pos = next
    for _ <- 2 until n do
      val target = roots(pos) + step
      val unused = (0 until n).filterNot(used.contains)
      if unused.isEmpty then return builder.result()
      val closest = unused.minBy(i => (roots(i) - target).magnitude)
      builder += closest
      used += closest
      pos = closest
    builder.result()

  // -----------------------------------------------------------------------
  // Scoring
  // -----------------------------------------------------------------------

  /** Score a candidate ordering by measuring how close the DFT coefficients'
    * Galois-orbit symmetric functions are to rationals.
    *
    * For cyclic (d=1): all d_s should be rational.
    * For dihedral (d=2): d_0 rational, conjugate pairs.
    * For larger d: elementary symmetric functions of orbits should be rational.
    */
  private def scoreOrdering(
      roots: Vector[Complex],
      ordering: Vector[Int],
      n: Int,
      tg: TransitiveGroup.TGroup
  ): Double =
    val ordered = ordering.map(roots(_))
    val rjPows = (0 until n).map { j =>
      val rj = (0 until n).map(k => omegaNC(n, j * k) * ordered(k)).reduce(_ + _)
      rj.pow(n)
    }.toVector
    val dVals = (0 until n).map { s =>
      val sum = (0 until n).map { j =>
        omegaNC(n, n - (j * s) % n) * rjPows(j)
      }.reduce(_ + _)
      Complex(sum.re / n, sum.im / n)
    }.toVector

    val p = n.toLong
    val d = tg.order / p

    if d == 1 then
      // Cyclic: all d_s should be rational
      dVals.map(scoreRational).sum
    else if d == 2 then
      // Dihedral: d_0 rational, conjugate pairs {d_s, d_{n-s}}
      scoreRational(dVals(0)) +
        (1 to n / 2).map { s =>
          val ds = dVals(s)
          val dns = dVals(n - s)
          scoreRational(ds + dns) + scoreRational(ds * dns)
        }.sum
    else
      // General: check orbit structure
      scoreOrderingGeneral(dVals, n, tg)

  /** Score for general solvable groups with d > 2.
    *
    * Computes the orbits of {1,...,n-1} under the action of the
    * stabiliser H (multiplication by g^{(p-1)/d} mod p), then
    * checks elementary symmetric polynomials of each orbit.
    */
  private def scoreOrderingGeneral(
      dVals: Vector[Complex],
      n: Int,
      tg: TransitiveGroup.TGroup
  ): Double =
    val p = n.toLong
    val d = tg.order / p
    val g = TransitiveGroup.primitiveRootP(p)
    val scaleFactor = TransitiveGroup.modExp(g, (p - 1) / d, p)

    // Compute orbits of {1,...,n-1} under multiplication by scaleFactor mod p
    val orbits = computeOrbits(n, scaleFactor, p)

    // d_0 should be rational
    var score = scoreRational(dVals(0))

    // For each orbit, check elementary symmetric polynomials
    for orbit <- orbits do
      val orbitVals = orbit.map(s => dVals(s.toInt))
      score += elemSymScore(orbitVals)

    score

  /** Compute orbits of {1,...,n-1} under multiplication by s mod p. */
  private def computeOrbits(n: Int, s: Long, p: Long): Vector[Vector[Long]] =
    var remaining = (1 until n).map(_.toLong).toSet
    val orbits = Vector.newBuilder[Vector[Long]]
    while remaining.nonEmpty do
      val start = remaining.min
      val orbit = Vector.newBuilder[Long]
      var cur = start
      while remaining.contains(cur) do
        orbit += cur
        remaining -= cur
        cur = (cur * s) % p
      orbits += orbit.result()
    orbits.result()

  /** Score how close the elementary symmetric polynomials of a set
    * of complex values are to rationals.
    *
    * Computes e_k via the coefficients of prod(x - v_i), which are
    * (-1)^k * e_k. Uses incremental polynomial multiplication.
    */
  private def elemSymScore(vals: Vector[Complex]): Double =
    val n = vals.length
    if n == 0 then return 0.0
    // Build coefficients of prod(x - v_i) incrementally
    // coeffs(k) = coefficient of x^k in the product
    var coeffs = Vector(Complex(-vals(0).re, -vals(0).im), Complex.one)
    for i <- 1 until n do
      val v = vals(i)
      val newCoeffs = Array.fill(coeffs.length + 1)(Complex.zero)
      for j <- coeffs.indices do
        // Multiply by (x - v): shift up and subtract
        newCoeffs(j + 1) = newCoeffs(j + 1) + coeffs(j)
        newCoeffs(j) = newCoeffs(j) + coeffs(j) * Complex(-v.re, -v.im)
      coeffs = newCoeffs.toVector

    // coeffs(k) = (-1)^(n-k) * e_{n-k} for the elementary symmetric polys
    // Check that all coefficients (except leading x^n) are close to rational
    // coeffs(0) = (-1)^n * e_n, coeffs(1) = (-1)^{n-1} * e_{n-1}, etc.
    // Score at most 5 coefficients to keep cost reasonable
    val numToCheck = math.min(n, 5)
    (0 until numToCheck).map(k => scoreRational(coeffs(k))).sum

  /** How close is a complex number to a rational? */
  private def scoreRational(z: Complex): Double =
    math.abs(z.im) + fracPart(z.re)

  /** Distance from nearest integer. */
  private def fracPart(x: Double): Double =
    math.abs(x - math.round(x).toDouble)

  // -----------------------------------------------------------------------
  // DFT coefficient matching
  // -----------------------------------------------------------------------

  /** Match DFT coefficients d_s to radical expressions,
    * dispatching on the Galois group structure.
    */
  private def matchDs(
      tg: TransitiveGroup.TGroup,
      dVals: Vector[Complex],
      n: Int
  ): Option[Vector[RadExpr[Rational]]] =
    val p = n.toLong
    val d = tg.order / p
    if d == 1 then matchDsAllRational(dVals)
    else if d == 2 then matchDsDihedral(dVals, n)
    else matchDsViaOrbits(tg, dVals, n)

  /** All d_s must be rational (cyclic Galois group). */
  private def matchDsAllRational(dVals: Vector[Complex]): Option[Vector[RadExpr[Rational]]] =
    val results = dVals.map(matchRatC)
    if results.forall(_.isDefined) then Some(results.map(_.get))
    else None

  /** Match a complex value to a rational literal. */
  private def matchRatC(z: Complex): Option[RadExpr[Rational]] =
    if math.abs(z.im) > 0.1 then None
    else Some(Lit(approxRat(z.re)))

  /** Dihedral group: d_0 rational, conjugate pairs {d_s, d_{n-s}}
    * satisfy quadratics over Q.
    */
  private def matchDsDihedral(dVals: Vector[Complex], n: Int): Option[Vector[RadExpr[Rational]]] =
    val d0 = matchRatC(dVals(0))
    if d0.isEmpty then return None

    val halfN = (n - 1) / 2
    val result = Array.fill[RadExpr[Rational]](n)(Lit(Rational.zero))
    result(0) = d0.get

    for s <- 1 to halfN do
      val pair = matchConjPair(dVals(s), dVals(n - s))
      if pair.isEmpty then return None
      val (eS, eNS) = pair.get
      result(s) = eS
      result(n - s) = eNS

    Some(result.toVector)

  /** Match a conjugate pair {d1, d2} to quadratic radical expressions.
    * s = d1 + d2 and p = d1 * d2 should be rational.
    */
  private def matchConjPair(d1: Complex, d2: Complex): Option[(RadExpr[Rational], RadExpr[Rational])] =
    val s = d1 + d2
    val p = d1 * d2
    val sExpr = matchRatC(s)
    val pExpr = matchRatC(p)
    if sExpr.isEmpty || pExpr.isEmpty then return None

    val sR = approxRat(s.re)
    val pR = approxRat(p.re)
    val discR = sR * sR - Rational(4) * pR
    val discExpr = Add(Mul(sExpr.get, sExpr.get), Neg(Mul(Lit(Rational(4)), pExpr.get)))
    val sqrtDisc = Root(2, discExpr)
    val ePlus = Mul(Inv(Lit(Rational(2))), Add(sExpr.get, sqrtDisc))
    val eMinus = Mul(Inv(Lit(Rational(2))), Add(sExpr.get, Neg(sqrtDisc)))

    // Determine which branch matches d1
    val sqrtDiscVal =
      if discR >= Rational.zero then Complex(math.sqrt(discR.toDouble), 0.0)
      else Complex(0.0, math.sqrt(math.abs(discR.toDouble)))
    val d1PlusVal = Complex((s.re + sqrtDiscVal.re) / 2, (s.im + sqrtDiscVal.im) / 2)

    if (d1PlusVal - d1).magnitude < (d1PlusVal - d2).magnitude then
      Some((ePlus, eMinus))
    else
      Some((eMinus, ePlus))

  /** Match DFT coefficients via Galois orbit structure for larger
    * stabiliser groups (d > 2).
    *
    * For each orbit of {d_1,...,d_{n-1}} under the stabiliser action,
    * the elementary symmetric polynomials are rational. For orbit
    * sizes <= 4, use quadratic/Cardano/Ferrari formulas.
    */
  private def matchDsViaOrbits(
      tg: TransitiveGroup.TGroup,
      dVals: Vector[Complex],
      n: Int
  ): Option[Vector[RadExpr[Rational]]] =
    // Try rational match first (works when d_s happen to be rational)
    matchDsAllRational(dVals) match
      case Some(exprs) => Some(exprs)
      case None =>
        val d0 = matchRatC(dVals(0))
        if d0.isEmpty then return None

        // Compute orbits
        val p = n.toLong
        val d = tg.order / p
        val g = TransitiveGroup.primitiveRootP(p)
        val scaleFactor = TransitiveGroup.modExp(g, (p - 1) / d, p)
        val orbits = computeOrbits(n, scaleFactor, p)

        val result = Array.fill[RadExpr[Rational]](n)(Lit(Rational.zero))
        result(0) = d0.get

        for orbit <- orbits do
          val orbitVals = orbit.map(s => dVals(s.toInt))
          val orbitExprs = matchOrbit(orbitVals, n)
          if orbitExprs.isEmpty then return None
          orbit.zip(orbitExprs.get).foreach { (s, e) =>
            result(s.toInt) = e
          }

        Some(result.toVector)

  /** Match an orbit of d_s values to radical expressions.
    * Tries Q(omega_n) decomposition for each value.
    */
  private def matchOrbit(vals: Vector[Complex], n: Int): Option[Vector[RadExpr[Rational]]] =
    if vals.length == 1 then
      // Single-element orbit: d_s is rational
      matchRatC(vals(0)).map(Vector(_))
    else if vals.length == 2 then
      // Conjugate pair
      matchConjPair(vals(0), vals(1)).map((a, b) => Vector(a, b))
    else
      // General: try matching each value in Q(omega_n)
      val results = vals.map(v => matchQOmegaN(n, v))
      if results.forall(_.isDefined) then Some(results.map(_.get))
      else None

  /** Match a complex number to an element of Q(omega_n).
    *
    * Tries single-term (d = r * omega_n^k), then 2-term decomposition.
    */
  private def matchQOmegaN(n: Int, d: Complex): Option[RadExpr[Rational]] =
    // Try single-term: d ~ r * omega_n^k
    val candidates = (0 until n).map { k =>
      val v = d * omegaNC(n, -k)
      (k, v, scoreRational(v))
    }
    val (bestK, bestV, bestScore) = candidates.minBy(_._3)
    if bestScore < 0.01 then
      val r = approxRat(bestV.re)
      val omExpr = omegaNExpr(n)
      if bestK == 0 then Some(Lit(r))
      else Some(Mul(Lit(r), omegaPowNExpr(n, omExpr, bestK)))
    else
      // Try 2-term decomposition: d = a * omega_n^j + b * omega_n^k
      matchTwoTermOmegaN(n, d)

  /** Try 2-term decomposition: d = a * omega_n^j + b * omega_n^k. */
  private def matchTwoTermOmegaN(n: Int, d: Complex): Option[RadExpr[Rational]] =
    val omExpr = omegaNExpr(n)
    val candidates = for
      j <- 0 until n
      k <- (j + 1) until n
      wj = omegaNC(n, j)
      wk = omegaNC(n, k)
      det = wj.re * wk.im - wj.im * wk.re
      if math.abs(det) > 1e-10
      a = (d.re * wk.im - d.im * wk.re) / det
      b = (wj.re * d.im - wj.im * d.re) / det
      aR = approxRat(a)
      bR = approxRat(b)
      recon = Complex(aR.toDouble, 0) * wj + Complex(bR.toDouble, 0) * wk
      err = (recon - d).magnitude
      if err < 0.01
    yield (err, j, aR, k, bR)

    candidates.sortBy(_._1).headOption.map { case (_, j, aR, k, bR) =>
      val termJ = if j == 0 then Lit(aR) else Mul(Lit(aR), omegaPowNExpr(n, omExpr, j))
      val termK = if k == 0 then Lit(bR) else Mul(Lit(bR), omegaPowNExpr(n, omExpr, k))
      Add(termJ, termK)
    }

  // -----------------------------------------------------------------------
  // omega_n expressions
  // -----------------------------------------------------------------------

  /** omega_5 = e^{2*pi*i/5} as a radical expression. */
  private val omega5Expr: RadExpr[Rational] =
    val cos5 = Mul(Inv(Lit(Rational(4))), Add(Root(2, Lit(Rational(5))), Neg(Lit(Rational.one))))
    val sin5 = Mul(Inv(Lit(Rational(4))), Root(2, Add(Lit(Rational(10)), Mul(Lit(Rational(2)), Root(2, Lit(Rational(5)))))))
    val i = Root(2, Lit(Rational(-1)))
    Add(cos5, Mul(i, sin5))

  /** omega_n = e^{2*pi*i/n} as a radical expression.
    *
    * Uses allPeriodsViaGauss to get zeta_n directly, with fallback
    * to numerical approximation.
    */
  private def omegaNExpr(n: Int): RadExpr[Rational] =
    if n == 5 then omega5Expr
    else
      TrigGalois.allPeriodsViaGauss(n) match
        case Some(periods) =>
          periods.get(1) match
            case Some(zeta) => zeta
            case None => fallbackOmega(n)
        case None => fallbackOmega(n)

  /** Fallback: build omega_n from numerical cos/sin approximation. */
  private def fallbackOmega(n: Int): RadExpr[Rational] =
    val theta = 2.0 * math.Pi / n
    val cosVal = approxRat(math.cos(theta))
    val sinVal = approxRat(math.sin(theta))
    Add(Lit(cosVal), Mul(Root(2, Lit(Rational(-1))), Lit(sinVal)))

  /** omega_n^k as a radical expression, reduced mod n. */
  private def omegaPowNExpr(n: Int, omExpr: RadExpr[Rational], k: Int): RadExpr[Rational] =
    val kMod = ((k % n) + n) % n
    if kMod == 0 then Lit(Rational.one)
    else if kMod == 1 then omExpr
    else Pow(omExpr, kMod)

  /** omega_n^k as Complex Double for numerical evaluation. */
  private def omegaNC(n: Int, k: Int): Complex =
    val angle = 2.0 * math.Pi * k / n
    Complex(math.cos(angle), math.sin(angle))

  // -----------------------------------------------------------------------
  // Branch selection
  // -----------------------------------------------------------------------

  /** Select the correct branch of the nth root of R_j^n.
    *
    * Tries all n candidates omega_n^k * nth_root(R_j^n) and picks
    * the one closest to the known numerical value.
    */
  private def selectBranchN(
      n: Int,
      omExpr: RadExpr[Rational],
      rjnExpr: RadExpr[Rational],
      targetVal: Complex
  ): RadExpr[Rational] =
    val rjnVal = dagEvalC(rjnExpr)
    val mag = math.pow(rjnVal.magnitude, 1.0 / n)
    val ph = rjnVal.phase / n
    val principalVal = Complex(mag * math.cos(ph), mag * math.sin(ph))
    val principalRoot = Root(n, rjnExpr)

    val scored = (0 until n).map { k =>
      val candidate = omegaNC(n, k) * principalVal
      (k, (candidate - targetVal).magnitude)
    }
    val bestK = scored.minBy(_._2)._1

    if bestK == 0 then principalRoot
    else Mul(omegaPowNExpr(n, omExpr, bestK), principalRoot)

  // -----------------------------------------------------------------------
  // Helpers
  // -----------------------------------------------------------------------

  /** Evaluate a RadExpr to Complex Double via the DAG evaluator. */
  private def dagEvalC(expr: RadExpr[Rational]): Complex =
    DAG.dagEvalComplex(DAG.toDAG(expr))

  /** Fold constants in a RadExpr via the DAG constant-folding pass. */
  private def dagFold(expr: RadExpr[Rational]): RadExpr[Rational] =
    DAG.fromDAG(DAG.dagFoldConstants(DAG.toDAG(expr)))

  /** Match radical expressions to original numerical roots by proximity. */
  private def matchToOriginal(
      exprs: Vector[RadExpr[Rational]],
      numRoots: Vector[Complex]
  ): Vector[RadExpr[Rational]] =
    val exprVals = exprs.map(e => (e, dagEvalC(e)))
    numRoots.map { t =>
      exprVals.minBy { case (_, v) => (v - t).magnitude }._1
    }

  /** Approximate a Double as a small-denominator rational. */
  def approxRat(x: Double): Rational =
    if x.isNaN || x.isInfinite then Rational.zero
    else
      val candidates = (1 to 10000).view.flatMap { d =>
        val n = math.round(x * d)
        if math.abs(x - n.toDouble / d) < 1e-6 then
          Some((math.abs(x - n.toDouble / d), Rational(n.toInt, d)))
        else None
      }
      candidates.headOption match
        case Some((_, r)) => r
        case None => Rational(math.round(x).toInt)

  /** Convert a numerical value to a radical expression.
    * Tries common forms: rational, sqrt, cbrt, etc.
    */
  def numericToRadical(x: Double): RadExpr[Rational] =
    // Check if rational
    val asRational = (1 to 100).view.flatMap { d =>
      val n = math.round(x * d)
      if math.abs(x - n.toDouble / d) < 1e-10 then Some(Lit(Rational(n.toInt, d)))
      else None
    }.headOption

    asRational.getOrElse {
      // Check if x = p/q * sqrt(r) for small r
      val asSqrt = (for
        r <- (2 to 20).view
        sr = math.sqrt(r.toDouble)
        d <- (1 to 20).view
        n = math.round(x * d / sr)
        if math.abs(x - n.toDouble * sr / d) < 1e-10
      yield Mul(Lit(Rational(n.toInt, d)), Root(2, Lit(Rational(r))))).headOption

      asSqrt.getOrElse {
        // Check if x = p/q * cbrt(r)
        val asCbrt = (for
          r <- (2 to 10).view
          cr = math.cbrt(r.toDouble)
          d <- (1 to 10).view
          n = math.round(x * d / cr)
          if math.abs(x - n.toDouble * cr / d) < 1e-10
        yield Mul(Lit(Rational(n.toInt, d)), Root(3, Lit(Rational(r))))).headOption

        asCbrt.getOrElse {
          val n = math.round(x * 1000000)
          Mul(Inv(Lit(Rational(1000000))), Lit(Rational(n.toInt)))
        }
      }
    }
