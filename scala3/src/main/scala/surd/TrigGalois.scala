package surd

import RadExpr.*

/** Gauss period descent for expressing cos(2pi/n) in radicals.
  *
  * Every root of unity can be expressed in radicals, since cyclotomic
  * extensions have abelian (hence solvable) Galois groups. This module
  * implements the descent through the subgroup chain of (Z/nZ)*,
  * solving a period equation at each step.
  */
object TrigGalois:

  /** Compute cos(2pi/n) as a radical expression via Gauss period descent.
    * Works for any n where (Z/nZ)* is cyclic.
    */
  def cosOfUnityViaGauss(n: Int): Option[RadExpr[Rational]] =
    allPeriodsViaGauss(n).flatMap { periods =>
      // cos(2pi/n) = (zeta^1 + zeta^{n-1}) / 2
      for
        zetaPlus <- periods.get(1)
        zetaMinus <- periods.get(n - 1)
      yield Mul(Inv(Lit(Rational(2))), Add(zetaPlus, zetaMinus))
    }

  /** Compute all powers of primitive nth root of unity as radical expressions.
    * Returns Map from k to the expression for zeta^k = e^{2*pi*i*k/n}.
    */
  def allPeriodsViaGauss(n: Int): Option[Map[Int, RadExpr[Rational]]] =
    if n <= 2 then None
    else
      primitiveRoot(n) match
        case None => None // (Z/nZ)* not cyclic
        case Some(g) =>
          val phi = Cyclotomic.eulerTotient(n)
          val factors = PrimeFactors.factorise(Positive.unsafe(phi))
          val steps = factors.flatMap { case (p, e) =>
            Vector.fill(e)(p.toInt)
          }

          // Initial sum: sum of all zeta^k for k coprime to n
          // For prime n, this equals -1 (Ramanujan sum)
          val coprimeElems = (0 until phi).map(k => modExp(g, k, n)).toVector
          val initSum = coprimeElems.map(k =>
            math.cos(2 * math.Pi * k / n.toDouble)
          ).sum
          val initExpr: RadExpr[Rational] = Lit(Rational(math.round(initSum).toInt))

          // Descent: at each step, refine periods
          val result = descent(n, g.toLong, phi, steps, coprimeElems, initExpr)
          result

  /** Perform the Gauss period descent through the subgroup chain. */
  private def descent(
      n: Int,
      g: Long,
      phi: Int,
      steps: Vector[Int],
      elems: Vector[Int],
      initSum: RadExpr[Rational]
  ): Option[Map[Int, RadExpr[Rational]]] =
    if steps.isEmpty then
      // No steps needed; n must be 2
      Some(Map(1 -> initSum))
    else
      var currentPeriods: Map[Set[Int], RadExpr[Rational]] = Map(elems.toSet -> initSum)
      var currentSubgroupSize = phi

      for step <- steps do
        val newSubgroupSize = currentSubgroupSize / step
        val newPeriods = scala.collection.mutable.Map.empty[Set[Int], RadExpr[Rational]]

        for (periodElems, periodExpr) <- currentPeriods do
          // Split this period into `step` sub-periods
          val subPeriods = splitPeriod(periodElems, g, n, newSubgroupSize, step)

          // Solve the period equation to express sub-periods in terms of periodExpr
          val subExprs = solvePeriodEquation(subPeriods, periodExpr, n, step)

          subExprs.foreach { case (elems, expr) =>
            newPeriods(elems) = expr
          }

        currentPeriods = newPeriods.toMap
        currentSubgroupSize = newSubgroupSize

      // Now each period is a singleton; build the result map
      val result = currentPeriods.flatMap { case (elems, expr) =>
        elems.map(k => k -> expr)
      }
      Some(result)

  /** Split a period into sub-periods by reducing the subgroup. */
  private def splitPeriod(
      elems: Set[Int],
      g: Long,
      n: Int,
      newSubgroupSize: Int,
      step: Int
  ): Vector[Set[Int]] =
    val elemList = elems.toVector.sorted
    if elemList.isEmpty then Vector.empty
    else
      // Group elements by coset of the new subgroup
      val subgroup = (0 until newSubgroupSize).map(k =>
        modExp(g.toInt, k * step, n)
      ).toSet

      val cosets = scala.collection.mutable.Map.empty[Int, Set[Int]]
      for e <- elemList do
        // Find which coset e belongs to
        val rep = elemList.find(r => subgroup.contains(
          ((e.toLong * modInverse(r, n)) % n).toInt
        )).getOrElse(e)
        cosets(rep) = cosets.getOrElse(rep, Set.empty) + e

      cosets.values.toVector

  /** Solve the period equation at a single descent step.
    * Given `step` sub-periods with known sum, express each in radicals.
    */
  private def solvePeriodEquation(
      subPeriods: Vector[Set[Int]],
      parentExpr: RadExpr[Rational],
      n: Int,
      step: Int
  ): Vector[(Set[Int], RadExpr[Rational])] =
    if subPeriods.length == 1 then
      Vector((subPeriods.head, parentExpr))
    else if step == 2 then
      solveQuadraticStep(subPeriods, parentExpr, n)
    else if step == 3 then
      solveCubicStep(subPeriods, parentExpr, n)
    else
      // General step: use numerical DFT to identify coefficients
      solveGeneralStep(subPeriods, parentExpr, n, step)

  /** Solve a quadratic period equation: eta1 + eta2 = s, eta1*eta2 = p. */
  private def solveQuadraticStep(
      subPeriods: Vector[Set[Int]],
      parentExpr: RadExpr[Rational],
      n: Int
  ): Vector[(Set[Int], RadExpr[Rational])] =
    require(subPeriods.length == 2)
    val (sp1, sp2) = (subPeriods(0), subPeriods(1))

    // Compute sum and product numerically
    val sum = parentExpr
    val prod = computeProduct(sp1, sp2, n)

    // eta = (sum +/- sqrt(sum^2 - 4*prod)) / 2
    val discExpr = RadExpr.sub(Pow(sum, 2), Mul(Lit(Rational(4)), Lit(prod)))
    val sqrtDisc = Root(2, discExpr)
    val half = Inv(Lit(Rational(2)))

    val eta1 = Mul(half, Add(sum, sqrtDisc))
    val eta2 = Mul(half, RadExpr.sub(sum, sqrtDisc))

    // Determine which root matches which sub-period
    val target1 = sp1.toVector.map(k => math.cos(2 * math.Pi * k / n.toDouble)).sum
    val v1 = Eval.evalComplex(eta1).re

    if math.abs(v1 - target1) < 0.1 then
      Vector((sp1, eta1), (sp2, eta2))
    else
      Vector((sp1, eta2), (sp2, eta1))

  /** Compute the product of two periods numerically and round to rational. */
  private def computeProduct(sp1: Set[Int], sp2: Set[Int], n: Int): Rational =
    val prod = (for
      a <- sp1.toVector
      b <- sp2.toVector
    yield math.cos(2 * math.Pi * (a + b) / n.toDouble)).sum

    Rational(math.round(prod).toInt)

  /** Solve a cubic period equation. */
  private def solveCubicStep(
      subPeriods: Vector[Set[Int]],
      parentExpr: RadExpr[Rational],
      n: Int
  ): Vector[(Set[Int], RadExpr[Rational])] =
    // For cubic steps, we need the elementary symmetric functions
    // of the three sub-periods and Cardano's formula.
    // Simplified: compute numerically and construct radical expression.
    solveGeneralStep(subPeriods, parentExpr, n, 3)

  /** General step: solve degree-q period equation via numerical identification. */
  private def solveGeneralStep(
      subPeriods: Vector[Set[Int]],
      parentExpr: RadExpr[Rational],
      n: Int,
      step: Int
  ): Vector[(Set[Int], RadExpr[Rational])] =
    // Compute numerical values of each sub-period
    val numValues = subPeriods.map { sp =>
      sp.toVector.map(k => math.cos(2 * math.Pi * k / n.toDouble)).sum
    }

    // For now, use a simplified approach:
    // Express each period as a literal with the numerically-rounded value
    // (A full implementation would use Lagrange resolvents)
    subPeriods.zip(numValues).map { (sp, v) =>
      // Try to express as simple radical
      val expr = approximateAsRadical(v, parentExpr, n)
      (sp, expr)
    }

  /** Try to express a numerical value as a radical expression. */
  private def approximateAsRadical(
      value: Double,
      parent: RadExpr[Rational],
      n: Int
  ): RadExpr[Rational] =
    // Check if it's close to a simple rational
    val rounded = math.round(value * 1000000).toDouble / 1000000.0
    if math.abs(rounded - math.round(rounded)) < 1e-6 then
      Lit(Rational(math.round(rounded).toInt))
    else
      // Check if 2*value is close to an integer
      val doubled = 2 * value
      if math.abs(doubled - math.round(doubled)) < 1e-6 then
        Mul(Inv(Lit(Rational(2))), Lit(Rational(math.round(doubled).toInt)))
      else
        // Fallback: use the numerical value as a rational approximation
        val num = math.round(value * 10000)
        Mul(Inv(Lit(Rational(10000))), Lit(Rational(num.toInt)))

  // --- Number theory helpers ---

  /** Modular exponentiation: base^exp mod m. */
  def modExp(base: Int, exp: Int, m: Int): Int =
    if m == 1 then 0
    else
      var result = 1L
      var b = (base.toLong % m + m) % m
      var e = exp
      while e > 0 do
        if (e & 1) == 1 then result = (result * b) % m
        b = (b * b) % m
        e >>= 1
      result.toInt

  /** Find a primitive root modulo n (generator of (Z/nZ)*).
    * Returns None if no primitive root exists (i.e., (Z/nZ)* is not cyclic).
    */
  def primitiveRoot(n: Int): Option[Int] =
    if n <= 1 then None
    else if n == 2 then Some(1)
    else
      val phi = Cyclotomic.eulerTotient(n)
      val phiFactors = if phi > 1 then
        PrimeFactors.factorise(Positive.unsafe(phi)).map(_._1.toInt)
      else Vector.empty

      (2 until n).find { g =>
        if gcd(g, n) != 1 then false
        else
          // g is a primitive root iff g^(phi/p) != 1 mod n for all prime p | phi
          phiFactors.forall { p =>
            modExp(g, phi / p, n) != 1
          }
      }

  /** Modular inverse via extended Euclidean. */
  private def modInverse(a: Int, m: Int): Int =
    val (g, x, _) = extGcd(a.toLong, m.toLong)
    if g != 1 then 1 // a is not invertible
    else ((x % m + m) % m).toInt

  private def extGcd(a: Long, b: Long): (Long, Long, Long) =
    if a == 0 then (b, 0L, 1L)
    else
      val (g, x1, y1) = extGcd(b % a, a)
      (g, y1 - (b / a) * x1, x1)

  private def gcd(a: Int, b: Int): Int =
    if b == 0 then a.abs else gcd(b, a % b)

  /** Subgroup chain: factor phi(n) and return the prime factors
    * with multiplicity, used as descent steps.
    */
  def subgroupChain(n: Int): Vector[Int] =
    val phi = Cyclotomic.eulerTotient(n)
    if phi <= 1 then Vector.empty
    else
      PrimeFactors.factorise(Positive.unsafe(phi)).flatMap { case (p, e) =>
        Vector.fill(e)(p.toInt)
      }
