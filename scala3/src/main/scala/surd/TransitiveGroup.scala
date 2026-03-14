package surd

/** Database of transitive subgroups of S_n for small n.
  *
  * For Galois group identification, we need the lattice of transitive
  * subgroups with generators, orders, solvability flags, and
  * maximal-supergroup relationships.
  *
  * Numbering follows the Butler-McKay convention (also used in GAP/Magma/LMFDB).
  *
  * For prime p, transitive subgroups are computed at runtime from the
  * structure of AGL(1,p). The solvable subgroups are Z/p ⋊ H for each
  * divisor d of p-1, plus A_p and S_p (non-solvable for p >= 5).
  */
object TransitiveGroup:

  /** A transitive group record. */
  final case class TGroup(
      degree: Int,
      index: Int,
      name: String,
      order: Long,
      solvable: Boolean,
      generators: Vector[Perm],
      maximalSupergroups: Vector[Int] = Vector.empty,
      compositionFactors: Vector[Int] = Vector.empty
  )

  // --- Degree 5 transitive groups (hard-coded fast path) ---

  /** C5 (T1): cyclic group of order 5. */
  val c5: TGroup = TGroup(
    degree = 5, index = 1, name = "C5", order = 5, solvable = true,
    generators = Vector(Perm.cycle(5, 0, 1, 2, 3, 4)),
    maximalSupergroups = Vector(1),
    compositionFactors = Vector(5)
  )

  /** D5 (T2): dihedral group of order 10. */
  val d5: TGroup = TGroup(
    degree = 5, index = 2, name = "D5", order = 10, solvable = true,
    generators = Vector(
      Perm.cycle(5, 0, 1, 2, 3, 4),
      Perm(Vector(0, 4, 3, 2, 1)) // reflection
    ),
    maximalSupergroups = Vector(2),
    compositionFactors = Vector(5, 2)
  )

  /** F20 (T3): Frobenius group of order 20. Z/5 ⋊ Z/4. */
  val f20: TGroup = TGroup(
    degree = 5, index = 3, name = "F20", order = 20, solvable = true,
    generators = Vector(
      Perm.cycle(5, 0, 1, 2, 3, 4),
      Perm(Vector(0, 2, 4, 1, 3)) // (1 2 4 3) in 1-indexed
    ),
    maximalSupergroups = Vector(3, 4),
    compositionFactors = Vector(5, 2, 2)
  )

  /** A5 (T4): alternating group of order 60. */
  val a5: TGroup = TGroup(
    degree = 5, index = 4, name = "A5", order = 60, solvable = false,
    generators = Vector(
      Perm.cycle(5, 0, 1, 2, 3, 4),
      Perm.cycle(5, 0, 1, 2)
    ),
    maximalSupergroups = Vector(4)
  )

  /** S5 (T5): symmetric group of order 120. */
  val s5: TGroup = TGroup(
    degree = 5, index = 5, name = "S5", order = 120, solvable = false,
    generators = Vector(
      Perm.cycle(5, 0, 1, 2, 3, 4),
      Perm.transposition(5, 0, 1)
    )
  )

  /** All degree-5 transitive groups, ordered by index. */
  val degree5Groups: Vector[TGroup] = Vector(c5, d5, f20, a5, s5)

  /** Look up a degree-5 transitive group by name. */
  def byName5(name: String): Option[TGroup] =
    degree5Groups.find(_.name == name)

  // --- Degree 4 transitive groups ---

  /** C4 (T1): cyclic group of order 4. */
  val c4: TGroup = TGroup(
    degree = 4, index = 1, name = "C4", order = 4, solvable = true,
    generators = Vector(Perm.cycle(4, 0, 1, 2, 3))
  )

  /** V4 (T2): Klein four-group. */
  val v4: TGroup = TGroup(
    degree = 4, index = 2, name = "V4", order = 4, solvable = true,
    generators = Vector(
      Perm(Vector(1, 0, 3, 2)),
      Perm(Vector(2, 3, 0, 1))
    )
  )

  /** D4 (T3): dihedral group of order 8. */
  val d4: TGroup = TGroup(
    degree = 4, index = 3, name = "D4", order = 8, solvable = true,
    generators = Vector(
      Perm.cycle(4, 0, 1, 2, 3),
      Perm(Vector(0, 3, 2, 1))
    )
  )

  /** A4 (T4): alternating group of order 12. */
  val a4: TGroup = TGroup(
    degree = 4, index = 4, name = "A4", order = 12, solvable = true,
    generators = Vector(
      Perm.cycle(4, 0, 1, 2),
      Perm(Vector(1, 0, 3, 2))
    )
  )

  /** S4 (T5): symmetric group of order 24. */
  val s4: TGroup = TGroup(
    degree = 4, index = 5, name = "S4", order = 24, solvable = true,
    generators = Vector(
      Perm.cycle(4, 0, 1, 2, 3),
      Perm.transposition(4, 0, 1)
    )
  )

  /** All degree-4 transitive groups. */
  val degree4Groups: Vector[TGroup] = Vector(c4, v4, d4, a4, s4)

  // -----------------------------------------------------------------------
  // General prime-degree transitive groups (runtime computation)
  // -----------------------------------------------------------------------

  /** All transitive subgroups of S_n (up to conjugacy) for the given degree.
    *
    * For degree 5, returns the hard-coded database.
    * For other primes p >= 3, computes at runtime from AGL(1,p) structure.
    * For unsupported composite degrees, returns empty.
    */
  def transGroupsOfDegree(n: Int): Vector[TGroup] =
    if n == 5 then degree5Groups
    else if n >= 3 && PrimeFactors.isPrime(n) then transGroupsOfPrimeRT(n)
    else Vector.empty

  /** Find transitive group(s) of the given degree and order. */
  def transGroupByOrder(deg: Int, ord: Long): Vector[TGroup] =
    transGroupsOfDegree(deg).filter(_.order == ord)

  /** Modular exponentiation: base^exp mod m. */
  def modExp(base: Long, exp: Long, m: Long): Long =
    if exp == 0 then 1L
    else if exp % 2 == 0 then
      val half = modExp(base, exp / 2, m)
      (half * half) % m
    else (base % m * modExp(base, exp - 1, m)) % m

  /** Primitive root modulo a prime p. */
  def primitiveRootP(p: Long): Long =
    val phi = p - 1
    val factors = primeFactorsFlat(phi).distinct
    (2L until p).find { g =>
      factors.forall(q => modExp(g, phi / q, p) != 1L)
    }.getOrElse(2L)

  /** Sorted positive divisors of n. */
  def divisors(n: Long): Vector[Long] =
    val isqrt = math.sqrt(n.toDouble).toLong
    val small = (1L to isqrt).filter(d => n % d == 0)
    val all = small.flatMap(d => if d * d == n then Vector(d) else Vector(d, n / d))
    all.distinct.sorted.toVector

  /** Prime factors with multiplicity (flat list). */
  private def primeFactorsFlat(n: Long): Vector[Long] =
    if n <= 1 then Vector.empty
    else
      val pos = Positive.unsafe(n.toInt)
      PrimeFactors.factorise(pos).flatMap { case (p, e) =>
        Vector.fill(e)(p.toLong)
      }

  /** Build all transitive groups for prime p at runtime.
    *
    * Solvable groups: Z/p ⋊ H for each divisor d of p-1.
    * Non-solvable: A_p, S_p.
    * Maximal supergroup relationships are computed from divisibility.
    */
  private def transGroupsOfPrimeRT(p: Int): Vector[TGroup] =
    val pL = p.toLong
    val g = primitiveRootP(pL)
    val ds = divisors(pL - 1)

    // Build one solvable group per divisor of p-1
    val solvableGroups = ds.map { d =>
      val trans = Perm(Vector.tabulate(p)(i => (i + 1) % p))
      val scaleFactor = modExp(g, (pL - 1) / d, pL)
      val scale = Perm(Vector.tabulate(p)(i => ((scaleFactor * i) % pL).toInt))
      val gens = if d == 1 then Vector(trans) else Vector(trans, scale)
      val gName =
        if d == 1 then s"Z$p"
        else if d == 2 then s"D$p"
        else if d == pL - 1 then s"AGL(1,$p)"
        else s"Z$p:Z$d"
      val cFactors = primeFactorsFlat(d).map(_.toInt) :+ p
      TGroup(
        degree = p, index = 0, name = gName, order = pL * d,
        solvable = true, generators = gens,
        compositionFactors = cFactors
      )
    }

    // A_p
    val apOrder = factorial(pL) / 2
    val ap = TGroup(
      degree = p, index = 0, name = s"A$p", order = apOrder,
      solvable = p < 5,
      generators = Vector(
        Perm.cycle(p, (0 until p)*),
        Perm.cycle(p, 0, 1, 2)
      )
    )

    // S_p
    val spOrder = factorial(pL)
    val sp = TGroup(
      degree = p, index = 0, name = s"S$p", order = spOrder,
      solvable = p < 4,
      generators = Vector(
        Perm.cycle(p, (0 until p)*),
        Perm.transposition(p, 0, 1)
      )
    )

    val allGroups = (solvableGroups :+ ap :+ sp).sortBy(_.order)

    // Assign indices and maximal supergroups
    val indexed = allGroups.zipWithIndex
    indexed.map { case (tg, myIdx) =>
      val myOrd = tg.order
      val cands = indexed.filter { case (cg, i) =>
        i != myIdx && cg.order > myOrd && cg.order % myOrd == 0
      }
      val maxSupers = cands.filter { case (_, superIdx) =>
        val superOrd = allGroups(superIdx).order
        !cands.exists { case (midG, _) =>
          val midOrd = midG.order
          midOrd > myOrd && midOrd < superOrd &&
            superOrd % midOrd == 0 && midOrd % myOrd == 0
        }
      }.map(_._2)
      tg.copy(index = myIdx + 1, maximalSupergroups = maxSupers)
    }

  /** Compute n! (for moderate n). */
  private def factorial(n: Long): Long =
    (1L to n).product

  // -----------------------------------------------------------------------
  // Composition series
  // -----------------------------------------------------------------------

  /** For a solvable transitive group, return the composition series as a
    * list of generating sets, descending from G to the trivial group {1}.
    *
    * Each consecutive quotient G_i / G_{i+1} is cyclic of prime order
    * (matching the composition factors). This chain drives radical tower
    * descent.
    *
    * Returns None for non-solvable groups.
    */
  def compositionSeries(tg: TGroup): Option[Vector[Vector[Perm]]] =
    if !tg.solvable then None
    else tg.name match
      // Fast path: degree-5 hard-coded
      case "C5" => Some(Vector(tg.generators, Vector.empty))
      case "D5" => Some(Vector(
        tg.generators,
        Vector(Perm.cycle(5, 0, 1, 2, 3, 4)),
        Vector.empty
      ))
      case "F20" => Some(Vector(
        tg.generators,
        Vector(Perm.cycle(5, 0, 1, 2, 3, 4), Perm(Vector(0, 4, 3, 2, 1))),
        Vector(Perm.cycle(5, 0, 1, 2, 3, 4)),
        Vector.empty
      ))
      case _ if PrimeFactors.isPrime(tg.degree) =>
        compositionSeriesPrime(tg)
      case _ => None

  /** Composition series for a solvable affine subgroup of S_p.
    *
    * The group has the form Z/p ⋊ H where H is cyclic of order d.
    * The series descends through subgroups of H by removing one
    * prime factor at a time, then drops to {1}.
    */
  private def compositionSeriesPrime(tg: TGroup): Option[Vector[Vector[Perm]]] =
    val p = tg.degree.toLong
    val n = tg.degree
    val g = primitiveRootP(p)
    val d = tg.order / p
    val dFactors = primeFactorsFlat(d)
    // Chain of divisors: d, d/q1, d/(q1*q2), ..., 1
    val dChain = dFactors.scanLeft(d)((acc, q) => acc / q)
    val trans = Perm.cycle(n, (0 until n)*)

    def mkGens(dPrime: Long): Vector[Perm] =
      if dPrime <= 1 then Vector(trans)
      else
        val sf = modExp(g, (p - 1) / dPrime, p)
        val scale = Perm(Vector.tabulate(n)(i => ((sf * i) % p).toInt))
        Vector(trans, scale)

    Some(dChain.map(mkGens) :+ Vector.empty)
