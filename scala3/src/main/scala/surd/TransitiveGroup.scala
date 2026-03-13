package surd

/** Database of transitive subgroups of S_n for small n.
  *
  * For Galois group identification, we need the lattice of transitive
  * subgroups with generators, orders, solvability flags, and
  * maximal-supergroup relationships.
  *
  * Numbering follows the Butler-McKay convention (also used in GAP/Magma/LMFDB).
  */
object TransitiveGroup:

  /** A transitive group record. */
  final case class TGroup(
      degree: Int,
      index: Int,
      name: String,
      order: Int,
      solvable: Boolean,
      generators: Vector[Perm]
  )

  // --- Degree 5 transitive groups ---

  /** C5 (T1): cyclic group of order 5. */
  val c5: TGroup = TGroup(
    degree = 5, index = 1, name = "C5", order = 5, solvable = true,
    generators = Vector(Perm.cycle(5, 0, 1, 2, 3, 4))
  )

  /** D5 (T2): dihedral group of order 10. */
  val d5: TGroup = TGroup(
    degree = 5, index = 2, name = "D5", order = 10, solvable = true,
    generators = Vector(
      Perm.cycle(5, 0, 1, 2, 3, 4),
      Perm(Vector(0, 4, 3, 2, 1)) // reflection
    )
  )

  /** F20 (T3): Frobenius group of order 20. Z/5 ⋊ Z/4. */
  val f20: TGroup = TGroup(
    degree = 5, index = 3, name = "F20", order = 20, solvable = true,
    generators = Vector(
      Perm.cycle(5, 0, 1, 2, 3, 4),
      Perm(Vector(0, 2, 4, 1, 3)) // (1 2 4 3) in 1-indexed
    )
  )

  /** A5 (T4): alternating group of order 60. */
  val a5: TGroup = TGroup(
    degree = 5, index = 4, name = "A5", order = 60, solvable = false,
    generators = Vector(
      Perm.cycle(5, 0, 1, 2, 3, 4),
      Perm.cycle(5, 0, 1, 2)
    )
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
