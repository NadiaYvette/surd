// Transitive subgroups of S5, used for Galois group identification
// of quintic polynomials.
//
// The five transitive subgroups of S5 are:
// Z/5Z (cyclic, order 5), D5 (dihedral, order 10),
// F20 (Frobenius, order 20), A5 (alternating, order 60),
// S5 (symmetric, order 120).
//
// Z/5Z, D5, and F20 are solvable; A5 and S5 are not.
module surd/transitive-group

import surd/permutation

// Enumeration of transitive subgroups of S5.
pub type transitive-group5
  Z5      // Cyclic group of order 5
  D5      // Dihedral group of order 10
  F20     // Frobenius group of order 20
  A5Group // Alternating group of order 60
  S5Group // Symmetric group of order 120

// Check if a transitive subgroup of S5 is solvable.
pub fun is-solvable(g : transitive-group5) : bool
  match g
    Z5  -> True
    D5  -> True
    F20 -> True
    A5Group -> False
    S5Group -> False

// Order of the group.
pub fun group-order5(g : transitive-group5) : int
  match g
    Z5  -> 5
    D5  -> 10
    F20 -> 20
    A5Group -> 60
    S5Group -> 120

// Generators for each transitive subgroup of S5.
pub fun generators(g : transitive-group5) : list<perm>
  match g
    Z5 ->
      // (0 1 2 3 4)
      [perm-from-cycle(5, [0, 1, 2, 3, 4])]
    D5 ->
      // (0 1 2 3 4) and (1 4)(2 3)
      val r = perm-from-cycle(5, [0, 1, 2, 3, 4])
      val s = Perm([0, 4, 3, 2, 1])
      [r, s]
    F20 ->
      // (0 1 2 3 4) and (1 2 4 3)
      val r = perm-from-cycle(5, [0, 1, 2, 3, 4])
      val s = Perm([0, 2, 4, 1, 3])
      [r, s]
    A5Group ->
      // (0 1 2 3 4) and (0 1 2)
      val r = perm-from-cycle(5, [0, 1, 2, 3, 4])
      val s = perm-from-cycle(5, [0, 1, 2])
      [r, s]
    S5Group ->
      // (0 1 2 3 4) and (0 1)
      val r = perm-from-cycle(5, [0, 1, 2, 3, 4])
      val s = perm-from-cycle(5, [0, 1])
      [r, s]

// Show the group name.
pub fun transitive-group5/show(g : transitive-group5) : string
  match g
    Z5  -> "Z/5Z"
    D5  -> "D5"
    F20 -> "F20"
    A5Group -> "A5"
    S5Group -> "S5"
