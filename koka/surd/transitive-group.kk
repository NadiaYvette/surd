// Transitive subgroups of S_n for Galois group identification.
//
// For prime p, the solvable transitive subgroups are exactly the
// affine groups Z/p ⋊ H where H is a cyclic subgroup of (Z/pZ)*,
// one per divisor d of p-1. Plus A_p and S_p (non-solvable for p >= 5).
//
// Degree-5 fast path retained for backward compatibility.
module surd/transitive-group

import std/core/undiv
import surd/permutation
import surd/positive
import surd/prime-factors

// A transitive subgroup of S_n, with metadata for identification
// and radical tower construction.
pub struct transitive-group
  tg-name : string             // Human-readable name (e.g. "Z7", "D7", "AGL(1,7)")
  tg-degree : int              // Degree n (number of roots)
  tg-order : int               // Group order |G|
  tg-generators : list<perm>   // Generating permutations
  tg-solvable : bool           // Solvable => polynomial is solvable by radicals
  tg-max-supergroups : list<int> // Indices of maximal supergroups (Hasse diagram)
  tg-composition-factors : list<int> // Prime orders of composition series quotients

// Check if a transitive group is solvable.
pub fun is-solvable(g : transitive-group) : bool
  g.tg-solvable

// Order of the group.
pub fun group-order-tg(g : transitive-group) : int
  g.tg-order

// Show the group name.
pub fun transitive-group/show(g : transitive-group) : string
  g.tg-name

// ---------------------------------------------------------------
// Degree-5 fast path (backward compatibility)
// ---------------------------------------------------------------

// Enumeration of transitive subgroups of S5 (legacy).
pub type transitive-group5
  Z5      // Cyclic group of order 5
  D5      // Dihedral group of order 10
  F20     // Frobenius group of order 20
  A5Group // Alternating group of order 60
  S5Group // Symmetric group of order 120

// Check if a transitive subgroup of S5 is solvable (legacy).
pub fun transitive-group5/is-solvable(g : transitive-group5) : bool
  match g
    Z5  -> True
    D5  -> True
    F20 -> True
    A5Group -> False
    S5Group -> False

// Order of the group (legacy).
pub fun group-order5(g : transitive-group5) : int
  match g
    Z5  -> 5
    D5  -> 10
    F20 -> 20
    A5Group -> 60
    S5Group -> 120

// Generators for each transitive subgroup of S5 (legacy).
pub fun transitive-group5/generators(g : transitive-group5) : list<perm>
  match g
    Z5 ->
      [perm-from-cycle(5, [0, 1, 2, 3, 4])]
    D5 ->
      val r = perm-from-cycle(5, [0, 1, 2, 3, 4])
      val s = Perm([0, 4, 3, 2, 1])
      [r, s]
    F20 ->
      val r = perm-from-cycle(5, [0, 1, 2, 3, 4])
      val s = Perm([0, 2, 4, 1, 3])
      [r, s]
    A5Group ->
      val r = perm-from-cycle(5, [0, 1, 2, 3, 4])
      val s = perm-from-cycle(5, [0, 1, 2])
      [r, s]
    S5Group ->
      val r = perm-from-cycle(5, [0, 1, 2, 3, 4])
      val s = perm-from-cycle(5, [0, 1])
      [r, s]

// Show the group name (legacy).
pub fun transitive-group5/show(g : transitive-group5) : string
  match g
    Z5  -> "Z/5Z"
    D5  -> "D5"
    F20 -> "F20"
    A5Group -> "A5"
    S5Group -> "S5"

// ---------------------------------------------------------------
// General prime degree: runtime AGL(1,p) computation
// ---------------------------------------------------------------

// All transitive subgroups of S_n (up to conjugacy) for degree n.
// For prime n, computed at runtime from AGL(1,n) structure.
// For degree 5, uses the hard-coded database as a fast path.
pub fun trans-groups-of-degree(n : int) : list<transitive-group>
  if n == 5 then degree5-groups()
  elif n >= 3 && is-prime(n) then trans-groups-of-prime(n)
  else []

// Find transitive group(s) of the given degree and order.
pub fun trans-group-by-order(deg : int, ord : int) : list<transitive-group>
  trans-groups-of-degree(deg).filter(fn(g) g.tg-order == ord)

// Hard-coded degree-5 transitive groups.
fun degree5-groups() : list<transitive-group>
  val c5 = Transitive-group(
    tg-name = "C5",
    tg-degree = 5,
    tg-order = 5,
    tg-generators = [perm-from-cycle(5, [0, 1, 2, 3, 4])],
    tg-solvable = True,
    tg-max-supergroups = [1],
    tg-composition-factors = [5]
  )
  val d5 = Transitive-group(
    tg-name = "D5",
    tg-degree = 5,
    tg-order = 10,
    tg-generators = [
      perm-from-cycle(5, [0, 1, 2, 3, 4]),
      Perm([0, 4, 3, 2, 1])
    ],
    tg-solvable = True,
    tg-max-supergroups = [2],
    tg-composition-factors = [5, 2]
  )
  val f20 = Transitive-group(
    tg-name = "F20",
    tg-degree = 5,
    tg-order = 20,
    tg-generators = [
      perm-from-cycle(5, [0, 1, 2, 3, 4]),
      Perm([0, 2, 4, 1, 3])
    ],
    tg-solvable = True,
    tg-max-supergroups = [3, 4],
    tg-composition-factors = [5, 2, 2]
  )
  val a5 = Transitive-group(
    tg-name = "A5",
    tg-degree = 5,
    tg-order = 60,
    tg-generators = [
      perm-from-cycle(5, [0, 1, 2, 3, 4]),
      perm-from-cycle(5, [0, 1, 2])
    ],
    tg-solvable = False,
    tg-max-supergroups = [4],
    tg-composition-factors = []
  )
  val s5 = Transitive-group(
    tg-name = "S5",
    tg-degree = 5,
    tg-order = 120,
    tg-generators = [
      perm-from-cycle(5, [0, 1, 2, 3, 4]),
      perm-from-cycle(5, [0, 1])
    ],
    tg-solvable = False,
    tg-max-supergroups = [],
    tg-composition-factors = []
  )
  [c5, d5, f20, a5, s5]

// Modular exponentiation (internal).
fun mod-exp-rt(base : int, exp : int, modulus : int) : int
  pretend-no-div{ mod-exp-rt-rec(base, exp, modulus) }

fun mod-exp-rt-rec(b : int, e : int, m : int) : div int
  if e == 0 then 1
  elif is-even(e) then
    val half = mod-exp-rt-rec(b, e / 2, m)
    (half * half) % m
  else (b * mod-exp-rt-rec(b, e - 1, m)) % m

// Primitive root modulo a prime (internal).
fun primitive-root-rt(p : int) : int
  pretend-no-div{ find-prim-root-rt(p, 2) }

fun find-prim-root-rt(p : int, g : int) : div int
  if g >= p then 1 // should not happen for primes
  else
    val phi = p - 1
    match positive(phi)
      Nothing -> g
      Just(pphi) ->
        val fs = factorise(pphi)
        val is-prim = fs.all(fn(f) mod-exp-rt(g, phi / f.fst, p) != 1)
        if is-prim then g
        else find-prim-root-rt(p, g + 1)

// Sorted positive divisors of n.
fun divisors-rt(n : int) : list<int>
  if n <= 0 then [1]
  else
    val small = list(1, n).filter(fn(d) n % d == 0)
    small

// Prime factorisation as a flat list with multiplicity.
fun prime-factors-with-mult(n : int) : list<int>
  if n <= 1 then []
  else match positive(n)
    Nothing -> []
    Just(pn) ->
      factorise(pn).flatmap(fn(pair) list(1, pair.snd).map(fn(_) pair.fst))

// Factorial.
fun factorial(n : int) : int
  if n <= 1 then 1
  else list(2, n).foldl(1, fn(acc, x) acc * x)

// All transitive subgroups of S_p for a prime p, computed at runtime.
//
// Solvable subgroups: Z/p ⋊ H for each divisor d of p-1.
// Plus A_p and S_p (non-solvable for p >= 5).
fun trans-groups-of-prime(p : int) : list<transitive-group>
  val g = primitive-root-rt(p)
  val ds = divisors-rt(p - 1)

  // Build one solvable group per divisor of p-1
  val solvable-groups = ds.map fn(d)
    val trans = Perm(list(0, p - 1).map(fn(i) (i + 1) % p))
    val scale-factor = mod-exp-rt(g, (p - 1) / d, p)
    val scale = Perm(list(0, p - 1).map(fn(i) (scale-factor * i) % p))
    val gens = if d == 1 then [trans] else [trans, scale]
    val gname = if d == 1 then "Z" ++ p.show
                elif d == 2 then "D" ++ p.show
                elif d == p - 1 then "AGL(1," ++ p.show ++ ")"
                else "Z" ++ p.show ++ ":Z" ++ d.show
    val cfactors = prime-factors-with-mult(d) ++ [p]
    Transitive-group(
      tg-name = gname,
      tg-degree = p,
      tg-order = p * d,
      tg-generators = gens,
      tg-solvable = True,
      tg-max-supergroups = [],
      tg-composition-factors = cfactors
    )

  // A_p
  val ap = Transitive-group(
    tg-name = "A" ++ p.show,
    tg-degree = p,
    tg-order = factorial(p) / 2,
    tg-generators = [
      perm-from-cycle(p, list(0, p - 1)),
      perm-from-cycle(p, [0, 1, 2])
    ],
    tg-solvable = p < 5,
    tg-max-supergroups = [],
    tg-composition-factors = []
  )

  // S_p
  val sp = Transitive-group(
    tg-name = "S" ++ p.show,
    tg-degree = p,
    tg-order = factorial(p),
    tg-generators = [
      perm-from-cycle(p, list(0, p - 1)),
      perm-from-cycle(p, [0, 1])
    ],
    tg-solvable = p < 4,
    tg-max-supergroups = [],
    tg-composition-factors = []
  )

  // Sort by order, then assign maximal supergroups
  val all-groups = sort-by-order(solvable-groups ++ [ap, sp])
  assign-supergroups(all-groups)

// Sort groups by order (merge sort).
fun sort-by-order(gs : list<transitive-group>) : list<transitive-group>
  pretend-no-div{ merge-sort-tg(gs) }

fun merge-sort-tg(xs : list<transitive-group>) : div list<transitive-group>
  match xs
    [] -> []
    [_] -> xs
    _ ->
      val n = xs.length / 2
      val left = xs.take(n)
      val right = xs.drop(n)
      merge-tg(merge-sort-tg(left), merge-sort-tg(right))

fun merge-tg(xs : list<transitive-group>, ys : list<transitive-group>) : list<transitive-group>
  match xs
    Nil -> ys
    Cons(x, xs') -> match ys
      Nil -> xs
      Cons(y, ys') ->
        if x.tg-order > y.tg-order then Cons(y, merge-tg(xs, ys'))
        else Cons(x, merge-tg(xs', ys))

// Assign maximal supergroups based on divisibility of orders.
fun assign-supergroups(gs : list<transitive-group>) : list<transitive-group>
  val indexed = gs.map-indexed(fn(i, g) (i, g))
  indexed.map fn(pair)
    val my-idx = pair.fst
    val tg = pair.snd
    val my-ord = tg.tg-order
    // Find groups whose order is a multiple of ours
    val cands = indexed.filter fn(other)
      val j = other.fst
      val cg = other.snd
      j != my-idx && cg.tg-order > my-ord && cg.tg-order % my-ord == 0
    // Among candidates, keep only maximal (no intermediate group between us and them)
    val maximal = cands.filter fn(cand)
      val super-ord = cand.snd.tg-order
      !cands.any fn(mid)
        val mid-ord = mid.snd.tg-order
        mid-ord > my-ord && mid-ord < super-ord &&
        super-ord % mid-ord == 0 && mid-ord % my-ord == 0
    tg(tg-max-supergroups = maximal.map(fst))

// ---------------------------------------------------------------
// Composition series
// ---------------------------------------------------------------

// For a solvable transitive group, return the composition series
// as a list of generator sets, descending from G to {1}.
// Returns Nothing for non-solvable groups.
pub fun composition-series(tg : transitive-group) : maybe<list<list<perm>>>
  if !tg.tg-solvable then Nothing
  else
    val p = tg.tg-degree
    if is-prime(p) then composition-series-prime(tg)
    else Nothing

// Composition series for a solvable affine subgroup of S_p.
// G = Z/p ⋊ H where H is cyclic of order d.
// Chain: G, ..., Z/p, {1}.
fun composition-series-prime(tg : transitive-group) : maybe<list<list<perm>>>
  val p = tg.tg-degree
  val g = primitive-root-rt(p)
  val d = tg.tg-order / p
  val d-factors = prime-factors-with-mult(d)
  // Chain of divisors: d, d/q1, d/(q1*q2), ..., 1
  val d-chain = pretend-no-div{ scan-divs(d, d-factors) }
  val trans = perm-from-cycle(p, list(0, p - 1))
  val gens-list = d-chain.map fn(d')
    if d' <= 1 then [trans]
    else
      val sf = mod-exp-rt(g, (p - 1) / d', p)
      val scale = Perm(list(0, p - 1).map(fn(i) (sf * i) % p))
      [trans, scale]
  Just(gens-list ++ [[]])

fun scan-divs(current : int, factors : list<int>) : div list<int>
  match factors
    [] -> [current]
    Cons(q, rest) -> Cons(current, scan-divs(current / q, rest))
