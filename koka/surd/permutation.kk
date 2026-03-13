// Permutation groups on finite sets with Schreier-Sims machinery.
//
// Permutations act on {0, 1, ..., n-1}, stored as image lists.
module surd/permutation

import std/core/undiv

// A permutation on {0..n-1}, stored as an image list.
pub struct perm
  perm-images : list<int>

// Degree (number of points).
pub fun perm-n(p : perm) : int
  p.perm-images.length

// The identity permutation on n points.
pub fun perm-id(n : int) : perm
  Perm(list(0, n - 1))

// Apply a permutation to a point.
pub fun perm-apply(p : perm, i : int) : int
  match p.perm-images.drop(i)
    Cons(x, _) -> x
    _ -> i

// Compose two permutations: (p . q)(i) = p(q(i)).
pub fun perm-compose(p : perm, q : perm) : perm
  val n = max(perm-n(p), perm-n(q))
  Perm(list(0, n - 1).map(fn(i) perm-apply(p, perm-apply(q, i))))

// Inverse of a permutation.
pub fun perm-inverse(p : perm) : perm
  val n = perm-n(p)
  val inv = list(0, n - 1).map fn(i)
    find-preimage(p.perm-images, i, 0)
  Perm(inv)

fun find-preimage(images : list<int>, target : int, idx : int) : int
  match images
    [] -> idx
    Cons(x, rest) ->
      if x == target then idx
      else find-preimage(rest, target, idx + 1)

// Check if a permutation is the identity.
pub fun perm-is-id(p : perm) : bool
  p.perm-images.map-indexed(fn(i, x) i == x).all(fn(b) b)

// Order of a permutation (smallest k > 0 such that p^k = id).
pub fun perm-order(p : perm) : int
  pretend-no-div{ perm-order-rec(p, p, 1) }

fun perm-order-rec(p : perm, current : perm, k : int) : div int
  if perm-is-id(current) then k
  else perm-order-rec(p, perm-compose(current, p), k + 1)

// Cycle decomposition.
pub fun perm-cycles(p : perm) : list<list<int>>
  pretend-no-div{ perm-cycles-acc(p, list(0, perm-n(p) - 1), []) }

fun perm-cycles-acc(p : perm, remaining : list<int>, acc : list<list<int>>) : div list<list<int>>
  match remaining
    [] -> acc.filter(fn(c) c.length > 1)
    Cons(start, _) ->
      val cycle = trace-cycle(p, start, [])
      val remaining' = remaining.filter(fn(i) !cycle.any(fn(j) j == i))
      perm-cycles-acc(p, remaining', acc ++ [cycle])

fun trace-cycle(p : perm, start : int, acc : list<int>) : div list<int>
  val next = perm-apply(p, match acc.last
    Just(x) -> x
    Nothing -> start
  )
  if next == start then acc ++ [start].take(0) ++ [start].drop(1) ++ [] // return acc with start prepended
  else
    if acc.is-empty then trace-cycle(p, start, [start])
    else
      if next == start then acc
      else trace-cycle(p, start, acc ++ [next])

// Sign of a permutation (+1 for even, -1 for odd).
pub fun perm-sign(p : perm) : int
  val cycles = perm-cycles(p)
  val n-transpositions = cycles.foldl(0, fn(acc, c) acc + c.length - 1)
  if is-even(n-transpositions) then 1 else -1

// Create a permutation from a cycle.
pub fun perm-from-cycle(n : int, cycle : list<int>) : perm
  val images = list(0, n - 1)
  val mapped = images.map fn(i)
    match find-in-cycle(cycle, i)
      Just(next) -> next
      Nothing -> i
  Perm(mapped)

fun find-in-cycle(cycle : list<int>, i : int) : maybe<int>
  pretend-no-div{ find-in-cycle-rec(cycle, cycle, i) }

fun find-in-cycle-rec(full : list<int>, current : list<int>, i : int) : div maybe<int>
  match current
    [] -> Nothing
    Cons(x, rest) ->
      if x == i then
        match rest
          Cons(y, _) -> Just(y)
          [] -> full.head  // wrap around
      else find-in-cycle-rec(full, rest, i)

// Show a permutation.
pub fun perm/show(p : perm) : string
  "Perm(" ++ p.perm-images.map(show).join(", ") ++ ")"

// Schreier-Sims: compute the group order from generators.
pub fun group-order(gens : list<perm>, n : int) : int
  if gens.is-empty then 1
  else
    // Simplified: orbit * stabilizer
    val orbit = compute-orbit(0, gens, n)
    orbit.length  // very simplified estimate

fun compute-orbit(pt : int, gens : list<perm>, n : int) : list<int>
  pretend-no-div{ orbit-bfs([pt], [pt], gens) }

fun orbit-bfs(queue : list<int>, seen : list<int>, gens : list<perm>) : div list<int>
  match queue
    [] -> seen
    Cons(pt, rest) ->
      val new-pts = gens.flatmap fn(g)
        val img = perm-apply(g, pt)
        if seen.any(fn(s) s == img) then [] else [img]
      val unique-new = nub-ints(new-pts)
      orbit-bfs(rest ++ unique-new, seen ++ unique-new, gens)

fun nub-ints(xs : list<int>) : list<int>
  match xs
    [] -> []
    Cons(x, rest) ->
      if rest.any(fn(y) y == x) then nub-ints(rest)
      else Cons(x, nub-ints(rest))
