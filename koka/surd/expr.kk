// Structural queries on radical expressions.
//
// depth, node-count, free-of, collect-radicals, topo-sort-radicals.
// Note: depth and node-count are already defined in rad-expr.kk,
// but we provide them here as well for module completeness.
module surd/expr

import std/core/undiv
import surd/rational
import surd/rad-expr

// Nesting depth of the expression tree (re-exported for convenience).
pub fun expr/depth(e : rad-expr<k>) : div int
  surd/rad-expr/depth(e)

// Number of nodes in the expression tree (re-exported for convenience).
pub fun expr/node-count(e : rad-expr<k>) : div int
  surd/rad-expr/node-count(e)

// Check if the expression is free of any coefficient satisfying a predicate.
// Returns True if ALL coefficients satisfy the predicate p.
pub fun free-of(e : rad-expr<k>, p : (k) -> bool) : div bool
  match e
    Lit(k)     -> p(k)
    Neg(a)     -> free-of(a, p)
    RAdd(a, b) -> free-of(a, p) && free-of(b, p)
    RMul(a, b) -> free-of(a, p) && free-of(b, p)
    Inv(a)     -> free-of(a, p)
    Root(_, a) -> free-of(a, p)
    Pow(a, _)  -> free-of(a, p)

// Collect distinct (root-index, radicand) pairs from an expression.
// Uses structural equality on radicands to deduplicate.
pub fun collect-radicals(e : rad-expr<rational>) : div list<(int, rad-expr<rational>)>
  val all = collect-radicals-raw(e)
  nub-radicals(all)

// Collect all (root-index, radicand) pairs without deduplication.
fun collect-radicals-raw(e : rad-expr<rational>) : div list<(int, rad-expr<rational>)>
  match e
    Lit(_)     -> []
    Neg(a)     -> collect-radicals-raw(a)
    RAdd(a, b) -> collect-radicals-raw(a) ++ collect-radicals-raw(b)
    RMul(a, b) -> collect-radicals-raw(a) ++ collect-radicals-raw(b)
    Inv(a)     -> collect-radicals-raw(a)
    Pow(a, _)  -> collect-radicals-raw(a)
    Root(n, a) -> collect-radicals-raw(a) ++ [(n, a)]

// Deduplicate radical pairs by structural equality, preserving order.
fun nub-radicals(xs : list<(int, rad-expr<rational>)>) : div list<(int, rad-expr<rational>)>
  nub-radicals-acc(xs, [])

fun nub-radicals-acc(xs : list<(int, rad-expr<rational>)>, seen : list<(int, rad-expr<rational>)>) : div list<(int, rad-expr<rational>)>
  match xs
    Nil -> []
    Cons((n, e), rest) ->
      if member-radical(seen, n, e) then nub-radicals-acc(rest, seen)
      else Cons((n, e), nub-radicals-acc(rest, Cons((n, e), seen)))

fun member-radical(xs : list<(int, rad-expr<rational>)>, n : int, e : rad-expr<rational>) : div bool
  match xs
    Nil -> False
    Cons((m, f), rest) ->
      if m == n && rad-eq(e, f, fn(a : rational, b : rational) a == b) then True
      else member-radical(rest, n, e)

// Topologically sort radicals so that radicals with rational radicands
// come first, followed by radicals whose radicands depend only on
// earlier radicals. Unresolvable radicals are appended at the end.
pub fun topo-sort-radicals(rads : list<(int, rad-expr<rational>)>) : div list<(int, rad-expr<rational>)>
  topo-sort-loop([], rads)

fun topo-sort-loop(sorted : list<(int, rad-expr<rational>)>, remaining : list<(int, rad-expr<rational>)>) : div list<(int, rad-expr<rational>)>
  match remaining
    Nil -> sorted
    _   ->
      val ready = remaining.filter(fn(r) all-roots-resolved(sorted, r.snd))
      val remaining' = remaining.filter(fn(r) !member-radical(ready, r.fst, r.snd))
      match ready
        Nil -> sorted ++ remaining  // Can't resolve more; append as-is
        _   -> topo-sort-loop(sorted ++ ready, remaining')

// Check whether all Root subexpressions in a radicand are present
// in the resolved set.
pub fun all-roots-resolved(resolved : list<(int, rad-expr<rational>)>, e : rad-expr<rational>) : div bool
  match e
    Lit(_)     -> True
    Neg(a)     -> all-roots-resolved(resolved, a)
    RAdd(a, b) -> all-roots-resolved(resolved, a) && all-roots-resolved(resolved, b)
    RMul(a, b) -> all-roots-resolved(resolved, a) && all-roots-resolved(resolved, b)
    Inv(a)     -> all-roots-resolved(resolved, a)
    Pow(a, _)  -> all-roots-resolved(resolved, a)
    Root(n, a) -> member-radical(resolved, n, a)
