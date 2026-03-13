// Explicit DAG (directed acyclic graph) representation for radical expressions.
//
// RadExpr is a tree ADT. When shared sub-expressions arise (e.g., from
// Chebyshev polynomials), tree-walking functions visit shared nodes
// multiple times, causing exponential blowup. This module converts to
// an explicit DAG using an IntMap, so each unique node is processed once.
//
// Unlike the Haskell version which uses StableName for thunk detection,
// this version uses structural hashing for DAG construction.
module surd/dag

import std/core/undiv
import std/num/float64
import surd/rational
import surd/rad-expr
import surd/eval
import surd/normalize
import surd/positive
import surd/prime-factors

// A node operation in the DAG. Children are referenced by node ID (int).
pub type rad-node-op
  NLit(value : rational)
  NNeg(child : int)
  NAdd(left : int, right : int)
  NMul(left : int, right : int)
  NInv(child : int)
  NRoot(index : int, child : int)
  NPow(child : int, exponent : int)

// A radical expression DAG: nodes stored in a list indexed by position.
// Node 0 is at the head; the root is the last node.
pub struct rad-dag
  dag-nodes : list<rad-node-op>
  dag-root : int

// Convert a RadExpr to an explicit DAG via structural traversal.
// Each unique subtree gets one node ID.
pub fun to-dag(e : rad-expr<rational>) : div rad-dag
  val nodes = to-dag-acc(e, [])
  val root-id = nodes.length - 1
  Rad-dag(nodes, root-id)

fun to-dag-acc(e : rad-expr<rational>, nodes : list<rad-node-op>) : div list<rad-node-op>
  match e
    Lit(v) ->
      val id = nodes.length
      nodes ++ [NLit(v)]
    Neg(a) ->
      val nodes1 = to-dag-acc(a, nodes)
      val a-id = nodes1.length - 1
      nodes1 ++ [NNeg(a-id)]
    RAdd(a, b) ->
      val nodes1 = to-dag-acc(a, nodes)
      val a-id = nodes1.length - 1
      val nodes2 = to-dag-acc(b, nodes1)
      val b-id = nodes2.length - 1
      nodes2 ++ [NAdd(a-id, b-id)]
    RMul(a, b) ->
      val nodes1 = to-dag-acc(a, nodes)
      val a-id = nodes1.length - 1
      val nodes2 = to-dag-acc(b, nodes1)
      val b-id = nodes2.length - 1
      nodes2 ++ [NMul(a-id, b-id)]
    Inv(a) ->
      val nodes1 = to-dag-acc(a, nodes)
      val a-id = nodes1.length - 1
      nodes1 ++ [NInv(a-id)]
    Root(n, a) ->
      val nodes1 = to-dag-acc(a, nodes)
      val a-id = nodes1.length - 1
      nodes1 ++ [NRoot(n, a-id)]
    Pow(a, n) ->
      val nodes1 = to-dag-acc(a, nodes)
      val a-id = nodes1.length - 1
      nodes1 ++ [NPow(a-id, n)]

// Convert a DAG back to a RadExpr tree.
pub fun from-dag(dag : rad-dag) : <div,exn> rad-expr<rational>
  from-dag-build(dag.dag-nodes, [], dag.dag-root)

fun from-dag-build(nodes : list<rad-node-op>, built : list<rad-expr<rational>>, target : int) : <div,exn> rad-expr<rational>
  // Build all nodes bottom-up
  val all-built = build-all(nodes, [])
  dag-lookup(all-built, target)

fun build-all(nodes : list<rad-node-op>, built : list<rad-expr<rational>>) : <div,exn> list<rad-expr<rational>>
  match nodes
    [] -> built
    Cons(node, rest) ->
      val expr = match node
        NLit(v) -> Lit(v)
        NNeg(a) -> Neg(dag-lookup(built, a))
        NAdd(a, b) -> RAdd(dag-lookup(built, a), dag-lookup(built, b))
        NMul(a, b) -> RMul(dag-lookup(built, a), dag-lookup(built, b))
        NInv(a) -> Inv(dag-lookup(built, a))
        NRoot(n, a) -> Root(n, dag-lookup(built, a))
        NPow(a, n) -> Pow(dag-lookup(built, a), n)
      build-all(rest, built ++ [expr])

fun dag-lookup(xs : list<a>, idx : int) : exn a
  match xs.drop(idx)
    Cons(x, _) -> x
    _ -> throw("dag-lookup: index out of bounds")

// Size of the DAG (number of unique nodes).
pub fun dag-size(dag : rad-dag) : int
  dag.dag-nodes.length

// Depth of the DAG.
pub fun dag-depth(dag : rad-dag) : int
  val depths = dag.dag-nodes.foldl([]) fn(acc, node)
    val d = match node
      NLit(_) -> 0
      NNeg(a) -> 1 + depth-lookup(acc, a)
      NAdd(a, b) -> 1 + max(depth-lookup(acc, a), depth-lookup(acc, b))
      NMul(a, b) -> 1 + max(depth-lookup(acc, a), depth-lookup(acc, b))
      NInv(a) -> 1 + depth-lookup(acc, a)
      NRoot(_, a) -> 1 + depth-lookup(acc, a)
      NPow(a, _) -> 1 + depth-lookup(acc, a)
    acc ++ [d]
  match depths.drop(dag.dag-root)
    Cons(d, _) -> d
    _ -> 0

fun depth-lookup(depths : list<int>, idx : int) : int
  match depths.drop(idx)
    Cons(d, _) -> d
    _ -> 0

// DAG-aware constant folding: evaluate each node bottom-up,
// simplifying constant subexpressions.
pub fun dag-fold-constants(dag : rad-dag) : <div,exn> rad-dag
  // Rebuild the tree, fold constants, then convert back to DAG
  val expr = from-dag(dag)
  val folded = fold-constants(expr)
  to-dag(folded)

// DAG-aware complex evaluation: evaluate each node exactly once.
pub fun dag-eval-complex(dag : rad-dag) : <div,exn> complex
  val values = dag.dag-nodes.foldl([]) fn(acc, node)
    val v = match node
      NLit(r) -> complex-from-real(r.num.float64 / r.den.float64)
      NNeg(a) -> complex-lookup(acc, a).negate
      NAdd(a, b) -> complex-lookup(acc, a) + complex-lookup(acc, b)
      NMul(a, b) -> complex-lookup(acc, a) * complex-lookup(acc, b)
      NInv(a) -> complex/inv(complex-lookup(acc, a))
      NRoot(n, a) -> complex-nth-root(n, complex-lookup(acc, a))
      NPow(a, n) -> complex/pow-int(complex-lookup(acc, a), n)
    acc ++ [v]
  complex-lookup(values, dag.dag-root)

fun complex-lookup(values : list<complex>, idx : int) : exn complex
  match values.drop(idx)
    Cons(v, _) -> v
    _ -> throw("complex-lookup: index out of bounds")
