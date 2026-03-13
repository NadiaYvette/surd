package surd

import scala.collection.mutable

/** Explicit DAG (directed acyclic graph) representation for radical expressions.
  *
  * Converts tree-shaped RadExpr to an IntMap-based DAG where each unique
  * subexpression is stored once. Algorithms on RadDAG process each unique
  * node exactly once, avoiding exponential blowup from shared subexpressions.
  *
  * Unlike the Haskell version (which uses StableName for thunk sharing detection),
  * the Scala version uses structural equality for DAG construction.
  */
object DAG:

  type NodeId = Int

  /** A node operation in the DAG. Children are referenced by NodeId. */
  enum RadNodeOp[+K]:
    case NLit(value: K)
    case NNeg(child: NodeId)
    case NAdd(left: NodeId, right: NodeId)
    case NMul(left: NodeId, right: NodeId)
    case NInv(child: NodeId)
    case NRoot(n: Int, child: NodeId)
    case NPow(child: NodeId, n: Int)

  /** An explicit DAG of radical expression nodes.
    * Nodes are stored in an IntMap (Map[Int, RadNodeOp]).
    * Children always have lower NodeIds than parents (topological order).
    */
  final case class RadDAG[+K](nodes: Map[NodeId, RadNodeOp[K]], root: NodeId)

  import RadNodeOp.*

  /** Convert a RadExpr to an explicit DAG using structural equality. */
  def toDAG[K](expr: RadExpr[K]): RadDAG[K] =
    val nodeMap = mutable.Map.empty[RadExpr[K], NodeId]
    val nodes = mutable.Map.empty[NodeId, RadNodeOp[K]]
    var nextId = 0

    def intern(e: RadExpr[K]): NodeId =
      nodeMap.getOrElseUpdate(e, {
        val op = e match
          case RadExpr.Lit(v) => NLit(v)
          case RadExpr.Neg(a) => NNeg(intern(a))
          case RadExpr.Add(a, b) => NAdd(intern(a), intern(b))
          case RadExpr.Mul(a, b) => NMul(intern(a), intern(b))
          case RadExpr.Inv(a) => NInv(intern(a))
          case RadExpr.Root(n, a) => NRoot(n, intern(a))
          case RadExpr.Pow(a, n) => NPow(intern(a), n)
        val id = nextId
        nextId += 1
        nodes(id) = op
        id
      })

    val rootId = intern(expr)
    RadDAG(nodes.toMap, rootId)

  /** Convert a DAG back to a RadExpr, preserving structural sharing. */
  def fromDAG[K](dag: RadDAG[K]): RadExpr[K] =
    val cache = mutable.Map.empty[NodeId, RadExpr[K]]

    def build(id: NodeId): RadExpr[K] =
      cache.getOrElseUpdate(id, {
        dag.nodes(id) match
          case NLit(v) => RadExpr.Lit(v)
          case NNeg(c) => RadExpr.Neg(build(c))
          case NAdd(l, r) => RadExpr.Add(build(l), build(r))
          case NMul(l, r) => RadExpr.Mul(build(l), build(r))
          case NInv(c) => RadExpr.Inv(build(c))
          case NRoot(n, c) => RadExpr.Root(n, build(c))
          case NPow(c, n) => RadExpr.Pow(build(c), n)
      })

    build(dag.root)

  /** Number of unique nodes in the DAG. */
  def dagSize[K](dag: RadDAG[K]): Int = dag.nodes.size

  /** Maximum depth of the DAG (longest path from root to any leaf). */
  def dagDepth[K](dag: RadDAG[K]): Int =
    val depths = mutable.Map.empty[NodeId, Int]
    def depth(id: NodeId): Int =
      depths.getOrElseUpdate(id, {
        dag.nodes(id) match
          case NLit(_) => 0
          case NNeg(c) => 1 + depth(c)
          case NAdd(l, r) => 1 + math.max(depth(l), depth(r))
          case NMul(l, r) => 1 + math.max(depth(l), depth(r))
          case NInv(c) => 1 + depth(c)
          case NRoot(_, c) => 1 + depth(c)
          case NPow(c, _) => 1 + depth(c)
      })
    depth(dag.root)

  /** DAG-aware constant folding. Each unique node processed once. */
  def dagFoldConstants(dag: RadDAG[Rational]): RadDAG[Rational] =
    val folded = mutable.Map.empty[NodeId, (NodeId, RadNodeOp[Rational])]
    var nextId = 0

    def fold(id: NodeId): NodeId =
      if folded.contains(id) then folded(id)._1
      else
        val op = dag.nodes(id)
        val newOp = op match
          case NLit(_) => op
          case NNeg(c) =>
            val fc = fold(c)
            folded.get(fc).map(_._2) match
              case Some(NLit(r)) => NLit(-r)
              case _ => NNeg(fc)
          case NAdd(l, r) =>
            val fl = fold(l); val fr = fold(r)
            (folded.get(fl).map(_._2), folded.get(fr).map(_._2)) match
              case (Some(NLit(a)), Some(NLit(b))) => NLit(a + b)
              case (Some(NLit(a)), _) if a.isZero => folded(fr)._2
              case (_, Some(NLit(b))) if b.isZero => folded(fl)._2
              case _ => NAdd(fl, fr)
          case NMul(l, r) =>
            val fl = fold(l); val fr = fold(r)
            (folded.get(fl).map(_._2), folded.get(fr).map(_._2)) match
              case (Some(NLit(a)), Some(NLit(b))) => NLit(a * b)
              case (Some(NLit(a)), _) if a.isZero => NLit(Rational.zero)
              case (_, Some(NLit(b))) if b.isZero => NLit(Rational.zero)
              case (Some(NLit(a)), _) if a.isOne => folded(fr)._2
              case (_, Some(NLit(b))) if b.isOne => folded(fl)._2
              case _ => NMul(fl, fr)
          case NInv(c) =>
            val fc = fold(c)
            folded.get(fc).map(_._2) match
              case Some(NLit(r)) if !r.isZero => NLit(r.inverse)
              case _ => NInv(fc)
          case NRoot(n, c) =>
            val fc = fold(c)
            folded.get(fc).map(_._2) match
              case Some(NLit(r)) if r.isZero => NLit(Rational.zero)
              case Some(NLit(r)) if r.isOne => NLit(Rational.one)
              case _ => NRoot(n, fc)
          case NPow(c, n) =>
            val fc = fold(c)
            folded.get(fc).map(_._2) match
              case Some(NLit(r)) => NLit(r.pow(n))
              case _ =>
                if n == 0 then NLit(Rational.one)
                else NPow(fc, n)

        val nid = nextId
        nextId += 1
        folded(id) = (nid, newOp)
        nid

    val newRoot = fold(dag.root)
    val newNodes = folded.values.map((nid, op) => (nid, op)).toMap
    RadDAG(newNodes, newRoot)

  /** DAG-aware complex evaluation. Each unique node evaluated once. */
  def dagEvalComplex(dag: RadDAG[Rational]): Eval.Complex =
    val cache = mutable.Map.empty[NodeId, Eval.Complex]

    def ev(id: NodeId): Eval.Complex =
      cache.getOrElseUpdate(id, {
        dag.nodes(id) match
          case NLit(r) => Eval.Complex.fromDouble(r.toDouble)
          case NNeg(c) => -ev(c)
          case NAdd(l, r) => ev(l) + ev(r)
          case NMul(l, r) => ev(l) * ev(r)
          case NInv(c) => ev(c).inverse
          case NRoot(n, c) => Eval.complexNthRoot(n, ev(c))
          case NPow(c, n) => ev(c).pow(n)
      })

    ev(dag.root)

  /** Convenience: convert, fold, convert back. */
  def dagFold(expr: RadExpr[Rational]): RadExpr[Rational] =
    fromDAG(dagFoldConstants(toDAG(expr)))

  /** Convenience: convert and evaluate. */
  def dagEval(expr: RadExpr[Rational]): Eval.Complex =
    dagEvalComplex(toDAG(expr))
