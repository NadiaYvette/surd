package surd

import RadExpr.*

/** Convenience constructors and structural queries on radical expressions.
  *
  * These do NOT normalise -- they just build or inspect the AST.
  */
object Expr:

  // ---------------------------------------------------------------
  // Construction helpers
  // ---------------------------------------------------------------

  /** Literal coefficient. */
  def lit[K](v: K): RadExpr[K] = Lit(v)

  def neg[K](e: RadExpr[K]): RadExpr[K] = Neg(e)

  def add[K](a: RadExpr[K], b: RadExpr[K]): RadExpr[K] = Add(a, b)

  def sub[K](a: RadExpr[K], b: RadExpr[K]): RadExpr[K] = Add(a, Neg(b))

  def mul[K](a: RadExpr[K], b: RadExpr[K]): RadExpr[K] = Mul(a, b)

  def div[K](a: RadExpr[K], b: RadExpr[K]): RadExpr[K] = Mul(a, Inv(b))

  def inv[K](e: RadExpr[K]): RadExpr[K] = Inv(e)

  def root[K](n: Int, e: RadExpr[K]): RadExpr[K] = Root(n, e)

  def sqrt[K](e: RadExpr[K]): RadExpr[K] = Root(2, e)

  def pow[K](e: RadExpr[K], n: Int): RadExpr[K] = Pow(e, n)

  def fromInt(n: Int): RadExpr[Rational] = Lit(Rational(n))

  def fromBigInt(n: BigInt): RadExpr[Rational] = Lit(Rational(n))

  def fromRational(r: Rational): RadExpr[Rational] = Lit(r)

  // ---------------------------------------------------------------
  // Structural queries
  // ---------------------------------------------------------------

  /** Nesting depth of the expression tree. */
  def depth[K](e: RadExpr[K]): Int = e match
    case Lit(_)      => 0
    case Neg(a)      => depth(a)
    case Add(a, b)   => 1 + math.max(depth(a), depth(b))
    case Mul(a, b)   => 1 + math.max(depth(a), depth(b))
    case Inv(a)      => 1 + depth(a)
    case Root(_, a)  => 1 + depth(a)
    case Pow(a, _)   => 1 + depth(a)

  /** Number of nodes in the expression tree. */
  def size[K](e: RadExpr[K]): Int = e match
    case Lit(_)      => 1
    case Neg(a)      => 1 + size(a)
    case Add(a, b)   => 1 + size(a) + size(b)
    case Mul(a, b)   => 1 + size(a) + size(b)
    case Inv(a)      => 1 + size(a)
    case Root(_, a)  => 1 + size(a)
    case Pow(a, _)   => 1 + size(a)

  /** Check whether every coefficient in the expression satisfies predicate p. */
  def freeOf[K](p: K => Boolean)(e: RadExpr[K]): Boolean = e match
    case Lit(k)      => p(k)
    case Neg(a)      => freeOf(p)(a)
    case Add(a, b)   => freeOf(p)(a) && freeOf(p)(b)
    case Mul(a, b)   => freeOf(p)(a) && freeOf(p)(b)
    case Inv(a)      => freeOf(p)(a)
    case Root(_, a)  => freeOf(p)(a)
    case Pow(a, _)   => freeOf(p)(a)

  /** Map a function over the coefficients. */
  def mapCoeffs[A, B](f: A => B)(e: RadExpr[A]): RadExpr[B] = RadExpr.map(e)(f)

  // ---------------------------------------------------------------
  // Radical collection and dependency ordering
  // ---------------------------------------------------------------

  /** Collect distinct (rootIndex, radicand) pairs from an expression.
    * Uses structural equality for deduplication.
    */
  def collectRadicals[K](e: RadExpr[K]): List[(Int, RadExpr[K])] =
    go(e).distinct

  private def go[K](e: RadExpr[K]): List[(Int, RadExpr[K])] = e match
    case Lit(_)      => Nil
    case Neg(a)      => go(a)
    case Add(a, b)   => go(a) ++ go(b)
    case Mul(a, b)   => go(a) ++ go(b)
    case Inv(a)      => go(a)
    case Pow(a, _)   => go(a)
    case Root(n, a)  => go(a) :+ (n, a)

  /** Topologically sort radicals so that radicals with rational radicands
    * come first, followed by radicals whose radicands depend only on
    * earlier radicals. Unresolvable radicals are appended at the end.
    */
  def topoSortRadicals[K](rads: List[(Int, RadExpr[K])]): List[(Int, RadExpr[K])] =
    def loop(sorted: List[(Int, RadExpr[K])], remaining: List[(Int, RadExpr[K])]): List[(Int, RadExpr[K])] =
      if remaining.isEmpty then sorted
      else
        val ready = remaining.filter(r => allRootsResolved(sorted, r._2))
        val rest = remaining.filterNot(ready.contains)
        if ready.isEmpty then sorted ++ remaining  // can't resolve more
        else loop(sorted ++ ready, rest)
    loop(Nil, rads)

  /** Check whether all Root subexpressions in a radicand are present
    * in the resolved set.
    */
  def allRootsResolved[K](resolved: List[(Int, RadExpr[K])], e: RadExpr[K]): Boolean = e match
    case Lit(_)      => true
    case Neg(a)      => allRootsResolved(resolved, a)
    case Add(a, b)   => allRootsResolved(resolved, a) && allRootsResolved(resolved, b)
    case Mul(a, b)   => allRootsResolved(resolved, a) && allRootsResolved(resolved, b)
    case Inv(a)      => allRootsResolved(resolved, a)
    case Pow(a, _)   => allRootsResolved(resolved, a)
    case Root(n, a)  => resolved.contains((n, a))
