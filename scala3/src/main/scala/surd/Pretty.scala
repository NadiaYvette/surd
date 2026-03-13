package surd

import RadExpr.*

/** Pretty-printing radical expressions in human-readable mathematical notation.
  *
  * Includes common subexpression elimination (CSE) for readability.
  */
object Pretty:

  private val PrecAdd  = 1
  private val PrecMul  = 2
  private val PrecNeg  = 3
  private val PrecPow  = 4
  private val PrecAtom = 5

  /** Render a radical expression as a human-readable string. */
  def pretty(expr: RadExpr[Rational]): String = prettyPrec(0, expr)

  /** Render with precedence context. */
  def prettyPrec(p: Int, expr: RadExpr[Rational]): String = expr match
    case Lit(r) => prettyRat(r)

    case Neg(e) => e match
      // Neg of sum: distribute sign
      case _: Add[_] =>
        parensIf(p > PrecAdd, renderTermsBasic(flattenAddBasic(e).map(negTerm)))
      // Neg of product with literal coefficient: absorb sign
      case Mul(Lit(c), rest) =>
        prettyPrec(p, Mul(Lit(-c), rest))
      // Neg of literal
      case Lit(r) =>
        prettyPrec(p, Lit(-r))
      // Otherwise: prefix minus
      case _ =>
        parensIf(p > PrecNeg, "-" + prettyPrec(PrecNeg, e))

    case e @ Add(_, _) =>
      parensIf(p > PrecAdd, renderTermsBasic(flattenAddBasic(e)))

    case Mul(a, Inv(b)) =>
      parensIf(p > PrecMul, prettyPrec(PrecMul, a) + "/" + prettyPrec(PrecPow, b))

    case e @ Mul(_, _) =>
      parensIf(p > PrecMul, renderFactorsBasic(flattenMulBasic(e)))

    case Inv(e) =>
      parensIf(p > PrecMul, "1/" + prettyPrec(PrecPow, e))

    case Root(2, Lit(r)) if r == -Rational.one => "i"
    case Root(2, e) => "\u221a" + prettyRadicandBasic(e)
    case Root(3, e) => "\u221b" + prettyRadicandBasic(e)
    case Root(n, e) => n.toString + "\u221a" + prettyRadicandBasic(e)

    case Pow(_, 0) => "1"
    case Pow(e, n) if n < 0 => prettyPrec(p, Inv(Pow(e, -n)))
    case Pow(e, 1) => prettyPrec(p, e)
    case Pow(e, n) =>
      parensIf(p > PrecPow, prettyPrec(PrecPow, e) + "^" + n.toString)

  // --------------------------------------------------------------------------
  // CSE pretty-printing
  // --------------------------------------------------------------------------

  /** Render with CSE: repeated subexpressions are shown as named intermediates. */
  def prettyCSE(expr: RadExpr[Rational]): String =
    val counts = countSubs(expr)
    val shared = counts.filter((k, v) => v >= 2 && worthNaming(k)).keys.toList
    val sorted = shared.sortBy(exprSize)
    val nameList = sorted.zipWithIndex.map((_, i) => varName(i))
    val nameMap: Map[RadExpr[Rational], String] = sorted.zip(nameList).toMap

    val bindings = sorted.zip(nameList).map { (sub, name) =>
      val availNames = nameMap.removed(sub)
      s"  $name = ${renderWith(availNames, 0, sub)}"
    }
    val body = renderWith(nameMap, 0, expr)

    if bindings.isEmpty then body
    else "let\n" + bindings.mkString("\n") + "\n" + "in " + body

  private def varName(i: Int): String =
    if i < 26 then ('a' + i).toChar.toString
    else ('a' + (i % 26)).toChar.toString + (i / 26).toString

  private def countSubs(expr: RadExpr[Rational]): Map[RadExpr[Rational], Int] =
    def go(m: Map[RadExpr[Rational], Int], e: RadExpr[Rational]): Map[RadExpr[Rational], Int] =
      val m1 = m.updatedWith(e) {
        case Some(c) => Some(c + 1)
        case None    => Some(1)
      }
      e match
        case Lit(_)      => m1
        case Neg(a)      => go(m1, a)
        case Add(a, b)   => go(go(m1, a), b)
        case Mul(a, b)   => go(go(m1, a), b)
        case Inv(a)      => go(m1, a)
        case Root(_, a)  => go(m1, a)
        case Pow(a, _)   => go(m1, a)
    go(Map.empty, expr)

  private def worthNaming[K](e: RadExpr[K]): Boolean = e match
    case Lit(_)         => false
    case Neg(Lit(_))    => false
    case Root(_, Lit(_))=> false
    case _              => true

  private def exprSize[K](e: RadExpr[K]): Int = e match
    case Lit(_)      => 1
    case Neg(a)      => 1 + exprSize(a)
    case Add(a, b)   => 1 + exprSize(a) + exprSize(b)
    case Mul(a, b)   => 1 + exprSize(a) + exprSize(b)
    case Inv(a)      => 1 + exprSize(a)
    case Root(_, a)  => 1 + exprSize(a)
    case Pow(a, _)   => 1 + exprSize(a)

  /** Pretty-print substituting names for shared subexpressions. */
  private def renderWith(
      names: Map[RadExpr[Rational], String],
      p: Int,
      e: RadExpr[Rational]
  ): String =
    names.get(e) match
      case Some(name) => parensIf(p > PrecAtom, name)
      case None => pp(names, p, e)

  private def pp(
      names: Map[RadExpr[Rational], String],
      p: Int,
      e: RadExpr[Rational]
  ): String =
    def go(prec: Int, expr: RadExpr[Rational]): String = renderWith(names, prec, expr)

    e match
      case Lit(r) => prettyRat(r)
      case Neg(inner) =>
        parensIf(p > PrecNeg, "-" + go(PrecNeg, inner))
      case e1 @ Add(_, _) =>
        val terms = flattenAddCSE(names, e1)
        parensIf(p > PrecAdd, renderTermsCSE(names, terms))
      case Mul(a, Inv(b)) =>
        parensIf(p > PrecMul, go(PrecMul, a) + "/" + go(PrecPow, b))
      case e1 @ Mul(_, _) =>
        val factors = flattenMulBasic(e1)
        parensIf(p > PrecMul, renderFactorsCSE(names, factors))
      case Inv(inner) =>
        parensIf(p > PrecMul, "1/" + go(PrecPow, inner))
      case Root(2, Lit(r)) if r == -Rational.one => "i"
      case Root(2, inner) =>
        "\u221a" + radicandCSE(names, inner)
      case Root(3, inner) =>
        "\u221b" + radicandCSE(names, inner)
      case Root(n, inner) =>
        n.toString + "\u221a" + radicandCSE(names, inner)
      case Pow(_, 0) => "1"
      case Pow(inner, n) if n < 0 =>
        pp(names, p, Inv(Pow(inner, -n)))
      case Pow(inner, 1) => go(p, inner)
      case Pow(inner, n) =>
        parensIf(p > PrecPow, go(PrecPow, inner) + "^" + n.toString)

  private def radicandCSE(
      names: Map[RadExpr[Rational], String],
      e: RadExpr[Rational]
  ): String = e match
    case Lit(r) if r >= Rational.zero && r.den == BigInt(1) =>
      r.num.toString
    case _ if isSimpleCSE(names, e) =>
      renderWith(names, PrecPow, e)
    case _ =>
      "(" + renderWith(names, 0, e) + ")"

  private def isSimpleCSE(names: Map[RadExpr[Rational], String], e: RadExpr[Rational]): Boolean =
    e match
      case Lit(_)      => true
      case Root(_, _)  => true
      case _ => names.contains(e)

  private def flattenAddCSE(
      names: Map[RadExpr[Rational], String],
      e: RadExpr[Rational]
  ): List[(Boolean, RadExpr[Rational])] = e match
    case Add(a, b) => flattenAddCSE(names, a) ++ flattenAddCSE(names, b)
    case Neg(a)    => flattenAddCSE(names, a).map((s, t) => (!s, t))
    case Lit(r) if r < Rational.zero => List((false, Lit(-r)))
    case Mul(Neg(a), b) => List((false, Mul(a, b)))
    case e1 @ Mul(_, _) =>
      flattenMulBasic(e1) match
        case Lit(r) :: rest if r < Rational.zero =>
          List((false, rebuildMul(Lit(-r) :: rest)))
        case _ => List((true, e1))
    case _ => List((true, e))

  private def renderTermsCSE(
      names: Map[RadExpr[Rational], String],
      terms: List[(Boolean, RadExpr[Rational])]
  ): String = terms match
    case Nil => "0"
    case (s, t) :: rest =>
      val hd = if s then renderWith(names, PrecAdd, t)
               else "-" + renderWith(names, PrecMul, t)
      hd + rest.map {
        case (true, e)  => " + " + renderWith(names, PrecAdd, e)
        case (false, e) => " - " + renderWith(names, PrecMul, e)
      }.mkString

  private def renderFactorsCSE(
      names: Map[RadExpr[Rational], String],
      fs: List[RadExpr[Rational]]
  ): String = fs match
    case Nil => "1"
    case x :: Nil => renderWith(names, PrecMul, x)
    case Lit(c) :: rest =>
      val restStrs = rest.map(renderWith(names, PrecPow, _))
      if c.isOne then restStrs.mkString("\u00b7")
      else if c == -Rational.one then "-" + restStrs.mkString("\u00b7")
      else prettyRat(c) + "\u00b7" + restStrs.mkString("\u00b7")
    case _ =>
      fs.map(renderWith(names, PrecPow, _)).mkString("\u00b7")

  // --------------------------------------------------------------------------
  // Shared helpers
  // --------------------------------------------------------------------------

  private def parensIf(cond: Boolean, s: String): String =
    if cond then "(" + s + ")" else s

  private def prettyRat(r: Rational): String =
    if r.den == BigInt(1) then r.num.toString
    else if r.num < BigInt(0) then
      s"(-${r.num.abs}/${r.den})"
    else s"(${r.num}/${r.den})"

  private def prettyRadicandBasic(e: RadExpr[Rational]): String = e match
    case Lit(r) if r >= Rational.zero && r.den == BigInt(1) =>
      r.num.toString
    case _ if isSimpleBasic(e) =>
      prettyPrec(PrecPow, e)
    case _ =>
      "(" + pretty(e) + ")"

  private def isSimpleBasic[K](e: RadExpr[K]): Boolean = e match
    case Lit(_)     => true
    case Root(_, _) => true
    case _          => false

  private def negTerm(t: (Boolean, RadExpr[Rational])): (Boolean, RadExpr[Rational]) =
    (!t._1, t._2)

  private def flattenAddBasic(e: RadExpr[Rational]): List[(Boolean, RadExpr[Rational])] = e match
    case Add(a, b) => flattenAddBasic(a) ++ flattenAddBasic(b)
    case Neg(inner) => flattenAddBasic(inner).map(negTerm)
    case Lit(r) if r < Rational.zero => List((false, Lit(-r)))
    case Mul(Neg(a), b) => List((false, Mul(a, b)))
    case e1 @ Mul(_, _) =>
      flattenMulBasic(e1) match
        case Lit(r) :: rest if r < Rational.zero =>
          List((false, rebuildMul(Lit(-r) :: rest)))
        case _ => List((true, e1))
    case _ => List((true, e))

  private def renderTermsBasic(terms: List[(Boolean, RadExpr[Rational])]): String = terms match
    case Nil => "0"
    case (s, t) :: rest =>
      val hd = if s then prettyPrec(PrecAdd, t) else "-" + prettyPrec(PrecMul, t)
      hd + rest.map {
        case (true, e)  => " + " + prettyPrec(PrecAdd, e)
        case (false, e) => " - " + prettyPrec(PrecMul, e)
      }.mkString

  private def flattenMulBasic[K](e: RadExpr[K]): List[RadExpr[K]] = e match
    case Mul(a, b) => flattenMulBasic(a) ++ flattenMulBasic(b)
    case _         => List(e)

  private def rebuildMul[K](xs: List[RadExpr[K]]): RadExpr[K] = xs match
    case Nil      => Lit(Rational.one).asInstanceOf[RadExpr[K]]
    case x :: Nil => x
    case x :: rest => rest.foldLeft(x)(Mul(_, _))

  private def renderFactorsBasic(fs: List[RadExpr[Rational]]): String = fs match
    case Nil => "1"
    case x :: Nil => prettyPrec(PrecMul, x)
    case Lit(c) :: rest =>
      val restStrs = rest.map(prettyPrec(PrecPow, _))
      if c.isOne then restStrs.mkString("\u00b7")
      else if c == -Rational.one then "-" + restStrs.mkString("\u00b7")
      else prettyRat(c) + "\u00b7" + restStrs.mkString("\u00b7")
    case _ =>
      fs.map(prettyPrec(PrecPow, _)).mkString("\u00b7")
