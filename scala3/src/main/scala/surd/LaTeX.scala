package surd

import RadExpr.*

/** LaTeX rendering of radical expressions. */
object LaTeX:

  private val PrecAdd = 1
  private val PrecMul = 2
  private val PrecNeg = 3
  private val PrecPow = 4

  /** Render a radical expression as a LaTeX math-mode string. */
  def latex(expr: RadExpr[Rational]): String = latexPrec(0, expr)

  /** Render with precedence context. */
  def latexPrec(p: Int, expr: RadExpr[Rational]): String = expr match
    case Lit(r) => latexRat(r)

    case Neg(e) => e match
      // Neg of sum: distribute sign
      case _: Add[_] =>
        parensIf(p > PrecAdd, renderTerms(flattenAdd(e).map(negTerm)))
      // Neg of product with literal coefficient: absorb sign
      case Mul(Lit(c), rest) =>
        latexPrec(p, Mul(Lit(-c), rest))
      // Neg of literal
      case Lit(r) =>
        latexPrec(p, Lit(-r))
      // Otherwise: prefix minus
      case _ =>
        parensIf(p > PrecNeg, "-" + latexPrec(PrecNeg, e))

    case e @ Add(_, _) =>
      parensIf(p > PrecAdd, renderTerms(flattenAdd(e)))

    // a / b
    case Mul(a, Inv(b)) =>
      parensIf(p > PrecMul,
        "\\frac{" + latexPrec(0, a) + "}{" + latexPrec(0, b) + "}")

    // (1/a) * b => b/a
    case Mul(Inv(a), b) =>
      parensIf(p > PrecMul,
        "\\frac{" + latexPrec(0, b) + "}{" + latexPrec(0, a) + "}")

    case Inv(e) =>
      parensIf(p > PrecMul,
        "\\frac{1}{" + latexPrec(0, e) + "}")

    case Root(2, Lit(r)) if r == -Rational.one => "\\mathrm{i}"
    case Root(2, e) => "\\sqrt{" + latexRadicand(e) + "}"
    case Root(n, e) => "\\sqrt[" + n.toString + "]{" + latexRadicand(e) + "}"

    case Pow(_, 0) => "1"
    case Pow(e, n) if n < 0 =>
      latexPrec(p, Inv(Pow(e, -n)))
    case Pow(e, 1) => latexPrec(p, e)
    case Pow(e, n) =>
      parensIf(p > PrecPow, latexBase(e) + "^{" + n.toString + "}")

    case e @ Mul(_, _) =>
      parensIf(p > PrecMul, renderFactors(flattenMul(e)))

  // --------------------------------------------------------------------------
  // Helpers
  // --------------------------------------------------------------------------

  private def parensIf(cond: Boolean, s: String): String =
    if cond then "\\left(" + s + "\\right)" else s

  private def latexRat(r: Rational): String =
    if r.den == BigInt(1) then r.num.toString
    else if r.num < BigInt(0) then
      s"-\\frac{${r.num.abs}}{${r.den}}"
    else s"\\frac{${r.num}}{${r.den}}"

  /** Render the base of a power expression. Compound expressions need grouping. */
  private def latexBase(e: RadExpr[Rational]): String = e match
    case Root(_, _) | Add(_, _) | Mul(_, _) | Neg(_) | Inv(_) =>
      "\\left(" + latexPrec(0, e) + "\\right)"
    case _ => latexPrec(PrecPow, e)

  /** Render a radicand inside \sqrt{...}. No outer parens needed since braces group. */
  private def latexRadicand(e: RadExpr[Rational]): String = e match
    case Lit(r) =>
      if r.den == BigInt(1) then r.num.toString
      else if r.num < BigInt(0) then
        s"-\\frac{${r.num.abs}}{${r.den}}"
      else s"\\frac{${r.num}}{${r.den}}"
    case _ => latexPrec(0, e)

  private def negTerm(t: (Boolean, RadExpr[Rational])): (Boolean, RadExpr[Rational]) =
    (!t._1, t._2)

  private def flattenAdd(e: RadExpr[Rational]): List[(Boolean, RadExpr[Rational])] = e match
    case Add(a, b) => flattenAdd(a) ++ flattenAdd(b)
    case Neg(inner) => flattenAdd(inner).map(negTerm)
    case Lit(r) if r < Rational.zero => List((false, Lit(-r)))
    case Mul(Neg(a), b) => List((false, Mul(a, b)))
    case e1 @ Mul(_, _) =>
      flattenMul(e1) match
        case Lit(r) :: rest if r < Rational.zero =>
          List((false, rebuildMul(Lit(-r) :: rest)))
        case _ => List((true, e1))
    case _ => List((true, e))

  private def rebuildMul(xs: List[RadExpr[Rational]]): RadExpr[Rational] = xs match
    case Nil      => Lit(Rational.one)
    case x :: Nil => x
    case x :: rest => rest.foldLeft(x)(Mul(_, _))

  private def renderTerms(terms: List[(Boolean, RadExpr[Rational])]): String = terms match
    case Nil => "0"
    case (s, t) :: rest =>
      val hd = if s then latexPrec(PrecAdd, t) else "-" + latexPrec(PrecMul, t)
      hd + rest.map {
        case (true, e)  => " + " + latexPrec(PrecAdd, e)
        case (false, e) => " - " + latexPrec(PrecMul, e)
      }.mkString

  private def flattenMul(e: RadExpr[Rational]): List[RadExpr[Rational]] = e match
    case Mul(a, b) => flattenMul(a) ++ flattenMul(b)
    case _         => List(e)

  private def renderFactors(fs: List[RadExpr[Rational]]): String = fs match
    case Nil => "1"
    case x :: Nil => latexPrec(PrecMul, x)
    case Lit(c) :: rest =>
      val restStrs = rest.map(latexPrec(PrecPow, _))
      val joined = restStrs.mkString(" \\cdot ")
      if c.isOne then joined
      else if c == -Rational.one then "-" + joined
      else latexRat(c) + " \\cdot " + joined
    case _ =>
      fs.map(latexPrec(PrecPow, _)).mkString(" \\cdot ")
