package surd

import Eval.Complex

/** Typeclass for evaluating radical expressions into a numeric type. */
trait Evaluator[A]:
  def fromRational(r: Rational): A
  def add(a: A, b: A): A
  def mul(a: A, b: A): A
  def neg(a: A): A
  def inv(a: A): A
  def nthRoot(n: Int, a: A): A
  def pow(a: A, n: Int): A

object Evaluator:

  given Evaluator[Double] with
    def fromRational(r: Rational): Double = r.toDouble
    def add(a: Double, b: Double): Double = a + b
    def mul(a: Double, b: Double): Double = a * b
    def neg(a: Double): Double = -a
    def inv(a: Double): Double = 1.0 / a
    def nthRoot(n: Int, a: Double): Double = Math.pow(a, 1.0 / n)
    def pow(a: Double, n: Int): Double = Math.pow(a, n)

  given Evaluator[Complex] with
    def fromRational(r: Rational): Complex = Complex.fromDouble(r.toDouble)
    def add(a: Complex, b: Complex): Complex = a + b
    def mul(a: Complex, b: Complex): Complex = a * b
    def neg(a: Complex): Complex = -a
    def inv(a: Complex): Complex = a.inverse
    def nthRoot(n: Int, a: Complex): Complex = Eval.complexNthRoot(n, a)
    def pow(a: Complex, n: Int): Complex =
      if n >= 0 then a.pow(n)
      else a.inverse.pow(-n)

  /** Evaluate a radical expression generically using an Evaluator typeclass instance. */
  def eval[A: Evaluator](expr: RadExpr[Rational]): A =
    val ev = summon[Evaluator[A]]
    expr match
      case RadExpr.Lit(r)     => ev.fromRational(r)
      case RadExpr.Neg(a)     => ev.neg(eval(a))
      case RadExpr.Add(a, b)  => ev.add(eval(a), eval(b))
      case RadExpr.Mul(a, b)  => ev.mul(eval(a), eval(b))
      case RadExpr.Inv(a)     => ev.inv(eval(a))
      case RadExpr.Root(n, a) => ev.nthRoot(n, eval(a))
      case RadExpr.Pow(a, n)  => ev.pow(eval(a), n)
