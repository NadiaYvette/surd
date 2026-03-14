package surd

import Eval.Complex

/** Typeclass for evaluating radical expressions into a numeric type.
  *
  * Implementations must provide conversions from rationals and all
  * arithmetic operations needed to evaluate the RadExpr AST.
  * Concrete instances are provided for Double and Complex.
  */
trait Evaluator[A]:
  /** Convert a rational coefficient to the target type. */
  def fromRational(r: Rational): A
  /** Addition in the target type. */
  def add(a: A, b: A): A
  /** Multiplication in the target type. */
  def mul(a: A, b: A): A
  /** Negation in the target type. */
  def neg(a: A): A
  /** Multiplicative inverse in the target type. */
  def inv(a: A): A
  /** Principal nth root in the target type. */
  def nthRoot(n: Int, a: A): A
  /** Integer power in the target type. */
  def pow(a: A, n: Int): A

object Evaluator:

  /** Evaluator instance for Double (fast but inexact; NaN for even roots of negatives). */
  given Evaluator[Double] with
    def fromRational(r: Rational): Double = r.toDouble
    def add(a: Double, b: Double): Double = a + b
    def mul(a: Double, b: Double): Double = a * b
    def neg(a: Double): Double = -a
    def inv(a: Double): Double = 1.0 / a
    def nthRoot(n: Int, a: Double): Double = Math.pow(a, 1.0 / n)
    def pow(a: Double, n: Int): Double = Math.pow(a, n)

  /** Evaluator instance for Complex (handles complex intermediates from casus irreducibilis). */
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
