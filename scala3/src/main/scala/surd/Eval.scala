package surd

import RadExpr.*

/** Numerical evaluation of radical expressions. */
object Eval:

  /** Simple complex number as a pair of Doubles. */
  final case class Complex(re: Double, im: Double):
    def +(that: Complex): Complex = Complex(re + that.re, im + that.im)
    def -(that: Complex): Complex = Complex(re - that.re, im - that.im)
    def unary_- : Complex = Complex(-re, -im)

    /** (a+bi)(c+di) = (ac-bd) + (ad+bc)i */
    def *(that: Complex): Complex =
      Complex(re * that.re - im * that.im, re * that.im + im * that.re)

    /** 1/(a+bi) = (a-bi)/(a^2+b^2) */
    def inverse: Complex =
      val d = re * re + im * im
      Complex(re / d, -im / d)

    def /(that: Complex): Complex = this * that.inverse

    def magnitude: Double = math.sqrt(re * re + im * im)
    def phase: Double = math.atan2(im, re)

    def pow(n: Int): Complex =
      if n == 0 then Complex.one
      else if n == 1 then this
      else if n < 0 then inverse.pow(-n)
      else if n % 2 == 0 then
        val half = pow(n / 2)
        half * half
      else this * pow(n - 1)

  object Complex:
    val zero: Complex = Complex(0.0, 0.0)
    val one: Complex = Complex(1.0, 0.0)
    val i: Complex = Complex(0.0, 1.0)
    def fromDouble(d: Double): Complex = Complex(d, 0.0)

  // --------------------------------------------------------------------------
  // eval: RadExpr[Rational] => Double
  // --------------------------------------------------------------------------

  /** Evaluate a radical expression to a Double.
    *
    * Note: expressions involving even roots of negative numbers will produce NaN.
    * Use evalComplex for expressions with complex intermediates.
    */
  def eval(expr: RadExpr[Rational]): Double =
    Evaluator.eval[Double](expr)

  // --------------------------------------------------------------------------
  // evalComplex: RadExpr[Rational] => Complex
  // --------------------------------------------------------------------------

  /** Evaluate a radical expression to a Complex value.
    *
    * Handles expressions that pass through complex intermediates,
    * such as those arising from the casus irreducibilis.
    */
  def evalComplex(expr: RadExpr[Rational]): Complex =
    Evaluator.eval[Complex](expr)

  /** Principal nth root of a complex number via polar form.
    * z = r * e^(i*theta) => z^(1/n) = r^(1/n) * e^(i*theta/n)
    */
  def complexNthRoot(n: Int, z: Complex): Complex =
    val r = z.magnitude
    val theta = z.phase
    val rn = math.pow(r, 1.0 / n)
    val an = theta / n
    Complex(rn * math.cos(an), rn * math.sin(an))

  // --------------------------------------------------------------------------
  // evalInterval: RadExpr[Rational] => Interval
  // --------------------------------------------------------------------------

  /** Evaluate a radical expression to an interval enclosure. */
  def evalInterval(expr: RadExpr[Rational]): Interval = expr match
    case Lit(r) =>
      Interval.point(r)
    case Neg(a) =>
      val iv = evalInterval(a)
      Interval(-iv.hi, -iv.lo)
    case Add(a, b) =>
      evalInterval(a) + evalInterval(b)
    case Mul(a, b) =>
      evalInterval(a) * evalInterval(b)
    case Inv(a) =>
      evalInterval(a).inverse
    case Root(n, a) =>
      Interval.inth(n, evalInterval(a))
    case Pow(a, n) =>
      evalInterval(a).pow(n)

  // --------------------------------------------------------------------------
  // evalComplexInterval: RadExpr[Rational] => ComplexInterval
  // --------------------------------------------------------------------------

  import Interval.ComplexInterval

  /** Evaluate a radical expression to a complex interval enclosure.
    * Handles expressions with complex intermediates (e.g., sqrt(-3)).
    */
  def evalComplexInterval(expr: RadExpr[Rational]): ComplexInterval = expr match
    case Lit(r) =>
      ComplexInterval.fromRational(r)
    case Neg(a) =>
      -evalComplexInterval(a)
    case Add(a, b) =>
      evalComplexInterval(a) + evalComplexInterval(b)
    case Mul(a, b) =>
      evalComplexInterval(a) * evalComplexInterval(b)
    case Inv(a) =>
      evalComplexInterval(a).inverse
    case Pow(a, n) =>
      evalComplexInterval(a).pow(n)
    case Root(n, a) =>
      val ci = evalComplexInterval(a)
      val rePart = ci.re
      val imPart = ci.im
      // Pure non-negative real
      if imPart.lo >= Rational.zero && imPart.hi <= Rational.zero && rePart.lo >= Rational.zero then
        ComplexInterval.fromReal(Interval.inth(n, rePart))
      // Negative real, odd root
      else if imPart.lo >= Rational.zero && imPart.hi <= Rational.zero
           && rePart.hi <= Rational.zero && n % 2 != 0 then
        val pos = Interval.inth(n, Interval(-rePart.hi, -rePart.lo))
        ComplexInterval(Interval(-pos.hi, -pos.lo), Interval.point(Rational.zero))
      // Negative real, square root => i * sqrt(|x|)
      else if imPart.lo >= Rational.zero && imPart.hi <= Rational.zero
           && rePart.hi <= Rational.zero && n == 2 then
        val pos = Interval.isqrt(Interval(-rePart.hi, -rePart.lo))
        ComplexInterval(Interval.point(Rational.zero), pos)
      // General complex root via polar form
      else
        ComplexInterval.nthRoot(n, ci)
