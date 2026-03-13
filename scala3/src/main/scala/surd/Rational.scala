package surd

/** Exact rational number with automatic GCD normalization.
  *
  * Invariant: den > 0, gcd(|num|, den) == 1.
  * The zero rational is Rational(0, 1).
  */
final case class Rational private (num: BigInt, den: BigInt)
    extends Ordered[Rational]:

  def isZero: Boolean = num == 0
  def isOne: Boolean = num == 1 && den == 1

  def +(that: Rational): Rational =
    Rational.normalize(num * that.den + that.num * den, den * that.den)

  def -(that: Rational): Rational =
    Rational.normalize(num * that.den - that.num * den, den * that.den)

  def *(that: Rational): Rational =
    Rational.normalize(num * that.num, den * that.den)

  def /(that: Rational): Rational =
    require(!that.isZero, "Rational division by zero")
    Rational.normalize(num * that.den, den * that.num)

  def unary_- : Rational = new Rational(-num, den)

  def inverse: Rational =
    require(!isZero, "Rational inverse of zero")
    Rational.normalize(den, num)

  def abs: Rational = if num < 0 then Rational(-num, den) else this

  def pow(n: Int): Rational =
    if n == 0 then Rational.one
    else if n < 0 then inverse.pow(-n)
    else
      var result = Rational.one
      var base = this
      var exp = n
      while exp > 0 do
        if (exp & 1) == 1 then result = result * base
        base = base * base
        exp >>= 1
      result

  def min(that: Rational): Rational = if this <= that then this else that
  def max(that: Rational): Rational = if this >= that then this else that

  def signum: Int = num.signum

  def compare(that: Rational): Int =
    (num * that.den).compare(that.num * den)

  def toDouble: Double = num.toDouble / den.toDouble

  override def toString: String =
    if den == 1 then num.toString
    else s"$num/$den"

object Rational:
  private def normalize(n: BigInt, d: BigInt): Rational =
    if n == 0 then new Rational(BigInt(0), BigInt(1))
    else
      val sign = if d < 0 then BigInt(-1) else BigInt(1)
      val g = n.gcd(d)
      new Rational(sign * n / g, sign * d / g)

  def apply(n: BigInt, d: BigInt): Rational =
    require(d != 0, "Rational denominator cannot be zero")
    normalize(n, d)

  def apply(n: BigInt): Rational = Rational(n, BigInt(1))
  def apply(n: Int): Rational = Rational(BigInt(n), BigInt(1))
  def apply(n: Long): Rational = Rational(BigInt(n), BigInt(1))
  def apply(n: Int, d: Int): Rational = Rational(BigInt(n), BigInt(d))

  val zero: Rational = Rational(BigInt(0), BigInt(1))
  val one: Rational = Rational(BigInt(1), BigInt(1))

  given Ordering[Rational] = (a, b) => a.compare(b)

  given Field[Rational] with
    def zero: Rational = Rational.zero
    def one: Rational = Rational.one
    def add(a: Rational, b: Rational): Rational = a + b
    def mul(a: Rational, b: Rational): Rational = a * b
    def negate(a: Rational): Rational = -a
    def fromInt(n: Int): Rational = Rational(n)
    def div(a: Rational, b: Rational): Rational = a / b
    def inv(a: Rational): Rational = a.inverse

  /** Implicit conversion from Int to Rational for convenience. */
  given Conversion[Int, Rational] = Rational(_)
