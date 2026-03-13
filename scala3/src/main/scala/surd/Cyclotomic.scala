package surd

/** Cyclotomic polynomial construction.
  *
  * The nth cyclotomic polynomial Phi_n(x) is the minimal polynomial of
  * primitive nth roots of unity over Q. It satisfies:
  *   x^n - 1 = product of Phi_d(x) for d | n
  *
  * Computed via Moebius inversion / iterated division.
  */
object Cyclotomic:

  /** Compute the nth cyclotomic polynomial over Q. */
  def cyclotomic(n: Int)(using Field[Rational]): Poly[Rational] =
    require(n >= 1, s"Cyclotomic: n must be >= 1, got $n")
    if n == 1 then
      // Phi_1(x) = x - 1
      Poly(Vector(Rational(-1), Rational.one))
    else
      // x^n - 1, then divide by Phi_d for each proper divisor d of n
      val xnm1 = xnMinus1(n)
      val divs = properDivisors(n)
      divs.foldLeft(xnm1) { (acc, d) =>
        val phiD = cyclotomic(d)
        Poly.divMod(acc, phiD)._1
      }

  /** x^n - 1 as a polynomial over Rational. */
  private def xnMinus1(n: Int)(using Ring[Rational]): Poly[Rational] =
    val cs = Vector.fill(n)(Rational.zero) :+ Rational.one
    Poly(cs) - Poly.const(Rational.one)

  /** All proper divisors of n (divisors < n), in ascending order. */
  private def properDivisors(n: Int): Vector[Int] =
    (1 until n).filter(d => n % d == 0).toVector

  /** All divisors of n, in ascending order. */
  def divisors(n: Int): Vector[Int] =
    (1 to n).filter(d => n % d == 0).toVector

  /** Euler's totient function. */
  def eulerTotient(n: Int): Int =
    if n <= 0 then 0
    else
      val pos = Positive.unsafe(n)
      val factors = PrimeFactors.factorise(pos)
      factors.foldLeft(n) { case (acc, (p, _)) =>
        acc / p.toInt * (p.toInt - 1)
      }

  /** Check if n is a prime power. Returns Some((p, k)) if n = p^k. */
  def primePowerDecomp(n: Int): Option[(Int, Int)] =
    if n <= 1 then None
    else
      val pos = Positive.unsafe(n)
      val factors = PrimeFactors.factorise(pos)
      if factors.length == 1 then
        val (p, k) = factors.head
        Some((p.toInt, k))
      else None

  /** Check if n is a power of 2. */
  def isPowerOf2(n: Int): Boolean =
    n > 0 && (n & (n - 1)) == 0
