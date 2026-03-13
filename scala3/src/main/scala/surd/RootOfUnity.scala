package surd

import RadExpr.*

/** Express roots of unity as radical expressions.
  *
  * A primitive nth root of unity can always be expressed in radicals,
  * since cyclotomic extensions have abelian Galois groups.
  *
  * The constructible case (n = 2^a * distinct Fermat primes) uses
  * only square roots. General n uses higher-degree roots.
  */
object RootOfUnity:

  /** Known Fermat primes. */
  val fermatPrimes: Vector[Int] = Vector(3, 5, 17, 257, 65537)

  /** Check if cos(2pi/n) is constructible (only square roots needed). */
  def isConstructible(n: Int): Boolean =
    if n <= 0 then false
    else
      val pos = Positive.unsafe(n)
      val factors = PrimeFactors.factorise(pos)
      factors.forall { case (p, e) =>
        p == BigInt(2) || (e == 1 && fermatPrimes.contains(p.toInt))
      }

  /** Compute cos(2pi/n) as a radical expression. */
  def cosOfUnity(n: Int): Option[RadExpr[Rational]] =
    if n <= 0 then None
    else if n == 1 then Some(Lit(Rational.one))
    else if n == 2 then Some(Lit(-Rational.one))
    else if n == 3 then Some(Lit(Rational(-1, 2)))
    else if n == 4 then Some(Lit(Rational.zero))
    else if n == 5 then Some(cos2piOver5)
    else if n == 6 then Some(Lit(Rational(1, 2)))
    else if n == 8 then Some(Mul(Inv(Lit(Rational(2))), Root(2, Lit(Rational(2)))))
    else if n == 10 then Some(cos2piOver10)
    else if n == 12 then Some(Mul(Inv(Lit(Rational(2))), Root(2, Lit(Rational(3)))))
    else if Cyclotomic.isPowerOf2(n) then Some(cosOfPow2(n))
    else if PrimeFactors.isPrime(n) then TrigGalois.cosOfUnityViaGauss(n)
    else
      // Composite: try CRT decomposition or Gauss periods
      Cyclotomic.primePowerDecomp(n) match
        case Some((p, k)) if p > 2 && k > 1 =>
          TrigGalois.cosOfUnityViaGauss(n)
        case _ =>
          // General composite: use Chebyshev or CRT
          compositeCos(n)

  /** Compute sin(2pi/n) as a radical expression. */
  def sinOfUnity(n: Int): Option[RadExpr[Rational]] =
    if n <= 0 then None
    else if n == 1 then Some(Lit(Rational.zero))
    else if n == 2 then Some(Lit(Rational.zero))
    else if n == 3 then Some(Mul(Inv(Lit(Rational(2))), Root(2, Lit(Rational(3)))))
    else if n == 4 then Some(Lit(Rational.one))
    else if n == 6 then Some(Mul(Inv(Lit(Rational(2))), Root(2, Lit(Rational(3)))))
    else
      // sin(2pi/n) = sqrt(1 - cos^2(2pi/n)) for most cases
      cosOfUnity(n).map { cosE =>
        val cos2 = Pow(cosE, 2)
        val sinSquared = RadExpr.sub(Lit(Rational.one), cos2)
        Root(2, Normalize.normalize(sinSquared))
      }

  /** All cos(2pi*k/n) for k coprime to n. */
  def allCosOfUnity(n: Int): Option[Map[Int, RadExpr[Rational]]] =
    TrigGalois.allPeriodsViaGauss(n).map { periods =>
      periods.map { case (k, zetaK) =>
        val conjK = ((n - k) % n + n) % n
        val conjExpr = periods.getOrElse(conjK, zetaK)
        k -> Normalize.normalize(
          Mul(Inv(Lit(Rational(2))), Add(zetaK, conjExpr))
        )
      }
    }

  /** All sin(2pi*k/n) for k coprime to n. */
  def allSinOfUnity(n: Int): Option[Map[Int, RadExpr[Rational]]] =
    TrigGalois.allPeriodsViaGauss(n).map { periods =>
      periods.map { case (k, zetaK) =>
        val conjK = ((n - k) % n + n) % n
        val conjExpr = periods.getOrElse(conjK, zetaK)
        // sin = (zeta - conj(zeta)) / (2i) = -i/2 * (zeta - conj(zeta))
        val diff = RadExpr.sub(zetaK, conjExpr)
        val iExpr = Root(2, Lit(-Rational.one))
        val result = Mul(Mul(Neg(iExpr), Inv(Lit(Rational(2)))), diff)
        k -> Normalize.normalize(result)
      }
    }

  // --- Hand-optimised values ---

  /** cos(2pi/5) = (-1 + sqrt(5)) / 4. */
  private def cos2piOver5: RadExpr[Rational] =
    Mul(Inv(Lit(Rational(4))),
      Add(Lit(-Rational.one), Root(2, Lit(Rational(5)))))

  /** cos(2pi/10) = cos(pi/5) = (1 + sqrt(5)) / 4. */
  private def cos2piOver10: RadExpr[Rational] =
    Mul(Inv(Lit(Rational(4))),
      Add(Lit(Rational.one), Root(2, Lit(Rational(5)))))

  /** cos(2pi/2^k) via half-angle recurrence. */
  private def cosOfPow2(n: Int): RadExpr[Rational] =
    if n <= 4 then cosOfUnity(n).getOrElse(Lit(Rational.zero))
    else
      // cos(2pi/2^k) = sqrt((1 + cos(2pi/2^{k-1})) / 2)
      val prev = cosOfPow2(n / 2)
      val inner = Mul(Inv(Lit(Rational(2))), Add(Lit(Rational.one), prev))
      Root(2, Normalize.normalize(inner))

  /** Handle composite n via CRT decomposition. */
  private def compositeCos(n: Int): Option[RadExpr[Rational]] =
    // For composite n, cos(2pi/n) can sometimes be expressed via
    // cos values of the prime power factors using product formulas.
    // This is a simplified version.
    TrigGalois.cosOfUnityViaGauss(n).orElse {
      // Fallback: check if n has a primitive root
      // (2*odd prime power has a primitive root)
      val pos = Positive.unsafe(n)
      val factors = PrimeFactors.factorise(pos)
      if factors.length == 1 then TrigGalois.cosOfUnityViaGauss(n)
      else if factors.length == 2 then
        val (p1, e1) = (factors(0)._1.toInt, factors(0)._2)
        val (p2, e2) = (factors(1)._1.toInt, factors(1)._2)
        if p1 == 2 && e1 == 1 then
          // n = 2 * odd prime power: has primitive root
          TrigGalois.cosOfUnityViaGauss(n)
        else None
      else None
    }
