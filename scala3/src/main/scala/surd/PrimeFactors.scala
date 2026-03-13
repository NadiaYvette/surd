package surd

/** Prime factorization and related utilities via trial division. */
object PrimeFactors:

  /** Infinite lazy stream of primes via trial division. */
  lazy val primes: LazyList[BigInt] =
    BigInt(2) #:: LazyList.from(3, 2).map(BigInt(_)).filter(isPrime)

  /** Trial-division primality test (adequate for moderate-sized integers). */
  def isPrime(n: BigInt): Boolean =
    if n < 2 then false
    else if n < 4 then true
    else primes.takeWhile(p => p * p <= n).forall(p => n % p != 0)

  def isPrime(n: Int): Boolean = isPrime(BigInt(n))

  /** Factorise a positive integer into (prime, exponent) pairs in ascending order.
    *
    * {{{
    * scala> factorise(Positive.unsafe(360))
    * Vector((2,3), (3,2), (5,1))
    * }}}
    */
  def factorise(pos: Positive): Vector[(BigInt, Int)] =
    val n = pos.value
    if n == 1 then Vector.empty
    else
      val factors = trialDivide(n, primes)
      // Group consecutive equal primes and count
      groupConsecutive(factors)

  private def trialDivide(n: BigInt, ps: LazyList[BigInt]): Vector[BigInt] =
    if n == 1 then Vector.empty
    else
      ps match
        case p #:: rest =>
          if p * p > n then Vector(n)
          else if n % p == 0 then p +: trialDivide(n / p, ps)
          else trialDivide(n, rest)
        case _ => Vector.empty // unreachable

  /** Group a sorted vector of values into (value, count) pairs. */
  private def groupConsecutive(vs: Vector[BigInt]): Vector[(BigInt, Int)] =
    if vs.isEmpty then Vector.empty
    else
      val builder = Vector.newBuilder[(BigInt, Int)]
      var current = vs.head
      var count = 1
      for v <- vs.tail do
        if v == current then count += 1
        else
          builder += ((current, count))
          current = v
          count = 1
      builder += ((current, count))
      builder.result()

  /** Distinct prime factors in ascending order. */
  def primeFactors(pos: Positive): Vector[BigInt] =
    factorise(pos).map(_._1)
