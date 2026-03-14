package surd

/** Typeclass for a commutative ring with unity. */
trait Ring[K]:
  def zero: K
  def one: K
  def add(a: K, b: K): K
  def sub(a: K, b: K): K = add(a, negate(b))
  def mul(a: K, b: K): K
  def negate(a: K): K
  def fromInt(n: Int): K

  extension (a: K)
    def +(b: K): K = add(a, b)
    def -(b: K): K = sub(a, b)
    def *(b: K): K = mul(a, b)
    def unary_- : K = negate(a)

/** Typeclass for a field (ring with division). */
trait Field[K] extends Ring[K]:
  def div(a: K, b: K): K
  def inv(a: K): K

  extension (a: K)
    def /(b: K): K = div(a, b)

object Ring:
  def apply[K](using r: Ring[K]): Ring[K] = r

  given Ring[Int] with
    inline def zero: Int = 0
    inline def one: Int = 1
    def add(a: Int, b: Int): Int = a + b
    def mul(a: Int, b: Int): Int = a * b
    def negate(a: Int): Int = -a
    def fromInt(n: Int): Int = n

  given Ring[BigInt] with
    inline def zero: BigInt = BigInt(0)
    inline def one: BigInt = BigInt(1)
    def add(a: BigInt, b: BigInt): BigInt = a + b
    def mul(a: BigInt, b: BigInt): BigInt = a * b
    def negate(a: BigInt): BigInt = -a
    def fromInt(n: Int): BigInt = BigInt(n)

  given Field[Double] with
    inline def zero: Double = 0.0
    inline def one: Double = 1.0
    def add(a: Double, b: Double): Double = a + b
    def mul(a: Double, b: Double): Double = a * b
    def negate(a: Double): Double = -a
    def fromInt(n: Int): Double = n.toDouble
    def div(a: Double, b: Double): Double = a / b
    def inv(a: Double): Double = 1.0 / a

object Field:
  def apply[K](using f: Field[K]): Field[K] = f
