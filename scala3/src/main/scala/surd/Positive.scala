package surd

/** A strictly positive integer (> 0).
  *
  * Implemented as an opaque type over BigInt for zero-cost abstraction.
  * Construct via `Positive.apply` (checked) or `Positive.unsafe` (unchecked).
  */
opaque type Positive = BigInt

object Positive:
  /** Smart constructor: returns None for non-positive values. */
  def apply(n: BigInt): Option[Positive] =
    if n > 0 then Some(n) else None

  /** Smart constructor from Int. */
  def apply(n: Int): Option[Positive] = apply(BigInt(n))

  /** Unsafe constructor for cases where positivity is guaranteed.
    * Throws IllegalArgumentException for non-positive values.
    */
  def unsafe(n: BigInt): Positive =
    require(n > 0, s"Positive.unsafe: non-positive value $n")
    n

  def unsafe(n: Int): Positive = unsafe(BigInt(n))

  /** Literal constructor for known-at-write-time constants. */
  def literal(n: Int): Positive =
    require(n > 0, s"Positive.literal: non-positive value $n")
    BigInt(n)

  extension (p: Positive)
    def value: BigInt = p
    def toInt: Int = p.toInt
    def toLong: Long = p.toLong

    def +(q: Positive): Positive = p + q
    def *(q: Positive): Positive = p * q
