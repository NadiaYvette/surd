package surd

/** Opaque type aliases for polynomials with additional invariants.
  *
  * These use Scala 3 opaque types to enforce at the type level that
  * a polynomial is square-free or monic, without runtime overhead.
  */

/** A polynomial guaranteed to be square-free (no repeated roots). */
opaque type SquareFreePoly = Poly[Rational]
object SquareFreePoly:
  /** Wrap a polynomial as square-free (caller must ensure the invariant). */
  def apply(p: Poly[Rational]): SquareFreePoly = p
  extension (p: SquareFreePoly)
    /** Unwrap to the underlying polynomial. */
    def underlying: Poly[Rational] = p

/** A polynomial guaranteed to be monic (leading coefficient = 1). */
opaque type MonicPoly = Poly[Rational]
object MonicPoly:
  /** Wrap a polynomial as monic (caller must ensure the invariant). */
  def apply(p: Poly[Rational]): MonicPoly = p
  extension (p: MonicPoly)
    /** Unwrap to the underlying polynomial. */
    def underlying: Poly[Rational] = p
