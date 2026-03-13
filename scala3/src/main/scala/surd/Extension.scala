package surd

/** Field extension K(alpha) where alpha is a root of an irreducible polynomial.
  *
  * Elements of K(alpha) are represented as polynomials of degree < deg(minpoly)
  * in alpha, with arithmetic modulo minpoly.
  */
final case class ExtElem[K](poly: Poly[K], minpoly: Poly[K])

object Extension:

  /** Create an element of K(alpha) from a polynomial in alpha. */
  def mkExt[K](p: Poly[K], minpoly: Poly[K])(using fld: Field[K]): ExtElem[K] =
    val reduced = if p.degree >= minpoly.degree then Poly.divMod(p, minpoly)._2 else p
    ExtElem(reduced, minpoly)

  /** The zero element. */
  def extZero[K](minpoly: Poly[K])(using fld: Field[K]): ExtElem[K] =
    ExtElem(Poly.zero[K], minpoly)

  /** The one element. */
  def extOne[K](minpoly: Poly[K])(using fld: Field[K]): ExtElem[K] =
    ExtElem(Poly.const(fld.one), minpoly)

  /** The generator alpha. */
  def extAlpha[K](minpoly: Poly[K])(using fld: Field[K]): ExtElem[K] =
    ExtElem(Poly.x[K], minpoly)

  /** Addition in K(alpha). */
  def extAdd[K](a: ExtElem[K], b: ExtElem[K])(using fld: Field[K]): ExtElem[K] =
    mkExt(Poly.add(a.poly, b.poly), a.minpoly)

  /** Subtraction in K(alpha). */
  def extSub[K](a: ExtElem[K], b: ExtElem[K])(using fld: Field[K]): ExtElem[K] =
    mkExt(Poly.sub(a.poly, b.poly), a.minpoly)

  /** Negation in K(alpha). */
  def extNeg[K](a: ExtElem[K])(using fld: Field[K]): ExtElem[K] =
    ExtElem(Poly.negate(a.poly), a.minpoly)

  /** Multiplication in K(alpha): multiply polynomials, reduce mod minpoly. */
  def extMul[K](a: ExtElem[K], b: ExtElem[K])(using fld: Field[K]): ExtElem[K] =
    mkExt(Poly.mul(a.poly, b.poly), a.minpoly)

  /** Multiplicative inverse via extended Euclidean algorithm.
    * Since minpoly is irreducible and deg(a.poly) < deg(minpoly),
    * gcd(a.poly, minpoly) = 1, so Bezout gives a.poly * s + minpoly * t = 1,
    * hence a^(-1) = s mod minpoly.
    */
  def extInv[K](a: ExtElem[K])(using fld: Field[K]): ExtElem[K] =
    require(!a.poly.isZero, "ExtElem: inverse of zero")
    val (_, s, _) = extendedGcd(a.poly, a.minpoly)
    mkExt(s, a.minpoly)

  /** Division in K(alpha). */
  def extDiv[K](a: ExtElem[K], b: ExtElem[K])(using fld: Field[K]): ExtElem[K] =
    extMul(a, extInv(b))

  /** Extended GCD: returns (gcd, s, t) such that a*s + b*t = gcd. */
  private def extendedGcd[K](a: Poly[K], b: Poly[K])(using fld: Field[K]): (Poly[K], Poly[K], Poly[K]) =
    if b.isZero then
      val lc = a.leadCoeff.getOrElse(fld.one)
      val lcInv = fld.inv(lc)
      (Poly.monic(a), Poly.const(lcInv), Poly.zero[K])
    else
      val (q, r) = Poly.divMod(a, b)
      val (g, s1, t1) = extendedGcd(b, r)
      (g, t1, Poly.sub(s1, Poly.mul(q, t1)))

  /** Construct a Field instance for K(alpha). */
  def extensionField[K](minpoly: Poly[K])(using fld: Field[K]): Field[ExtElem[K]] =
    new Field[ExtElem[K]]:
      def zero: ExtElem[K] = extZero(minpoly)
      def one: ExtElem[K] = extOne(minpoly)
      def add(a: ExtElem[K], b: ExtElem[K]): ExtElem[K] = extAdd(a, b)
      def mul(a: ExtElem[K], b: ExtElem[K]): ExtElem[K] = extMul(a, b)
      def negate(a: ExtElem[K]): ExtElem[K] = extNeg(a)
      def fromInt(n: Int): ExtElem[K] = ExtElem(Poly.const(fld.fromInt(n)), minpoly)
      def div(a: ExtElem[K], b: ExtElem[K]): ExtElem[K] = extDiv(a, b)
      def inv(a: ExtElem[K]): ExtElem[K] = extInv(a)
