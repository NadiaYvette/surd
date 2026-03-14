package surd

/** Dense univariate polynomial with coefficients in K, stored low-degree first.
  *
  * `coeffs(i)` is the coefficient of x^i.
  * Invariant: no trailing zeros (leading coefficient is nonzero), except
  * the zero polynomial which has an empty Vector.
  *
  * Requires a Ring[K] for construction and most operations, Field[K] for division.
  */
final class Poly[K] private (val coeffs: Vector[K]):

  /** Degree of the polynomial. Returns -1 for the zero polynomial. */
  def degree: Int = coeffs.length - 1

  /** Leading coefficient, or None for the zero polynomial. */
  def leadCoeff: Option[K] =
    if coeffs.isEmpty then None else Some(coeffs.last)

  def isZero: Boolean = coeffs.isEmpty

  override def equals(that: Any): Boolean = that match
    case p: Poly[?] => coeffs == p.coeffs
    case _          => false

  override def hashCode: Int = coeffs.hashCode

  override def toString: String =
    if coeffs.isEmpty then "Poly(0)"
    else s"Poly(${coeffs.mkString(", ")})"

object Poly:
  /** Smart constructor: strips trailing zeros. */
  def apply[K](cs: Vector[K])(using r: Ring[K]): Poly[K] =
    new Poly(stripZeros(cs)(using r))

  def apply[K](cs: K*)(using r: Ring[K]): Poly[K] =
    new Poly(stripZeros(cs.toVector)(using r))

  private def stripZeros[K](cs: Vector[K])(using r: Ring[K]): Vector[K] =
    cs.reverse.dropWhile(_ == r.zero).reverse

  /** The zero polynomial. */
  def zero[K]: Poly[K] = new Poly(Vector.empty)

  /** A constant polynomial. */
  def const[K](c: K)(using r: Ring[K]): Poly[K] =
    if c == r.zero then zero else new Poly(Vector(c))

  /** The polynomial x. */
  def x[K](using r: Ring[K]): Poly[K] = new Poly(Vector(r.zero, r.one))

  /** Monomial c * x^n. */
  def monomial[K](c: K, n: Int)(using r: Ring[K]): Poly[K] =
    if c == r.zero then zero
    else new Poly(Vector.fill(n)(r.zero) :+ c)

  // --- Arithmetic operations ---

  /** Evaluate at a point via Horner's method. */
  inline def eval[K](p: Poly[K], at: K)(using r: Ring[K]): K =
    if p.isZero then r.zero
    else p.coeffs.foldRight(r.zero)((c, acc) => r.add(c, r.mul(at, acc)))

  /** Scalar multiplication. */
  def scale[K](s: K, p: Poly[K])(using r: Ring[K]): Poly[K] =
    if s == r.zero then zero
    else Poly(p.coeffs.map(c => r.mul(s, c)))

  /** Addition. */
  def add[K](a: Poly[K], b: Poly[K])(using r: Ring[K]): Poly[K] =
    val len = math.max(a.coeffs.length, b.coeffs.length)
    val cs = Vector.tabulate(len) { i =>
      val ai = if i < a.coeffs.length then a.coeffs(i) else r.zero
      val bi = if i < b.coeffs.length then b.coeffs(i) else r.zero
      r.add(ai, bi)
    }
    Poly(cs)

  /** Subtraction. */
  def sub[K](a: Poly[K], b: Poly[K])(using r: Ring[K]): Poly[K] =
    val len = math.max(a.coeffs.length, b.coeffs.length)
    val cs = Vector.tabulate(len) { i =>
      val ai = if i < a.coeffs.length then a.coeffs(i) else r.zero
      val bi = if i < b.coeffs.length then b.coeffs(i) else r.zero
      r.sub(ai, bi)
    }
    Poly(cs)

  /** Negation. */
  def negate[K](p: Poly[K])(using r: Ring[K]): Poly[K] =
    new Poly(p.coeffs.map(r.negate))

  /** Schoolbook multiplication. */
  def mul[K](a: Poly[K], b: Poly[K])(using r: Ring[K]): Poly[K] =
    if a.isZero || b.isZero then zero
    else
      val rlen = a.coeffs.length + b.coeffs.length - 1
      val arr = Array.fill[Any](rlen)(r.zero)
      for
        i <- a.coeffs.indices
        j <- b.coeffs.indices
      do
        val idx = i + j
        arr(idx) = r.add(arr(idx).asInstanceOf[K], r.mul(a.coeffs(i), b.coeffs(j)))
      Poly(arr.toVector.asInstanceOf[Vector[K]])

  /** Polynomial division with remainder.
    * Returns (q, r) such that a = b*q + r and degree(r) < degree(b).
    * Requires Field[K] for coefficient division.
    */
  def divMod[K](a: Poly[K], b: Poly[K])(using f: Field[K]): (Poly[K], Poly[K]) =
    require(!b.isZero, "Poly.divMod: division by zero polynomial")
    if a.degree < b.degree then (zero, a)
    else
      val lc = b.leadCoeff.get
      var q = zero[K]
      var r = a
      while r.degree >= b.degree do
        val lr = r.leadCoeff.get
        val c = f.div(lr, lc)
        val d = r.degree - b.degree
        val term = monomial(c, d)
        q = add(q, term)
        r = sub(r, mul(term, b))
      (q, r)

  /** GCD via Euclidean algorithm, result made monic. */
  def gcd[K](a: Poly[K], b: Poly[K])(using f: Field[K]): Poly[K] =
    if b.isZero then monic(a)
    else gcd(b, divMod(a, b)._2)

  /** Make monic (leading coefficient 1). */
  def monic[K](p: Poly[K])(using f: Field[K]): Poly[K] =
    p.leadCoeff match
      case None     => zero
      case Some(lc) => Poly(p.coeffs.map(c => f.div(c, lc)))

  /** Formal derivative. */
  def diff[K](p: Poly[K])(using r: Ring[K]): Poly[K] =
    if p.coeffs.length <= 1 then zero
    else
      val cs = p.coeffs.drop(1).zipWithIndex.map { (c, i) =>
        r.mul(r.fromInt(i + 1), c)
      }
      Poly(cs)

  /** Composition: compose(f, g) = f(g(x)). */
  def compose[K](f: Poly[K], g: Poly[K])(using r: Ring[K]): Poly[K] =
    if f.isZero then zero
    else
      f.coeffs.foldRight(zero[K]) { (c, acc) =>
        add(const(c), mul(g, acc))
      }

  /** Square-free factorisation via Yun's algorithm (characteristic 0).
    * Returns (factor, multiplicity) pairs.
    */
  def squareFree[K](f: Poly[K])(using fld: Field[K]): Vector[(Poly[K], Int)] =
    if f.isZero then Vector.empty
    else
      val fp = diff(f)
      val c = gcd(f, fp)
      val w = divMod(f, c)._1
      yun(w, c, 1)

  private def yun[K](w: Poly[K], c: Poly[K], i: Int)(using fld: Field[K]): Vector[(Poly[K], Int)] =
    if w.degree == 0 then
      if c.degree > 0 then Vector((c, i))
      else Vector.empty
    else
      val y = gcd(w, c)
      val z = divMod(w, y)._1
      val cp = divMod(c, y)._1
      val rest = yun(y, cp, i + 1)
      if z.degree > 0 then (z, i) +: rest else rest

  // --- Extension methods for operator syntax ---

  extension [K](p: Poly[K])
    def +(q: Poly[K])(using Ring[K]): Poly[K] = add(p, q)
    def -(q: Poly[K])(using Ring[K]): Poly[K] = sub(p, q)
    def *(q: Poly[K])(using Ring[K]): Poly[K] = mul(p, q)
    def unary_-(using Ring[K]): Poly[K] = negate(p)
    def /%(q: Poly[K])(using Field[K]): (Poly[K], Poly[K]) = divMod(p, q)
    def evalAt(x: K)(using Ring[K]): K = eval(p, x)
