package surd

/** Sturm sequences and root isolation for polynomials over Q.
  *
  * A Sturm sequence for a polynomial f is a sequence f_0 = f, f_1 = f',
  * f_{i+1} = -rem(f_{i-1}, f_i). The number of real roots of f in (a, b)
  * equals V(a) - V(b), where V(x) counts sign variations in the Sturm
  * sequence evaluated at x.
  */
object RootIsolation:

  /** Compute the Sturm sequence of a polynomial. */
  def sturmSequence(f: Poly[Rational])(using Field[Rational]): Vector[Poly[Rational]] =
    val fp = Poly.diff(f)
    if fp.isZero then Vector(f)
    else sturmSeqHelper(f, fp)

  private def sturmSeqHelper(a: Poly[Rational], b: Poly[Rational])(using Field[Rational]): Vector[Poly[Rational]] =
    if b.isZero then Vector(a)
    else
      val (_, r) = Poly.divMod(a, b)
      val negR = Poly.negate(r)
      a +: sturmSeqHelper(b, negR)

  /** Count sign variations in a sequence of rationals.
    * Ignoring zeros, count the number of consecutive sign changes.
    */
  def signVariations(values: Vector[Rational]): Int =
    val nonzero = values.filter(!_.isZero)
    if nonzero.length <= 1 then 0
    else
      nonzero.sliding(2).count { case Vector(a, b) =>
        a.signum != b.signum
      }

  /** Count the number of distinct real roots of f in the open interval (a, b).
    * Uses the Sturm sequence.
    */
  def countRoots(f: Poly[Rational], a: Rational, b: Rational)(using Field[Rational]): Int =
    val sturm = sturmSequence(f)
    val va = signVariations(sturm.map(p => Poly.eval(p, a)))
    val vb = signVariations(sturm.map(p => Poly.eval(p, b)))
    va - vb

  /** Isolate all real roots of a square-free polynomial into disjoint intervals.
    * Returns a vector of intervals, each containing exactly one root.
    */
  def isolateRoots(f: Poly[Rational])(using Field[Rational]): Vector[Interval] =
    if f.isZero || f.degree == 0 then Vector.empty
    else
      // Make square-free
      val sf = Poly.divMod(f, Poly.gcd(f, Poly.diff(f)))._1
      val bound = RootBound.rootBound(sf)
      val lo = -bound
      val hi = bound
      isolateInInterval(sf, lo, hi)

  /** Isolate roots within a given interval by recursive bisection. */
  private def isolateInInterval(
      f: Poly[Rational],
      lo: Rational,
      hi: Rational
  )(using Field[Rational]): Vector[Interval] =
    val n = countRoots(f, lo, hi)
    if n == 0 then Vector.empty
    else if n == 1 then Vector(Interval(lo, hi))
    else
      val mid = (lo + hi) / Rational(2)
      // Check if mid is a root
      val fMid = Poly.eval(f, mid)
      val midRoots = if fMid.isZero then Vector(Interval.point(mid)) else Vector.empty
      val leftRoots = isolateInInterval(f, lo, mid)
      val rightRoots = isolateInInterval(f, mid, hi)
      leftRoots ++ midRoots ++ rightRoots

  /** Refine an isolating interval to a given width. */
  def refineRoot(
      f: Poly[Rational],
      iv: Interval,
      targetWidth: Rational
  )(using Field[Rational]): Interval =
    var current = iv
    var fuel = 200
    while current.width > targetWidth && fuel > 0 do
      val mid = current.midpoint
      val fMid = Poly.eval(f, mid)
      if fMid.isZero then return Interval.point(mid)
      val fLo = Poly.eval(f, current.lo)
      if fLo.signum * fMid.signum < 0 then
        current = Interval(current.lo, mid)
      else
        current = Interval(mid, current.hi)
      fuel -= 1
    current

  /** Find a specific root near a given numerical approximation. */
  def findRootNear(
      f: Poly[Rational],
      approx: Double,
      tolerance: Rational = Rational(1, 1000000)
  )(using Field[Rational]): Option[Interval] =
    val intervals = isolateRoots(f)
    intervals.find { iv =>
      val mid = iv.midpoint.toDouble
      math.abs(mid - approx) < 1.0
    }.map(iv => refineRoot(f, iv, tolerance))
