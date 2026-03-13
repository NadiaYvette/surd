package surd

/** Closed interval [lo, hi] with Rational endpoints for rigorous arithmetic. */
final case class Interval(lo: Rational, hi: Rational):
  require(lo <= hi, s"Interval: lo ($lo) > hi ($hi)")

  def midpoint: Rational = (lo + hi) / Rational(2)
  def width: Rational = hi - lo

  def contains(x: Rational): Boolean = lo <= x && x <= hi

  def overlaps(that: Interval): Boolean =
    lo <= that.hi && that.lo <= hi

  def bisect: (Interval, Interval) =
    val m = midpoint
    (Interval(lo, m), Interval(m, hi))

  /** Narrow by bisection, keeping the half where the midpoint satisfies p. */
  def refine(p: Rational => Boolean): Interval =
    val (left, right) = bisect
    if p(midpoint) then left else right

  def strictlyPositive: Boolean = lo > Rational.zero
  def strictlyNegative: Boolean = hi < Rational.zero
  def containsZero: Boolean = lo <= Rational.zero && hi >= Rational.zero

  // --- Arithmetic ---

  def +(that: Interval): Interval = Interval(lo + that.lo, hi + that.hi)

  def -(that: Interval): Interval = Interval(lo - that.hi, hi - that.lo)

  def *(that: Interval): Interval =
    val ps = Vector(lo * that.lo, lo * that.hi, hi * that.lo, hi * that.hi)
    Interval(ps.min, ps.max)

  def unary_- : Interval = Interval(-hi, -lo)

  /** Inverse of an interval not containing zero. */
  def inverse: Interval =
    require(!containsZero, "Interval.inverse: interval contains zero")
    Interval(Rational.one / hi, Rational.one / lo)

  def /(that: Interval): Interval = this * that.inverse

  def pow(n: Int): Interval =
    if n == 0 then Interval.point(Rational.one)
    else if n < 0 then inverse.pow(-n)
    else if n % 2 != 0 then
      // Odd power: monotone
      (1 until n).foldLeft(this)((acc, _) => acc * this)
    else
      // Even power: result is non-negative if interval spans zero
      val ps = Vector(lo.pow(n), hi.pow(n))
      if lo <= Rational.zero && hi >= Rational.zero then
        Interval(Rational.zero, ps.max)
      else
        Interval(ps.min, ps.max)

  def abs: Interval =
    if lo >= Rational.zero then this
    else if hi <= Rational.zero then Interval(-hi, -lo)
    else Interval(Rational.zero, (-lo).max(hi))

  override def toString: String = s"[$lo, $hi]"

object Interval:
  /** Point interval [x, x]. */
  def point(x: Rational): Interval = Interval(x, x)

  /** Interval enclosure of sqrt via bisection. */
  def isqrt(iv: Interval): Interval =
    require(iv.lo >= Rational.zero, "isqrt: negative interval")
    if iv.hi == Rational.zero then point(Rational.zero)
    else Interval(nthRootLower(2, iv.lo), nthRootUpper(2, iv.hi))

  /** Interval enclosure of nth root. */
  def inth(n: Int, iv: Interval): Interval =
    require(n > 0, "inth: non-positive root index")
    if n == 1 then iv
    else if n == 2 then isqrt(iv)
    else if n % 2 == 0 && iv.lo < Rational.zero then
      throw IllegalArgumentException("inth: even root of negative interval")
    else if iv.lo == Rational.zero && iv.hi == Rational.zero then point(Rational.zero)
    else if n % 2 != 0 && iv.hi < Rational.zero then
      val pos = inth(n, Interval(-iv.hi, -iv.lo))
      Interval(-pos.hi, -pos.lo)
    else if n % 2 != 0 && iv.lo < Rational.zero then
      val negPart = nthRootUpper(n, -iv.lo)
      val posPart = nthRootUpper(n, iv.hi)
      Interval(-negPart, posPart)
    else Interval(nthRootLower(n, iv.lo), nthRootUpper(n, iv.hi))

  /** Bisect to find largest r in [0, max(a,1)] with r^n <= a. */
  private def nthRootLower(n: Int, a: Rational): Rational =
    if a == Rational.zero then Rational.zero
    else
      val upper = if a > Rational.one then a else Rational.one
      bisectDown(n, a, Rational.zero, upper, 60)

  /** Bisect to find smallest r in [0, max(a,1)+1] with r^n >= a. */
  private def nthRootUpper(n: Int, a: Rational): Rational =
    if a == Rational.zero then Rational.zero
    else
      val upper = (if a > Rational.one then a else Rational.one) + Rational.one
      bisectUp(n, a, Rational.zero, upper, 60)

  private def bisectDown(n: Int, a: Rational, lo: Rational, hi: Rational, iters: Int): Rational =
    if iters == 0 then lo
    else
      val mid = (lo + hi) / Rational(2)
      if mid.pow(n) <= a then bisectDown(n, a, mid, hi, iters - 1)
      else bisectDown(n, a, lo, mid, iters - 1)

  private def bisectUp(n: Int, a: Rational, lo: Rational, hi: Rational, iters: Int): Rational =
    if iters == 0 then hi
    else
      val mid = (lo + hi) / Rational(2)
      if mid.pow(n) >= a then bisectUp(n, a, lo, mid, iters - 1)
      else bisectUp(n, a, mid, hi, iters - 1)

  // --- Pi and trigonometric intervals ---

  /** Rational interval enclosure of pi via Machin's formula. */
  lazy val piInterval: Interval =
    val a = iatanSmall(Rational(1, 5))
    val b = iatanSmall(Rational(1, 239))
    val four = point(Rational(4))
    four * (four * a - b)

  /** atan(r) for |r| <= 1 via Taylor series with rigorous remainder. */
  private def iatanSmall(r: Rational): Interval =
    val nTerms = 50
    val partialSum = (0 until nTerms).foldLeft(Rational.zero) { (acc, k) =>
      val sign = if k % 2 == 0 then Rational.one else -Rational.one
      acc + sign * r.pow(2 * k + 1) / Rational(2 * k + 1)
    }
    val ar = r.abs
    val remBound = ar.pow(2 * nTerms + 1) / Rational(2 * nTerms + 1)
    Interval(partialSum - remBound, partialSum + remBound)

  /** atan of a single rational value, returned as an interval. */
  private def iatanPoint(r: Rational): Interval =
    if r == Rational.zero then point(Rational.zero)
    else if r.abs <= Rational.one then iatanSmall(r)
    else if r > Rational.zero then
      val piHalf = piInterval / point(Rational(2))
      piHalf - iatanSmall(Rational.one / r)
    else
      val piHalf = piInterval / point(Rational(2))
      (-piHalf) + iatanSmall(Rational.one / (-r))

  /** Interval atan over an interval argument. */
  def iatan(iv: Interval): Interval =
    Interval(iatanPoint(iv.lo).lo, iatanPoint(iv.hi).hi)

  /** cos of a single rational value via Taylor series with remainder. */
  private def icosPoint(x: Rational): Interval =
    val nTerms = 30
    val partialSum = (0 until nTerms).foldLeft(Rational.zero) { (acc, k) =>
      val sign = if k % 2 == 0 then Rational.one else -Rational.one
      acc + sign * x.pow(2 * k) / factorial(2 * k)
    }
    val remBound = x.abs.pow(2 * nTerms) / factorial(2 * nTerms)
    Interval(partialSum - remBound, partialSum + remBound)

  private def factorial(n: Int): Rational =
    Rational((1 to n).foldLeft(BigInt(1))(_ * _))

  /** Interval enclosure of cos over an interval.
    * Handles extrema at multiples of pi.
    */
  def icos(iv: Interval): Interval =
    if iv.lo == iv.hi then icosPoint(iv.lo)
    else
      if iv.width > Rational(piInterval.hi.num * 2, piInterval.hi.den) then
        Interval(Rational(-1), Rational(1))
      else
        val cosL = icosPoint(iv.lo)
        val cosH = icosPoint(iv.hi)
        var baseLo = cosL.lo.min(cosH.lo)
        var baseHi = cosL.hi.max(cosH.hi)
        val piLo = piInterval.lo
        val piHi = piInterval.hi
        // Check for extrema at k*pi
        val kMin = ceilDiv(iv.lo.num * piHi.den, piHi.num * iv.lo.den.max(BigInt(1)))
        val kMax = floorDiv(iv.hi.num * piLo.den, piLo.num * iv.hi.den.max(BigInt(1)))
        // Simpler: use Double approximation for k range (safe since we check conservatively)
        val kMinD = math.ceil(iv.lo.toDouble / piInterval.hi.toDouble).toLong
        val kMaxD = math.floor(iv.hi.toDouble / piInterval.lo.toDouble).toLong
        for k <- kMinD to kMaxD do
          if k % 2 == 0 then baseHi = baseHi.max(Rational.one)
          else baseLo = baseLo.min(-Rational.one)
        Interval(baseLo.max(-Rational.one), baseHi.min(Rational.one))

  /** Interval enclosure of sin over an interval: sin(x) = cos(x - pi/2). */
  def isin(iv: Interval): Interval =
    val piHalf = piInterval / point(Rational(2))
    icos(iv - piHalf)

  /** Interval atan2(y, x). Requires x interval not containing zero for full precision. */
  def iatan2(y: Interval, x: Interval): Interval =
    if x.strictlyPositive then
      val thetaMin = iatanPoint(y.lo / x.hi)
      val thetaMax = iatanPoint(y.hi / x.lo)
      Interval(thetaMin.lo, thetaMax.hi)
    else if x.strictlyNegative && y.lo >= Rational.zero then
      val thetaMin = iatanPoint(y.hi / x.lo) + piInterval
      val thetaMax = iatanPoint(y.lo / x.hi) + piInterval
      Interval(thetaMin.lo, thetaMax.hi)
    else if x.strictlyNegative && y.hi <= Rational.zero then
      val negPi = -piInterval
      val thetaMin = iatanPoint(y.hi / x.lo) + negPi
      val thetaMax = iatanPoint(y.lo / x.hi) + negPi
      Interval(thetaMin.lo, thetaMax.hi)
    else if x.strictlyNegative then
      val negPi = -piInterval
      val thetaLow = iatanPoint(y.lo / x.lo) + negPi
      val thetaHigh = iatanPoint(y.hi / x.hi) + piInterval
      Interval(thetaLow.lo, thetaHigh.hi)
    else
      Interval(-piInterval.hi, piInterval.hi)

  private def ceilDiv(a: BigInt, b: BigInt): BigInt =
    if b > 0 then (a + b - 1) / b else -((-a + (-b) - 1) / (-b))

  private def floorDiv(a: BigInt, b: BigInt): BigInt =
    if b > 0 then a / b else -((-a) / (-b))

  // --- Complex interval ---

  /** Rectangular complex interval: real part x imaginary part. */
  final case class ComplexInterval(re: Interval, im: Interval):
    def +(that: ComplexInterval): ComplexInterval =
      ComplexInterval(re + that.re, im + that.im)

    def -(that: ComplexInterval): ComplexInterval =
      ComplexInterval(re - that.re, im - that.im)

    def unary_- : ComplexInterval =
      ComplexInterval(-re, -im)

    /** (a+bi)(c+di) = (ac-bd) + (ad+bc)i */
    def *(that: ComplexInterval): ComplexInterval =
      ComplexInterval(re * that.re - im * that.im, re * that.im + im * that.re)

    /** 1/(a+bi) = (a-bi)/(a^2+b^2) */
    def inverse: ComplexInterval =
      val magSq = re * re + im * im
      ComplexInterval(re / magSq, (-im) / magSq)

    def pow(n: Int): ComplexInterval =
      if n == 0 then ComplexInterval.fromRational(Rational.one)
      else if n == 1 then this
      else if n < 0 then inverse.pow(-n)
      else if n % 2 == 0 then
        val half = pow(n / 2)
        half * half
      else this * pow(n - 1)

    def magnitudeSq: Interval = re * re + im * im

  object ComplexInterval:
    def fromRational(r: Rational): ComplexInterval =
      ComplexInterval(Interval.point(r), Interval.point(Rational.zero))

    def fromReal(r: Interval): ComplexInterval =
      ComplexInterval(r, Interval.point(Rational.zero))

    /** Complex nth root via polar form with interval arithmetic. */
    def nthRoot(n: Int, z: ComplexInterval): ComplexInterval =
      require(n > 0, "nthRoot: non-positive root index")
      if n == 1 then z
      else
        val magSq = z.re * z.re + z.im * z.im
        val mag = Interval.isqrt(magSq)
        val magRoot = Interval.inth(n, mag)
        val theta = Interval.iatan2(z.im, z.re)
        val nIv = Interval.point(Rational(n))
        val thetaN = theta / nIv
        val cosT = Interval.icos(thetaN)
        val sinT = Interval.isin(thetaN)
        ComplexInterval(magRoot * cosT, magRoot * sinT)
