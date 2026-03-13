package surd

import RadExpr.*

/** Top-level trigonometric functions: cosExact and sinExact.
  *
  * Computes exact radical expressions for cos(p*pi/q) and sin(p*pi/q).
  *
  * Strategy:
  *   1. Reduce to first quadrant
  *   2. Use cos(2pi/n) lookup + Chebyshev if needed
  *   3. Apply symmetries: cos(pi-x) = -cos(x), etc.
  */
object Trig:

  /** Result of an exact trig computation. */
  enum TrigResult:
    case Radical(expr: RadExpr[Rational])
    case MinPoly(poly: Poly[Rational])

  /** Compute cos(p*pi/q) as a radical expression.
    * p and q are integers, q > 0.
    */
  def cosExact(p: Long, q: Long): TrigResult =
    require(q > 0, "cosExact: denominator must be positive")

    // Reduce p mod 2q (period of cos is 2pi)
    val period = 2 * q
    val pp = ((p % period) + period) % period

    // Symmetries
    if pp == 0 then return TrigResult.Radical(Lit(Rational.one))
    if pp == q then return TrigResult.Radical(Lit(-Rational.one))
    if pp * 2 == q then return TrigResult.Radical(Lit(Rational.zero)) // cos(pi/2)
    if pp * 2 == 3 * q then return TrigResult.Radical(Lit(Rational.zero)) // cos(3pi/2)

    // cos(pi - x) = -cos(x)
    if pp > q then
      val r = cosExact(2 * q - pp, q)
      return negateResult(r)

    // cos(pi/2 - x) = sin(x) -- don't use this, work with cos directly

    // Now 0 < pp < q, and we need cos(pp*pi/q)
    // cos(pp*pi/q) = cos(2*pi*(pp)/(2q))
    // So this is related to cos(2pi/n) where n = 2q / gcd(pp, 2q)
    val g = gcd(pp, 2 * q)
    val n = (2 * q / g).toInt
    val k = (pp / g).toInt

    if k == 0 then return TrigResult.Radical(Lit(Rational.one))
    if n <= 2 then return TrigResult.Radical(Lit(if k % 2 == 0 then Rational.one else -Rational.one))

    // cos(2*pi*k/n) via Chebyshev: cos(k*theta) = T_k(cos(theta))
    if k == 1 then
      RootOfUnity.cosOfUnity(n) match
        case Some(expr) => TrigResult.Radical(Normalize.normalize(expr))
        case None => TrigResult.Radical(Lit(Rational.zero)) // fallback
    else
      // Use Chebyshev polynomial: cos(k*theta) = T_k(cos(theta))
      RootOfUnity.cosOfUnity(n) match
        case Some(cosBase) =>
          val cheb = chebyshevT(k, cosBase)
          TrigResult.Radical(Normalize.normalize(cheb))
        case None =>
          TrigResult.Radical(Lit(Rational.zero)) // fallback

  /** Compute sin(p*pi/q) as a radical expression. */
  def sinExact(p: Long, q: Long): TrigResult =
    require(q > 0, "sinExact: denominator must be positive")

    // sin(x) = cos(pi/2 - x)
    // sin(p*pi/q) = cos(pi/2 - p*pi/q) = cos((q - 2p)*pi/(2q))
    val newP = q - 2 * p
    val newQ = 2 * q
    cosExact(newP, newQ)

  /** Simplify a TrigResult (try normalization, NF round-trip, etc.). */
  def simplifyTrigResult(tr: TrigResult): TrigResult = tr match
    case TrigResult.Radical(expr) =>
      val normalized = Normalize.normalize(expr)
      // Try NF round-trip for further simplification
      try
        val nf = NormalForm.toNormExpr(normalized)
        val back = NormalForm.fromNormExpr(nf)
        TrigResult.Radical(Normalize.normalize(back))
      catch
        case _: Exception => TrigResult.Radical(normalized)
    case mp => mp

  /** Chebyshev polynomial T_n(x) evaluated at x = expr.
    * T_0(x) = 1, T_1(x) = x, T_{n+1}(x) = 2x*T_n(x) - T_{n-1}(x)
    */
  private def chebyshevT(n: Int, x: RadExpr[Rational]): RadExpr[Rational] =
    if n == 0 then Lit(Rational.one)
    else if n == 1 then x
    else
      var tPrev = Lit(Rational.one): RadExpr[Rational]
      var tCurr = x
      for _ <- 2 to n do
        val tNext = RadExpr.sub(
          Mul(Mul(Lit(Rational(2)), x), tCurr),
          tPrev
        )
        tPrev = tCurr
        tCurr = tNext
      tCurr

  /** Negate a TrigResult. */
  private def negateResult(tr: TrigResult): TrigResult = tr match
    case TrigResult.Radical(expr) => TrigResult.Radical(Neg(expr))
    case mp => mp

  private def gcd(a: Long, b: Long): Long =
    if b == 0 then a.abs else gcd(b, a % b)
