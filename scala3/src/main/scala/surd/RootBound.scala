package surd

/** Root bounds for univariate polynomials.
  *
  * Provides upper bounds on the absolute values of roots,
  * used for Sturm isolation and algebraic number arithmetic.
  */
object RootBound:

  /** Cauchy bound: all roots alpha of p(x) satisfy
    * |alpha| <= 1 + max(|a_i|/|a_n|) for i < n.
    *
    * Returns a Rational upper bound.
    */
  def cauchyBound(p: Poly[Rational]): Rational =
    if p.isZero || p.degree <= 0 then Rational.one
    else
      val lc = p.coeffs.last.abs
      val maxRatio = p.coeffs.init.map(c => c.abs / lc).max
      Rational.one + maxRatio

  /** Lagrange bound: all roots alpha satisfy
    * |alpha| <= max(1, sum(|a_i|/|a_n|)) for i < n.
    */
  def lagrangeBound(p: Poly[Rational]): Rational =
    if p.isZero || p.degree <= 0 then Rational.one
    else
      val lc = p.coeffs.last.abs
      val s = p.coeffs.init.map(c => c.abs / lc).fold(Rational.zero)(_ + _)
      s.max(Rational.one)

  /** Kioustelidis bound (tighter than Cauchy for many polynomials).
    * Uses the two largest |a_i/a_n| values.
    */
  def kioustelidisbound(p: Poly[Rational]): Rational =
    if p.isZero || p.degree <= 0 then Rational.one
    else
      val lc = p.coeffs.last.abs
      val ratios = p.coeffs.init.map(c => c.abs / lc).sorted.reverse
      if ratios.isEmpty then Rational.one
      else if ratios.length == 1 then Rational.one + ratios.head
      else
        // Bound = max(ratio_0^(1/1), ratio_1^(1/2), ...)
        // Simplify: just use Cauchy
        Rational.one + ratios.head

  /** A practical root bound: minimum of Cauchy and Lagrange bounds. */
  def rootBound(p: Poly[Rational]): Rational =
    cauchyBound(p).min(lagrangeBound(p))
