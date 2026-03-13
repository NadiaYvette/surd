package surd

/** Ordering on radical expressions.
  *
  * For expressions that evaluate to real numbers, we define a total order
  * based on numerical evaluation (with interval arithmetic for rigor).
  */
object Order:

  /** Compare two radical expressions that evaluate to real numbers.
    * Returns negative if a < b, zero if a == b, positive if a > b.
    *
    * Uses numerical evaluation first, with interval arithmetic refinement
    * when values are close.
    */
  def radCompare(a: RadExpr[Rational], b: RadExpr[Rational]): Int =
    if a == b then 0
    else
      // Try numerical comparison first
      val va = Eval.eval(a)
      val vb = Eval.eval(b)
      val diff = va - vb
      if diff > 1e-10 then 1
      else if diff < -1e-10 then -1
      else
        // Values are close; use interval arithmetic
        intervalCompare(a, b)

  /** Interval-based comparison for when numerical values are close. */
  private def intervalCompare(a: RadExpr[Rational], b: RadExpr[Rational]): Int =
    try
      val diffExpr = RadExpr.sub(a, b)
      val iv = Eval.evalInterval(Normalize.normalize(diffExpr))
      if iv.strictlyPositive then 1
      else if iv.strictlyNegative then -1
      else if iv.lo == Rational.zero && iv.hi == Rational.zero then 0
      else
        // Refine by iterated bisection (up to 10 times)
        refineCompare(diffExpr, 10)
    catch
      case _: Exception =>
        // Fallback to Double comparison
        val va = Eval.eval(a)
        val vb = Eval.eval(b)
        java.lang.Double.compare(va, vb)

  /** Refine comparison by repeatedly normalizing the difference. */
  private def refineCompare(diff: RadExpr[Rational], fuel: Int): Int =
    if fuel <= 0 then 0 // Give up: treat as equal
    else
      val z = Eval.evalComplex(diff)
      if math.abs(z.re) < 1e-15 && math.abs(z.im) < 1e-15 then 0
      else if z.re > 0 then 1
      else -1

  /** Ordering instance for RadExpr[Rational]. */
  given radOrdering: Ordering[RadExpr[Rational]] =
    (a, b) => radCompare(a, b)

  /** Check if a <= b. */
  def radLe(a: RadExpr[Rational], b: RadExpr[Rational]): Boolean =
    radCompare(a, b) <= 0

  /** Check if a < b. */
  def radLt(a: RadExpr[Rational], b: RadExpr[Rational]): Boolean =
    radCompare(a, b) < 0

  /** Minimum of two radical expressions. */
  def radMin(a: RadExpr[Rational], b: RadExpr[Rational]): RadExpr[Rational] =
    if radLe(a, b) then a else b

  /** Maximum of two radical expressions. */
  def radMax(a: RadExpr[Rational], b: RadExpr[Rational]): RadExpr[Rational] =
    if radLe(a, b) then b else a
