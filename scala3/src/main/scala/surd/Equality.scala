package surd

/** Radical expression equality testing.
  *
  * Two radical expressions are equal if they evaluate to the same value.
  * We use a multi-level strategy:
  *   1. Structural equality (fast, but misses many equalities)
  *   2. Normal form equality (catches algebraic identities)
  *   3. Numerical evaluation with interval arithmetic (rigorous fallback)
  */
object Equality:

  /** Test whether two radical expressions are equal.
    * Uses normalization + numerical evaluation.
    */
  def radEqual(a: RadExpr[Rational], b: RadExpr[Rational]): Boolean =
    // Fast path: structural equality
    if a == b then true
    else
      // Try normalization
      val na = Normalize.normalize(a)
      val nb = Normalize.normalize(b)
      if na == nb then true
      else
        // Try normal form
        val nfa = NormalForm.toNormExpr(na)
        val nfb = NormalForm.toNormExpr(nb)
        if nfa == nfb then true
        else
          // Numerical fallback: evaluate difference and check if zero
          numericallyZero(RadExpr.sub(a, b))

  /** Test whether a radical expression is zero using numerical evaluation. */
  def numericallyZero(expr: RadExpr[Rational]): Boolean =
    val normalized = Normalize.normalize(expr)
    normalized match
      case RadExpr.Lit(r) => r.isZero
      case _ =>
        // Complex evaluation
        val z = Eval.evalComplex(normalized)
        val mag = z.magnitude
        if mag > 1e-10 then false
        else if mag < 1e-15 then
          // High confidence it's zero; verify with interval arithmetic
          try
            val iv = Eval.evalInterval(normalized)
            iv.containsZero
          catch
            case _: Exception =>
              // Interval eval may fail for complex intermediates
              mag < 1e-14
        else
          // Ambiguous: try harder with interval arithmetic
          try
            val iv = Eval.evalInterval(normalized)
            iv.lo == Rational.zero && iv.hi == Rational.zero
          catch
            case _: Exception => false

  /** Test whether a radical expression is numerically real. */
  def isReal(expr: RadExpr[Rational]): Boolean =
    val z = Eval.evalComplex(expr)
    math.abs(z.im) < 1e-10
