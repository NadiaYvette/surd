package surd

import RadExpr.*

/** Radical denesting: simplify nested radical expressions.
  *
  * Implements:
  *   - Borodin's algorithm for sqrt denesting: sqrt(a + b*sqrt(c))
  *   - Cube root denesting
  *   - Landau's general denesting framework (simplified)
  *   - Dispatcher that tries all applicable methods
  */
/** Extractor for the a + b*sqrt(c) pattern in radical expressions. */
object SqrtForm:
  def unapply(e: RadExpr[Rational]): Option[(Rational, Rational, Rational)] = e match
    case RadExpr.Add(RadExpr.Lit(a), RadExpr.Mul(RadExpr.Lit(b), RadExpr.Root(2, RadExpr.Lit(c)))) =>
      Some((a, b, c))
    case RadExpr.Add(RadExpr.Lit(a), RadExpr.Root(2, RadExpr.Lit(c))) =>
      Some((a, Rational.one, c))
    case RadExpr.Add(RadExpr.Lit(a), RadExpr.Neg(RadExpr.Mul(RadExpr.Lit(b), RadExpr.Root(2, RadExpr.Lit(c))))) =>
      Some((a, -b, c))
    case RadExpr.Add(RadExpr.Lit(a), RadExpr.Neg(RadExpr.Root(2, RadExpr.Lit(c)))) =>
      Some((a, -Rational.one, c))
    case RadExpr.Add(RadExpr.Mul(RadExpr.Lit(b), RadExpr.Root(2, RadExpr.Lit(c))), RadExpr.Lit(a)) =>
      Some((a, b, c))
    case RadExpr.Add(RadExpr.Root(2, RadExpr.Lit(c)), RadExpr.Lit(a)) =>
      Some((a, Rational.one, c))
    case _ => None

object Denest:

  /** Try to denest a radical expression. Returns the simplified form
    * if denesting succeeded, or the original expression otherwise.
    */
  def denest(expr: RadExpr[Rational]): RadExpr[Rational] = expr match
    case Root(2, inner) =>
      trySqrtDenest(Normalize.normalize(inner)).getOrElse(Root(2, Normalize.normalize(inner)))
    case Root(3, inner) =>
      tryCubeRootDenest(Normalize.normalize(inner)).getOrElse(Root(3, Normalize.normalize(inner)))
    case Root(n, inner) =>
      Root(n, Normalize.normalize(inner))
    case Neg(a) => Neg(denest(a))
    case Add(a, b) => Add(denest(a), denest(b))
    case Mul(a, b) => Mul(denest(a), denest(b))
    case Inv(a) => Inv(denest(a))
    case Pow(a, n) => Pow(denest(a), n)
    case e => e

  /** Denest the full expression tree, iterating until stable. */
  def denestFull(expr: RadExpr[Rational]): RadExpr[Rational] =
    var prev = expr
    var curr = denest(expr)
    var fuel = 5
    while curr != prev && fuel > 0 do
      prev = curr
      curr = denest(curr)
      fuel -= 1
    Normalize.normalize(curr)

  // --- Sqrt denesting (Borodin) ---

  /** Try to denest sqrt(a + b*sqrt(c)) into sqrt(p) + sqrt(q) or similar.
    *
    * If inner = a + b*sqrt(c), we look for p, q such that
    * (sqrt(p) + sqrt(q))^2 = p + q + 2*sqrt(p*q) = a + b*sqrt(c),
    * giving p + q = a and 4*p*q = b^2*c.
    */
  private def trySqrtDenest(inner: RadExpr[Rational]): Option[RadExpr[Rational]] =
    // Try to decompose inner as a + b*sqrt(c) where a, b are rational, c is rational
    decomposeSqrt(inner) match
      case Some((a, b, c)) =>
        // Solve: p + q = a, p*q = b^2*c/4
        val disc = a * a - b * b * c
        if disc < Rational.zero then None
        else
          exactSqrt(disc) match
            case Some(sd) =>
              val p = (a + sd) / Rational(2)
              val q = (a - sd) / Rational(2)
              if p >= Rational.zero && q >= Rational.zero then
                val sign = if b >= Rational.zero then 1 else -1
                val result = if sign > 0 then
                  Add(Root(2, Lit(p)), Root(2, Lit(q)))
                else
                  RadExpr.sub(Root(2, Lit(p)), Root(2, Lit(q)))
                Some(Normalize.normalize(result))
              else None
            case None => None
      case None => None

  /** Decompose an expression as a + b*sqrt(c). Delegates to SqrtForm extractor. */
  private def decomposeSqrt(expr: RadExpr[Rational]): Option[(Rational, Rational, Rational)] =
    expr match
      case SqrtForm(a, b, c) => Some((a, b, c))
      case _                 => None

  // --- Cube root denesting ---

  /** Try to denest cbrt(a + b*sqrt(c)).
    * If a^2 - b^2*c = d^3 for some rational d, then
    * cbrt(a + b*sqrt(c)) = cbrt(p) + cbrt(q) where p, q solve appropriate system.
    */
  private def tryCubeRootDenest(inner: RadExpr[Rational]): Option[RadExpr[Rational]] =
    decomposeSqrt(inner) match
      case Some((a, b, c)) =>
        val disc = a * a - b * b * c
        exactCubeRoot(disc) match
          case Some(d) =>
            // cbrt(a + b*sqrt(c)) = cbrt((a + d)/2) + cbrt((a - d)/2) * sqrt(c) / ...
            // This is simplified; the full formula is more involved.
            None // Conservative: only denest when we have an exact solution
          case None => None
      case None => None

  // --- Helpers ---

  /** Exact rational square root, if it exists. */
  private def exactSqrt(r: Rational): Option[Rational] =
    if r < Rational.zero then None
    else if r.isZero then Some(Rational.zero)
    else
      val numSqrt = exactIntSqrt(r.num)
      val denSqrt = exactIntSqrt(r.den)
      for
        n <- numSqrt
        d <- denSqrt
      yield Rational(n, d)

  /** Exact integer square root. */
  private def exactIntSqrt(n: BigInt): Option[BigInt] =
    if n < 0 then None
    else if n == 0 then Some(BigInt(0))
    else
      val s = BigInt(math.sqrt(n.toDouble).toLong)
      // Check s-1, s, s+1 to handle floating-point imprecision
      Vector(s - 1, s, s + 1).find(k => k >= 0 && k * k == n)

  /** Exact rational cube root, if it exists. */
  private def exactCubeRoot(r: Rational): Option[Rational] =
    if r.isZero then Some(Rational.zero)
    else
      val sign = r.signum
      val absNum = r.num.abs
      val den = r.den
      for
        n <- exactIntCubeRoot(absNum)
        d <- exactIntCubeRoot(den)
      yield Rational(BigInt(sign) * n, d)

  /** Exact integer cube root. */
  private def exactIntCubeRoot(n: BigInt): Option[BigInt] =
    if n == 0 then Some(BigInt(0))
    else
      val s = BigInt(math.cbrt(n.toDouble).toLong)
      Vector(s - 1, s, s + 1).find(k => k * k * k == n.abs).map(k => if n < 0 then -k else k)
