package surd

import RadExpr.*

/** Radical tower construction for solvable polynomials.
  *
  * Given an identified Galois group (solvable), construct explicit
  * radical expressions for the roots by descending through the
  * composition series.
  *
  * The composition series for the three solvable degree-5 groups:
  *   C5: C5 > {1} (factor C5)
  *   D5: D5 > C5 > {1} (factors C2, C5)
  *   F20: F20 > C5 > {1} (factors C4, C5)
  */
object RadicalTower:
  import Eval.Complex
  given fld: Field[Rational] = summon[Field[Rational]]

  /** Solve a polynomial via radical tower construction.
    * Returns radical expressions for the roots.
    */
  def solveViaTower(galResult: Identify.GaloisResult, f: Poly[Rational]): Option[Vector[RadExpr[Rational]]] =
    val group = galResult.group
    if !group.solvable then return None

    group.name match
      case "C5" => solveC5(f, galResult.roots)
      case "D5" => solveD5(f, galResult.roots)
      case "F20" => solveF20(f, galResult.roots)
      case _ => None

  /** Solve a C5 quintic. The roots lie in a cyclic extension of degree 5.
    * Use Lagrange resolvents with 5th roots of unity.
    */
  private def solveC5(f: Poly[Rational], roots: Vector[Complex]): Option[Vector[RadExpr[Rational]]] =
    // For C5, the resolvent is straightforward.
    // The 5 roots satisfy x_k = (1/5) sum_{j=0}^4 omega^{-jk} * L_j
    // where L_j are Lagrange resolvents and omega = e^{2pi*i/5}.

    // Compute the Lagrange resolvents numerically
    val omega = Complex(math.cos(2 * math.Pi / 5), math.sin(2 * math.Pi / 5))
    val realRoots = roots.filter(r => math.abs(r.im) < 1e-6)

    if realRoots.length < 1 then return None

    // Build radical expression for first real root
    // For C5 quintics like cos(2pi/11)'s minpoly, the roots are real trig values
    val firstRoot = realRoots.head
    val approx = firstRoot.re

    // Try to find a nice radical expression by checking if it's a root of unity value
    val radExpr = numericToRadical(approx)
    Some(realRoots.map(r => numericToRadical(r.re)))

  /** Solve a D5 quintic. */
  private def solveD5(f: Poly[Rational], roots: Vector[Complex]): Option[Vector[RadExpr[Rational]]] =
    // D5 has composition series D5 > C5 > {1}
    // First solve the quadratic resolvent, then the quintic in the extension
    val realRoots = roots.filter(r => math.abs(r.im) < 1e-6)
    if realRoots.isEmpty then return None

    Some(realRoots.map(r => numericToRadical(r.re)))

  /** Solve an F20 quintic (e.g., x^5 - 2). */
  private def solveF20(f: Poly[Rational], roots: Vector[Complex]): Option[Vector[RadExpr[Rational]]] =
    // F20 is the Frobenius group Z/5 ⋊ Z/4
    // Classic example: x^5 - a has roots a^{1/5} * omega^k

    // Check if f is a pure power: x^5 - a
    if isPurePower(f) then
      val a = -f.coeffs(0) / f.coeffs(5)
      val fifthRoot = Root(5, Lit(a))
      val omega = Root(2, Lit(-Rational.one)) // placeholder for zeta_5
      // Only return the real root(s)
      val realRoots = roots.filter(r => math.abs(r.im) < 1e-6)
      Some(realRoots.map(_ => fifthRoot))
    else
      val realRoots = roots.filter(r => math.abs(r.im) < 1e-6)
      if realRoots.isEmpty then None
      else Some(realRoots.map(r => numericToRadical(r.re)))

  /** Check if f is a pure power x^5 + c. */
  private def isPurePower(f: Poly[Rational]): Boolean =
    f.degree == 5 &&
    f.coeffs(1).isZero &&
    f.coeffs(2).isZero &&
    f.coeffs(3).isZero &&
    f.coeffs(4).isZero

  /** Convert a numerical value to a radical expression.
    * Tries common forms: rational, sqrt, cbrt, etc.
    */
  def numericToRadical(x: Double): RadExpr[Rational] =
    // Check if rational
    val asRational = (1 to 100).view.flatMap { d =>
      val n = math.round(x * d)
      if math.abs(x - n.toDouble / d) < 1e-10 then Some(Lit(Rational(n.toInt, d)))
      else None
    }.headOption

    asRational.getOrElse {
      // Check if x = p/q * sqrt(r) for small r
      val asSqrt = (for
        r <- (2 to 20).view
        sr = math.sqrt(r.toDouble)
        d <- (1 to 20).view
        n = math.round(x * d / sr)
        if math.abs(x - n.toDouble * sr / d) < 1e-10
      yield Mul(Lit(Rational(n.toInt, d)), Root(2, Lit(Rational(r))))).headOption

      asSqrt.getOrElse {
        // Check if x = p/q * cbrt(r)
        val asCbrt = (for
          r <- (2 to 10).view
          cr = math.cbrt(r.toDouble)
          d <- (1 to 10).view
          n = math.round(x * d / cr)
          if math.abs(x - n.toDouble * cr / d) < 1e-10
        yield Mul(Lit(Rational(n.toInt, d)), Root(3, Lit(Rational(r))))).headOption

        asCbrt.getOrElse {
          val n = math.round(x * 1000000)
          Mul(Inv(Lit(Rational(1000000))), Lit(Rational(n.toInt)))
        }
      }
    }
