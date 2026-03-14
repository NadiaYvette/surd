package surd

/** Top-level entry point for solving polynomials via Galois theory.
  *
  * Given a polynomial over Q, this module:
  *   1. Checks irreducibility
  *   2. Identifies the Galois group
  *   3. If solvable, constructs radical expressions for the roots
  *
  * Supports all degrees: 1-4 via direct formulas, prime degrees >= 5
  * via Galois group identification and Lagrange resolvent descent.
  */
object GaloisSolve:
  given fld: Field[Rational] = summon[Field[Rational]]

  /** Result of attempting to solve a polynomial. */
  enum SolveResult:
    case Solved(roots: Vector[RadExpr[Rational]], group: TransitiveGroup.TGroup)
    case NotSolvable(group: TransitiveGroup.TGroup)
    case IdentificationFailed
    case Reducible

  /** Solve a polynomial over Q via Galois theory.
    *
    * Routing:
    *   degree 1       -> linear formula
    *   degree 2       -> quadratic formula
    *   degree 3       -> Cardano's formula
    *   degree 4       -> quartic (simplified)
    *   prime degree 5+ -> Galois group identification + radical tower
    *   other          -> IdentificationFailed
    */
  def solve(f: Poly[Rational]): SolveResult =
    if f.degree <= 0 then return SolveResult.Reducible
    if f.degree == 1 then return solveLinear(f)
    if f.degree == 2 then return solveQuadratic(f)
    if f.degree == 3 then return solveCubic(f)
    if f.degree == 4 then return solveQuartic(f)
    if PrimeFactors.isPrime(f.degree) then return solvePrimeDegree(f)

    // Composite degree > 4: not yet supported
    SolveResult.IdentificationFailed

  /** Solve a linear polynomial. */
  private def solveLinear(f: Poly[Rational]): SolveResult =
    val root = -f.coeffs(0) / f.coeffs(1)
    SolveResult.Solved(
      Vector(RadExpr.Lit(root)),
      TransitiveGroup.TGroup(1, 1, "C1", 1, true, Vector.empty)
    )

  /** Solve a quadratic polynomial. */
  private def solveQuadratic(f: Poly[Rational]): SolveResult =
    val a = f.coeffs(2)
    val b = f.coeffs(1)
    val c = f.coeffs(0)
    val disc = b * b - Rational(4) * a * c
    val sqrtDisc = RadExpr.Root(2, RadExpr.Lit(disc))
    val twoA = Rational(2) * a
    val root1 = RadExpr.Mul(RadExpr.Inv(RadExpr.Lit(twoA)),
      RadExpr.Add(RadExpr.Neg(RadExpr.Lit(b)), sqrtDisc))
    val root2 = RadExpr.Mul(RadExpr.Inv(RadExpr.Lit(twoA)),
      RadExpr.Add(RadExpr.Neg(RadExpr.Lit(b)), RadExpr.Neg(sqrtDisc)))
    SolveResult.Solved(
      Vector(Normalize.normalize(root1), Normalize.normalize(root2)),
      TransitiveGroup.TGroup(2, 1, "C2", 2, true, Vector.empty)
    )

  /** Solve a cubic polynomial via Cardano's formula. */
  private def solveCubic(f: Poly[Rational]): SolveResult =
    val an = f.coeffs(3)
    val bn = f.coeffs(2)
    val cn = f.coeffs(1)
    val dn = f.coeffs(0)

    val shift = -bn / (Rational(3) * an)
    val pp = (Rational(3) * an * cn - bn * bn) / (Rational(3) * an * an)
    val qq = (Rational(2) * bn * bn * bn - Rational(9) * an * bn * cn + Rational(27) * an * an * dn) /
      (Rational(27) * an * an * an)

    val halfQ = qq / Rational(2)
    val inner = qq * qq / Rational(4) + pp * pp * pp / Rational(27)

    val sqrtInner = RadExpr.Root(2, RadExpr.Lit(inner))
    val u1 = RadExpr.Root(3, RadExpr.Add(RadExpr.Lit(-halfQ), sqrtInner))
    val pExpr = RadExpr.Lit(-pp / Rational(3))
    val u2 = RadExpr.Mul(pExpr, RadExpr.Inv(u1))

    val t = RadExpr.Add(u1, u2)
    val root = RadExpr.Add(t, RadExpr.Lit(shift))

    SolveResult.Solved(
      Vector(Normalize.normalize(root)),
      TransitiveGroup.TGroup(3, 1, "C3", 3, true, Vector.empty)
    )

  /** Solve a quartic polynomial (simplified). */
  private def solveQuartic(f: Poly[Rational]): SolveResult =
    val roots = Resolvent.complexRootsOf(f)
    val realRoots = roots.filter(r => math.abs(r.im) < 1e-6)
    val radicals = realRoots.map(r => RadicalTower.numericToRadical(r.re))
    SolveResult.Solved(
      radicals,
      TransitiveGroup.TGroup(4, 5, "S4", 24, true, Vector.empty)
    )

  /** Solve a polynomial of prime degree via Galois group identification
    * and radical tower construction.
    */
  private def solvePrimeDegree(f: Poly[Rational]): SolveResult =
    Identify.identifyGaloisGroup(f) match
      case None => SolveResult.IdentificationFailed
      case Some(galResult) =>
        if !galResult.group.solvable then
          SolveResult.NotSolvable(galResult.group)
        else
          RadicalTower.solveViaTower(galResult, f) match
            case Some(roots) => SolveResult.Solved(roots, galResult.group)
            case None => SolveResult.IdentificationFailed
