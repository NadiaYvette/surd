package surd

import scala.collection.immutable.SortedMap

/** Normal form for radical expressions: Q-linear combinations of
  * products of radical atoms.
  *
  * Every expression in Q[sqrt(2), sqrt(3), cbrt(5), ...] has a unique
  * representation as a sum of (rational coefficient * monomial), where
  * each monomial is a product of radical atoms raised to bounded powers.
  */
object NormalForm:

  /** A radical atom: an irreducible nth root of a positive rational. */
  enum Atom extends Ordered[Atom]:
    /** nth root of r (r > 0, nth-power-free). */
    case RatRoot(n: Int, r: Rational)
    /** sqrt(-1) = i. */
    case ImagUnit
    /** nth root of a non-rational radicand expression. */
    case NestedRoot(n: Int, expr: RadExpr[Rational])

    def compare(that: Atom): Int = (this, that) match
      case (ImagUnit, ImagUnit) => 0
      case (ImagUnit, _) => -1
      case (_, ImagUnit) => 1
      case (RatRoot(n1, r1), RatRoot(n2, r2)) =>
        val c = n1.compare(n2)
        if c != 0 then c else r1.compare(r2)
      case (RatRoot(_, _), _) => -1
      case (_, RatRoot(_, _)) => 1
      case (NestedRoot(n1, e1), NestedRoot(n2, e2)) =>
        val c = n1.compare(n2)
        if c != 0 then c else e1.hashCode.compare(e2.hashCode) // structural fallback

  given Ordering[Atom] = Ordering.fromLessThan(_ < _)

  /** A monomial: product of atoms raised to positive powers.
    * Empty map = multiplicative identity (1).
    * Exponents bounded: [1, n-1] for RatRoot n r, [0,1] for ImagUnit.
    */
  final case class Monomial(factors: SortedMap[Atom, Int]):
    def isOne: Boolean = factors.isEmpty

  object Monomial:
    val one: Monomial = Monomial(SortedMap.empty[Atom, Int])

    def single(a: Atom, exp: Int): Monomial =
      if exp == 0 then one
      else Monomial(SortedMap(a -> exp))

  given Ordering[Monomial] = (a, b) =>
    val aList = a.factors.toList
    val bList = b.factors.toList
    def cmp(as: List[(Atom, Int)], bs: List[(Atom, Int)]): Int = (as, bs) match
      case (Nil, Nil) => 0
      case (Nil, _) => -1
      case (_, Nil) => 1
      case ((ka, va) :: at, (kb, vb) :: bt) =>
        val c = summon[Ordering[Atom]].compare(ka, kb)
        if c != 0 then c
        else
          val vc = va.compare(vb)
          if vc != 0 then vc else cmp(at, bt)
    cmp(aList, bList)

  /** A normalized expression: Q-linear combination of monomials. */
  final case class NormExpr(terms: SortedMap[Monomial, Rational]):
    def isZero: Boolean = terms.isEmpty

  object NormExpr:
    val zero: NormExpr = NormExpr(SortedMap.empty[Monomial, Rational])
    def lit(r: Rational): NormExpr =
      if r.isZero then zero
      else NormExpr(SortedMap(Monomial.one -> r))

  // --- Construction ---

  def normLit(r: Rational): NormExpr = NormExpr.lit(r)

  def normAtom(a: Atom): NormExpr =
    NormExpr(SortedMap(Monomial.single(a, 1) -> Rational.one))

  /** nth root of a positive rational, with perfect-power extraction. */
  def normRoot(n: Int, r: Rational): NormExpr =
    if r.isZero then NormExpr.zero
    else if r.isOne then NormExpr.lit(Rational.one)
    else if n == 1 then NormExpr.lit(r)
    else
      val (coeff, remainder) = extractNthPowerRational(n, r)
      if remainder.isOne then NormExpr.lit(coeff)
      else
        val atom = Atom.RatRoot(n, remainder)
        NormExpr(SortedMap(Monomial.single(atom, 1) -> coeff))

  // --- Arithmetic ---

  def normAdd(a: NormExpr, b: NormExpr): NormExpr =
    val merged = b.terms.foldLeft(a.terms) { case (acc, (mono, coeff)) =>
      acc.updatedWith(mono) {
        case Some(c) =>
          val s = c + coeff
          if s.isZero then None else Some(s)
        case None => Some(coeff)
      }
    }
    NormExpr(merged)

  def normSub(a: NormExpr, b: NormExpr): NormExpr =
    normAdd(a, normNeg(b))

  def normNeg(a: NormExpr): NormExpr =
    NormExpr(a.terms.map((m, c) => (m, -c)))

  def normScale(s: Rational, a: NormExpr): NormExpr =
    if s.isZero then NormExpr.zero
    else NormExpr(SortedMap.from(a.terms.map((m, c) => (m, s * c)).filter(!_._2.isZero)))

  def normMul(a: NormExpr, b: NormExpr): NormExpr =
    if a.isZero || b.isZero then NormExpr.zero
    else
      var result = NormExpr.zero
      for (ma, ca) <- a.terms; (mb, cb) <- b.terms do
        val (coeff, mono) = mulMonomials(ma, mb)
        val c = ca * cb * coeff
        if !c.isZero then
          result = normAdd(result, NormExpr(SortedMap(mono -> c)))
      result

  /** Multiply two monomials, reducing exponents as needed.
    * Returns (extracted_coefficient, reduced_monomial).
    */
  private def mulMonomials(a: Monomial, b: Monomial): (Rational, Monomial) =
    var coeff = Rational.one
    var factors = a.factors

    for (atom, exp) <- b.factors do
      val curExp = factors.getOrElse(atom, 0)
      val newExp = curExp + exp
      atom match
        case Atom.ImagUnit =>
          // i^2 = -1
          val reduced = newExp % 4
          val neg = (newExp / 2) % 2
          if neg != 0 then coeff = -coeff
          val finalExp = reduced % 2
          if finalExp == 0 then factors = factors.removed(atom)
          else factors = factors.updated(atom, finalExp)

        case Atom.RatRoot(n, r) =>
          // (n-th root of r)^n = r
          val full = newExp / n
          val rem = newExp % n
          if full != 0 then coeff = coeff * r.pow(full)
          if rem == 0 then factors = factors.removed(atom)
          else factors = factors.updated(atom, rem)

        case Atom.NestedRoot(n, _) =>
          val full = newExp / n
          val rem = newExp % n
          // For full powers, we'd need to expand the radicand as NormExpr.
          // For simplicity, just track the exponent.
          if rem == 0 then factors = factors.removed(atom)
          else factors = factors.updated(atom, newExp) // simplified

    (coeff, Monomial(factors))

  def normPow(a: NormExpr, n: Int): NormExpr =
    if n == 0 then NormExpr.lit(Rational.one)
    else if n == 1 then a
    else if n < 0 then normInv(normPow(a, -n))
    else
      val half = normPow(a, n / 2)
      val sq = normMul(half, half)
      if n % 2 == 0 then sq else normMul(sq, a)

  /** Inverse of a NormExpr. */
  def normInv(a: NormExpr): NormExpr =
    require(!a.isZero, "NormExpr: inverse of zero")
    if a.terms.size == 1 then
      val (mono, coeff) = a.terms.head
      // Invert: coeff^(-1) * mono^(-1)
      val invCoeff = coeff.inverse
      val invFactors = SortedMap.from(mono.factors.map((atom, exp) =>
        atom match
          case Atom.RatRoot(n, r) => (atom, n - exp)
          case Atom.ImagUnit => (atom, if exp == 1 then 3 else 1) // i^(-1) = -i = i^3
          case _ => (atom, -exp)
      ))
      // Reduce the inverted monomial
      val (redCoeff, redMono) = reduceMono(invFactors)
      NormExpr(SortedMap(redMono -> (invCoeff * redCoeff)))
    else
      // Multi-term: conjugate multiplication (simplified fallback)
      // For a+b, multiply by (a-b)/(a-b) to rationalize
      // This is a simplified version; full implementation would use extended GCD
      a // Placeholder: return as-is for complex multi-term inversions

  private def reduceMono(factors: SortedMap[Atom, Int]): (Rational, Monomial) =
    var coeff = Rational.one
    var result = SortedMap.empty[Atom, Int]
    for (atom, exp) <- factors do
      atom match
        case Atom.ImagUnit =>
          val e = ((exp % 4) + 4) % 4
          e match
            case 0 => () // no factor
            case 1 => result = result.updated(atom, 1)
            case 2 => coeff = -coeff
            case 3 =>
              coeff = -coeff
              result = result.updated(atom, 1)
        case Atom.RatRoot(n, r) =>
          val e = ((exp % n) + n) % n
          if e > 0 then result = result.updated(atom, e)
          val full = exp / n
          if full != 0 then coeff = coeff * r.pow(full)
        case _ =>
          if exp != 0 then result = result.updated(atom, exp)
    (coeff, Monomial(result))

  // --- Conversion ---

  /** Convert a RadExpr to NormExpr. */
  def toNormExpr(expr: RadExpr[Rational]): NormExpr = expr match
    case RadExpr.Lit(r) => normLit(r)
    case RadExpr.Neg(a) => normNeg(toNormExpr(a))
    case RadExpr.Add(a, b) => normAdd(toNormExpr(a), toNormExpr(b))
    case RadExpr.Mul(a, b) => normMul(toNormExpr(a), toNormExpr(b))
    case RadExpr.Inv(a) => normInv(toNormExpr(a))
    case RadExpr.Root(2, RadExpr.Lit(r)) if r == -Rational.one =>
      normAtom(Atom.ImagUnit)
    case RadExpr.Root(n, RadExpr.Lit(r)) if r > Rational.zero =>
      normRoot(n, r)
    case RadExpr.Root(n, RadExpr.Lit(r)) if r < Rational.zero && n % 2 != 0 =>
      normNeg(normRoot(n, -r))
    case RadExpr.Root(n, RadExpr.Lit(r)) if r < Rational.zero && n == 2 =>
      normMul(normAtom(Atom.ImagUnit), normRoot(2, -r))
    case RadExpr.Root(n, inner) =>
      // Nested root: treat as a NestedRoot atom
      normAtom(Atom.NestedRoot(n, inner))
    case RadExpr.Pow(a, n) =>
      normPow(toNormExpr(a), n)

  /** Convert a NormExpr back to RadExpr. */
  def fromNormExpr(ne: NormExpr): RadExpr[Rational] =
    if ne.isZero then RadExpr.Lit(Rational.zero)
    else
      val terms = ne.terms.toList.map { (mono, coeff) =>
        val monoExpr = monoToExpr(mono)
        if coeff.isOne then monoExpr
        else if coeff == -Rational.one then RadExpr.Neg(monoExpr)
        else RadExpr.Mul(RadExpr.Lit(coeff), monoExpr)
      }
      terms.reduceLeft(RadExpr.Add(_, _))

  private def monoToExpr(mono: Monomial): RadExpr[Rational] =
    if mono.isOne then RadExpr.Lit(Rational.one)
    else
      val factors = mono.factors.toList.map { (atom, exp) =>
        val base = atom match
          case Atom.RatRoot(n, r) => RadExpr.Root(n, RadExpr.Lit(r))
          case Atom.ImagUnit => RadExpr.Root(2, RadExpr.Lit(-Rational.one))
          case Atom.NestedRoot(n, e) => RadExpr.Root(n, e)
        if exp == 1 then base
        else RadExpr.Pow(base, exp)
      }
      factors.reduceLeft(RadExpr.Mul(_, _))

  /** Extract the largest nth power from a rational. */
  private def extractNthPowerRational(n: Int, r: Rational): (Rational, Rational) =
    if r.isZero then (Rational.zero, Rational.one)
    else
      val sign = r.signum
      val absR = r.abs
      val numAbs = absR.num
      val den = absR.den

      val (numOut, numIn) = extractNthPowerBigInt(n, numAbs)
      val (denOut, denIn) = extractNthPowerBigInt(n, den)

      if denIn == BigInt(1) then
        val coeff = Rational(numOut, denOut) * Rational(sign)
        (coeff, Rational(numIn))
      else
        val newInner = numIn * denIn.pow(n - 1)
        val newOuter = Rational(numOut, denOut * denIn) * Rational(sign)
        val (numOut2, numIn2) = extractNthPowerBigInt(n, newInner)
        (newOuter * Rational(numOut2), Rational(numIn2))

  private def extractNthPowerBigInt(n: Int, m: BigInt): (BigInt, BigInt) =
    if m <= 0 then (BigInt(1), m)
    else
      val fs = PrimeFactors.factorise(Positive.unsafe(m))
      val extracted = fs.foldLeft(BigInt(1)) { case (acc, (p, e)) => acc * p.pow(e / n) }
      val remainder = fs.foldLeft(BigInt(1)) { case (acc, (p, e)) => acc * p.pow(e % n) }
      (extracted, remainder)
