package surd

import RadExpr.*

/** Normalization passes for radical expressions.
  *
  * These are explicit transformations the user applies as needed.
  * They do NOT denest.
  */
object Normalize:

  /** Apply all normalization passes, iterated to a fixed point (max 10 iterations). */
  def normalize(expr: RadExpr[Rational]): RadExpr[Rational] =
    fixN(10, normalizeOnce, expr)

  /** A single normalization pass (all sub-passes composed once). */
  def normalizeOnce(expr: RadExpr[Rational]): RadExpr[Rational] =
    val pipe = (flattenArith[Rational])
      .andThen(foldConstants)
      .andThen(simplifyPowers)
      .andThen(extractPerfectPowers)
      .andThen(sortCommutative)
      .andThen(distribute)
      .andThen(collectCoefficients)
      .andThen(collectTerms)
    pipe(expr)

  /** Iterate a function until fixed point or fuel runs out. */
  private def fixN[A](n: Int, f: A => A, x: A): A =
    if n <= 0 then x
    else
      val x1 = f(x)
      if x1 == x then x else fixN(n - 1, f, x1)

  // ---------------------------------------------------------------
  // flattenArith: cancel double negations and double inverses
  // ---------------------------------------------------------------

  def flattenArith[K](expr: RadExpr[K]): RadExpr[K] =
    RadExpr.transform[K] {
      case Neg(Neg(a)) => a
      case Inv(Inv(a)) => a
      case e           => e
    }(expr)

  // ---------------------------------------------------------------
  // foldConstants: evaluate pure-literal subtrees
  // ---------------------------------------------------------------

  def foldConstants(expr: RadExpr[Rational]): RadExpr[Rational] = expr match
    case Lit(_) => expr
    case Neg(a) =>
      foldConstants(a) match
        case Lit(r)  => Lit(-r)
        case Neg(a1) => a1
        case a1      => Neg(a1)
    case Add(a, b) =>
      (foldConstants(a), foldConstants(b)) match
        case (Lit(r), Lit(s))   => Lit(r + s)
        case (Lit(r), b1) if r.isZero => b1
        case (a1, Lit(s)) if s.isZero => a1
        case (a1, Neg(b1)) if a1 == b1 => Lit(Rational.zero)
        case (a1, b1)           => Add(a1, b1)
    case Mul(a, b) =>
      (foldConstants(a), foldConstants(b)) match
        case (Lit(r), Lit(s))   => Lit(r * s)
        case (Lit(r), _) if r.isZero => Lit(Rational.zero)
        case (_, Lit(s)) if s.isZero => Lit(Rational.zero)
        case (Lit(r), b1) if r.isOne => b1
        case (a1, Lit(s)) if s.isOne => a1
        case (Lit(r), b1) if r == -Rational.one => Neg(b1)
        case (a1, Lit(s)) if s == -Rational.one => Neg(a1)
        case (a1, b1)           => Mul(a1, b1)
    case Inv(a) =>
      foldConstants(a) match
        case Lit(r) if !r.isZero => Lit(r.inverse)
        case Inv(a1)             => a1
        case a1                  => Inv(a1)
    case Root(n, a) =>
      foldConstants(a) match
        case Lit(r) if r.isZero => Lit(Rational.zero)
        case Lit(r) if r.isOne  => Lit(Rational.one)
        case a1                 => Root(n, a1)
    case Pow(a, n) =>
      foldConstants(a) match
        case Lit(r)  => Lit(r.pow(n))
        case a1      =>
          if n == 0 then Lit(Rational.one)
          else if n == 1 then a1
          else Pow(a1, n)

  // ---------------------------------------------------------------
  // simplifyPowers
  // ---------------------------------------------------------------

  def simplifyPowers(expr: RadExpr[Rational]): RadExpr[Rational] =
    RadExpr.transform[Rational] {
      case Mul(Root(2, a), Root(2, b)) if a == b => a
      case Pow(Pow(a, m), n)                     => Pow(a, m * n)
      case Pow(Root(n, a), m) if m == n          => a
      case Root(n, Pow(a, m)) if m == n          => a
      case Root(m, Root(n, a))                   => Root(m * n, a)
      case e                                     => e
    }(expr)

  // ---------------------------------------------------------------
  // extractPerfectPowers
  // ---------------------------------------------------------------

  def extractPerfectPowers(expr: RadExpr[Rational]): RadExpr[Rational] = expr match
    case Root(n, Lit(r)) if r > Rational.zero =>
      val numAbs = r.num.abs
      val den = r.den
      val (numOut, numIn) = extractNthPower(n, numAbs)
      val (denOut, denIn) = extractNthPower(n, den)
      val (outerCoeff, innerRat) =
        if denIn == BigInt(1) then
          (Rational(numOut, denOut), Rational(numIn))
        else
          val newInner = numIn * denIn.pow(n - 1)
          val newOuter = Rational(numOut, denOut * denIn)
          val (numOut2, numIn2) = extractNthPower(n, newInner)
          (newOuter * Rational(numOut2), Rational(numIn2))

      (outerCoeff.isOne, innerRat.isOne) match
        case (true, true)   => Lit(Rational.one)
        case (true, false)  => Root(n, Lit(innerRat))
        case (_, true)      => Lit(outerCoeff)
        case _              => Mul(Lit(outerCoeff), Root(n, Lit(innerRat)))

    case Root(n, Lit(r)) if r < Rational.zero && n % 2 != 0 =>
      Neg(extractPerfectPowers(Root(n, Lit(-r))))

    case Root(n, a) => Root(n, extractPerfectPowers(a))
    case Neg(a)     => Neg(extractPerfectPowers(a))
    case Add(a, b)  => Add(extractPerfectPowers(a), extractPerfectPowers(b))
    case Mul(a, b)  => Mul(extractPerfectPowers(a), extractPerfectPowers(b))
    case Inv(a)     => Inv(extractPerfectPowers(a))
    case Pow(a, n)  => Pow(extractPerfectPowers(a), n)
    case e          => e

  /** Given n and a positive integer m, extract the largest nth power divisor.
    * Returns (extracted, remainder) such that m = extracted^n * remainder.
    */
  private def extractNthPower(n: Int, m: BigInt): (BigInt, BigInt) =
    if m <= BigInt(0) then (BigInt(1), m)
    else
      val fs = PrimeFactors.factorise(Positive.unsafe(m))
      val extracted = fs.foldLeft(BigInt(1)) { case (acc, (p, e)) => acc * p.pow(e / n) }
      val remainder = fs.foldLeft(BigInt(1)) { case (acc, (p, e)) => acc * p.pow(e % n) }
      (extracted, remainder)

  // ---------------------------------------------------------------
  // sortCommutative: canonical ordering of Add/Mul children
  // ---------------------------------------------------------------

  def sortCommutative(expr: RadExpr[Rational]): RadExpr[Rational] = expr match
    case _: Add[_] =>
      val terms = flattenAddList(expr).map(sortCommutative)
      val sorted = terms.sorted(using radExprOrdering[Rational])
      buildAddList(sorted)
    case _: Mul[_] =>
      val factors = flattenMulList(expr).map(sortCommutative)
      val sorted = factors.sorted(using radExprOrdering[Rational])
      buildMulList(sorted)
    case Neg(a)    => Neg(sortCommutative(a))
    case Inv(a)    => Inv(sortCommutative(a))
    case Root(n,a) => Root(n, sortCommutative(a))
    case Pow(a, n) => Pow(sortCommutative(a), n)
    case e         => e

  // ---------------------------------------------------------------
  // distribute: scalar multiplication over addition
  // ---------------------------------------------------------------

  def distribute(expr: RadExpr[Rational]): RadExpr[Rational] = expr match
    // Lit * (sum) => distribute
    case Mul(Lit(c), r) if isSum(r) =>
      val terms = flattenSNE(r)
      buildAddList(terms.map(t => Mul(Lit(c), t)))
    case Mul(l, Lit(c)) if isSum(l) =>
      val terms = flattenSNE(l)
      buildAddList(terms.map(t => Mul(t, Lit(c))))
    // (tiny sum) * x => distribute
    case Mul(l, r) if isTinySum(l) && !isSum(r) =>
      val terms = flattenSNE(l)
      buildAddList(terms.map(t => distribute(Mul(t, r))))
    case Mul(l, r) if !isSum(l) && isTinySum(r) =>
      val terms = flattenSNE(r)
      buildAddList(terms.map(t => distribute(Mul(l, t))))
    // c * (-a) => -(c*a)
    case Mul(Lit(c), Neg(a)) => Neg(distribute(Mul(Lit(c), a)))
    case Mul(Neg(a), Lit(c)) => Neg(distribute(Mul(a, Lit(c))))
    // recurse
    case Neg(a)    => Neg(distribute(a))
    case Add(a, b) => Add(distribute(a), distribute(b))
    case Mul(a, b) => Mul(distribute(a), distribute(b))
    case Inv(a)    => Inv(distribute(a))
    case Root(n,a) => Root(n, distribute(a))
    case Pow(a, n) => Pow(distribute(a), n)
    case e         => e

  private def isSum[K](e: RadExpr[K]): Boolean = e match
    case Add(_, _) => true
    case Neg(a)    => isSum(a)
    case _         => false

  private def isTinySum[K](e: RadExpr[K]): Boolean =
    isSum(e) && flattenSNE(e).length <= 2

  /** Flatten a sum, distributing Neg into each summand. */
  private def flattenSNE[K](e: RadExpr[K]): List[RadExpr[K]] = e match
    case Add(a, b) => flattenSNE(a) ++ flattenSNE(b)
    case Neg(a)    => flattenSNE(a).map(Neg(_))
    case _         => List(e)

  // ---------------------------------------------------------------
  // collectCoefficients: merge Lit factors in products
  // ---------------------------------------------------------------

  def collectCoefficients(expr: RadExpr[Rational]): RadExpr[Rational] =
    def go(e: RadExpr[Rational]): RadExpr[Rational] = e match
      case _: Mul[_] =>
        val factors = flattenMulList(e)
        val processed = factors.map(go)
        val (lits, rest) = partitionLits(processed)
        val coeff = if lits.isEmpty then Rational.one else lits.reduce(_ * _)
        val body = buildMulList(rest)
        applyCoeffMul(coeff, body)
      case Neg(a) =>
        go(a) match
          case Lit(r) => Lit(-r)
          case a1     => Neg(a1)
      case Add(a, b)  => Add(go(a), go(b))
      case Inv(a)     => Inv(go(a))
      case Root(n, a) => Root(n, go(a))
      case Pow(a, n)  => Pow(go(a), n)
      case _          => e
    go(expr)

  private def partitionLits(xs: List[RadExpr[Rational]]): (List[Rational], List[RadExpr[Rational]]) =
    val lits = scala.collection.mutable.ListBuffer[Rational]()
    val rest = scala.collection.mutable.ListBuffer[RadExpr[Rational]]()
    xs.foreach {
      case Lit(r) => lits += r
      case Inv(Lit(r)) if !r.isZero => lits += r.inverse
      case x => rest += x
    }
    (lits.toList, rest.toList)

  private def applyCoeffMul(c: Rational, body: RadExpr[Rational]): RadExpr[Rational] =
    if c.isZero then Lit(Rational.zero)
    else if c.isOne then body
    else if c == -Rational.one then Neg(body)
    else body match
      case Lit(r) => Lit(c * r)
      case _      => Mul(Lit(c), body)

  // ---------------------------------------------------------------
  // collectTerms: group like terms in sums
  // ---------------------------------------------------------------

  def collectTerms(expr: RadExpr[Rational]): RadExpr[Rational] =
    given Ordering[RadExpr[Rational]] = radExprOrdering[Rational]

    def go(e: RadExpr[Rational]): RadExpr[Rational] = e match
      case _: Add[_] =>
        val terms = flattenAddList(e).map(go)
        val grouped = scala.collection.mutable.LinkedHashMap.empty[RadExpr[Rational], Rational]
        terms.foreach { t =>
          val (c, base) = splitCoeff(t)
          grouped.updateWith(base) {
            case Some(old) => Some(old + c)
            case None      => Some(c)
          }
        }
        val sorted = grouped.toList.sortBy(_._1)
        val rebuilt = sorted.collect {
          case (base, c) if !c.isZero => applyCoeffAdd(c, base)
        }
        buildAddList(rebuilt)
      case Neg(a)     => Neg(go(a))
      case Mul(a, b)  => Mul(go(a), go(b))
      case Inv(a)     => Inv(go(a))
      case Root(n, a) => Root(n, go(a))
      case Pow(a, n)  => Pow(go(a), n)
      case _          => e
    go(expr)

  private def splitCoeff(e: RadExpr[Rational]): (Rational, RadExpr[Rational]) = e match
    case Mul(Lit(c), body) => (c, body)
    case Neg(inner) =>
      val (c, b) = splitCoeff(inner)
      (-c, b)
    case Lit(r) => (r, Lit(Rational.one))
    case _      => (Rational.one, e)

  private def applyCoeffAdd(c: Rational, base: RadExpr[Rational]): RadExpr[Rational] =
    if c.isOne && base == Lit(Rational.one) then Lit(Rational.one)
    else if base == Lit(Rational.one) then Lit(c)
    else if c.isOne then base
    else if c == -Rational.one then Neg(base)
    else Mul(Lit(c), base)

  // ---------------------------------------------------------------
  // Shared list helpers
  // ---------------------------------------------------------------

  private def flattenAddList[K](e: RadExpr[K]): List[RadExpr[K]] = e match
    case Add(a, b) => flattenAddList(a) ++ flattenAddList(b)
    case _         => List(e)

  private def flattenMulList[K](e: RadExpr[K]): List[RadExpr[K]] = e match
    case Mul(a, b) => flattenMulList(a) ++ flattenMulList(b)
    case _         => List(e)

  private def buildAddList[K](xs: List[RadExpr[K]]): RadExpr[K] = xs match
    case Nil      => Lit(Rational.zero).asInstanceOf[RadExpr[K]]
    case x :: Nil => x
    case x :: rest => rest.foldLeft(x)(Add(_, _))

  private def buildMulList[K](xs: List[RadExpr[K]]): RadExpr[K] = xs match
    case Nil      => Lit(Rational.one).asInstanceOf[RadExpr[K]]
    case x :: Nil => x
    case x :: rest => rest.foldLeft(x)(Mul(_, _))

  // ---------------------------------------------------------------
  // Ordering for RadExpr (structural, for canonical sorting)
  // ---------------------------------------------------------------

  /** Structural ordering on RadExpr for canonical sorting.
    * Ordering: Lit < Neg < Add < Mul < Inv < Root < Pow, then by fields.
    */
  def radExprOrdering[K: Ordering]: Ordering[RadExpr[K]] = new Ordering[RadExpr[K]]:
    def compare(x: RadExpr[K], y: RadExpr[K]): Int =
      val kOrd = summon[Ordering[K]]
      (x, y) match
        case (Lit(a), Lit(b))       => kOrd.compare(a, b)
        case (Lit(_), _)            => -1
        case (_, Lit(_))            => 1
        case (Neg(a), Neg(b))       => compare(a, b)
        case (Neg(_), _)            => -1
        case (_, Neg(_))            => 1
        case (Add(a1, a2), Add(b1, b2)) =>
          val c = compare(a1, b1)
          if c != 0 then c else compare(a2, b2)
        case (Add(_, _), _)         => -1
        case (_, Add(_, _))         => 1
        case (Mul(a1, a2), Mul(b1, b2)) =>
          val c = compare(a1, b1)
          if c != 0 then c else compare(a2, b2)
        case (Mul(_, _), _)         => -1
        case (_, Mul(_, _))         => 1
        case (Inv(a), Inv(b))       => compare(a, b)
        case (Inv(_), _)            => -1
        case (_, Inv(_))            => 1
        case (Root(n1, a), Root(n2, b)) =>
          val c = n1.compare(n2)
          if c != 0 then c else compare(a, b)
        case (Root(_, _), _)        => -1
        case (_, Root(_, _))        => 1
        case (Pow(a, n1), Pow(b, n2)) =>
          val c = compare(a, b)
          if c != 0 then c else n1.compare(n2)
