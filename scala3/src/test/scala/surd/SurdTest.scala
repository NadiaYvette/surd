package surd

import org.scalatest.funsuite.AnyFunSuite

class RationalTest extends AnyFunSuite:

  test("rational addition"):
    assert(Rational(1, 2) + Rational(1, 3) == Rational(5, 6))

  test("rational subtraction"):
    assert(Rational(3, 4) - Rational(1, 4) == Rational(1, 2))

  test("rational multiplication"):
    assert(Rational(2, 3) * Rational(3, 4) == Rational(1, 2))

  test("rational division"):
    assert(Rational(2, 3) / Rational(4, 5) == Rational(5, 6))

  test("rational negation"):
    assert(-Rational(3, 7) == Rational(-3, 7))

  test("rational inverse"):
    assert(Rational(2, 3).inverse == Rational(3, 2))

  test("rational power"):
    assert(Rational(2, 3).pow(3) == Rational(8, 27))

  test("rational zero and one"):
    assert(Rational.zero.isZero)
    assert(Rational.one.isOne)
    assert(!Rational.zero.isOne)
    assert(!Rational.one.isZero)

  test("rational normalization"):
    assert(Rational(4, 6) == Rational(2, 3))
    assert(Rational(-2, -3) == Rational(2, 3))
    assert(Rational(2, -3) == Rational(-2, 3))

  test("rational ordering"):
    assert(Rational(1, 3) < Rational(1, 2))
    assert(Rational(2, 3) > Rational(1, 2))
    assert(Rational(1, 2) == Rational(1, 2))


class PolyTest extends AnyFunSuite:
  given Field[Rational] = summon[Field[Rational]]

  test("polynomial addition"):
    val p = Poly(Vector(Rational(1), Rational(2)))      // 1 + 2x
    val q = Poly(Vector(Rational(3), Rational(4)))      // 3 + 4x
    val sum = Poly.add(p, q)
    assert(sum == Poly(Vector(Rational(4), Rational(6))))

  test("polynomial multiplication"):
    val p = Poly(Vector(Rational(1), Rational(1)))      // 1 + x
    val q = Poly(Vector(Rational(1), Rational(1)))      // 1 + x
    val prod = Poly.mul(p, q)
    // (1+x)^2 = 1 + 2x + x^2
    assert(prod == Poly(Vector(Rational(1), Rational(2), Rational(1))))

  test("polynomial evaluation"):
    val p = Poly(Vector(Rational(1), Rational(2), Rational(3)))  // 1 + 2x + 3x^2
    assert(Poly.eval(p, Rational(2)) == Rational(17))  // 1 + 4 + 12

  test("polynomial gcd"):
    // (x-1)(x+1) = x^2 - 1 and (x-1)(x+2) = x^2 + x - 2
    // gcd should be (x-1) up to monic normalization
    val p = Poly(Vector(-Rational.one, Rational.zero, Rational.one))  // x^2 - 1
    val q = Poly(Vector(Rational(-2), Rational.one, Rational.one))    // x^2 + x - 2
    val g = Poly.gcd(p, q)
    assert(g.degree == 1)
    // Should be monic: x - 1
    assert(Poly.eval(g, Rational.one).isZero)

  test("polynomial zero"):
    val z = Poly.zero[Rational]
    assert(z.isZero)
    assert(z.degree == -1)

  test("polynomial divMod"):
    // (x^2 - 1) / (x - 1) = x + 1, remainder 0
    val p = Poly(Vector(-Rational.one, Rational.zero, Rational.one))
    val d = Poly(Vector(-Rational.one, Rational.one))
    val (q, r) = Poly.divMod(p, d)
    assert(q == Poly(Vector(Rational.one, Rational.one)))
    assert(r.isZero)

  test("polynomial square-free"):
    // (x-1)^2 (x+1) = x^3 - x^2 - x + 1
    val xm1 = Poly(Vector(-Rational.one, Rational.one))
    val xp1 = Poly(Vector(Rational.one, Rational.one))
    val f = Poly.mul(Poly.mul(xm1, xm1), xp1)
    val sf = Poly.squareFree(f)
    assert(sf.nonEmpty)


class NormalizeTest extends AnyFunSuite:

  test("normalize idempotent"):
    val e = RadExpr.Add(RadExpr.Lit(Rational(1)), RadExpr.Root(2, RadExpr.Lit(Rational(2))))
    assert(Normalize.normalize(Normalize.normalize(e)) == Normalize.normalize(e))

  test("normalize folds constants"):
    val e = RadExpr.Add(RadExpr.Lit(Rational(2)), RadExpr.Lit(Rational(3)))
    assert(Normalize.normalize(e) == RadExpr.Lit(Rational(5)))

  test("normalize cancels double negation"):
    val e = RadExpr.Neg(RadExpr.Neg(RadExpr.Lit(Rational(7))))
    assert(Normalize.normalize(e) == RadExpr.Lit(Rational(7)))

  test("normalize cancels double inverse"):
    val e = RadExpr.Inv(RadExpr.Inv(RadExpr.Lit(Rational(3))))
    assert(Normalize.normalize(e) == RadExpr.Lit(Rational(3)))

  test("normalize multiply by zero"):
    val e = RadExpr.Mul(RadExpr.Lit(Rational.zero), RadExpr.Root(2, RadExpr.Lit(Rational(5))))
    assert(Normalize.normalize(e) == RadExpr.Lit(Rational.zero))

  test("normalize multiply by one"):
    val e = RadExpr.Mul(RadExpr.Lit(Rational.one), RadExpr.Root(2, RadExpr.Lit(Rational(5))))
    val n = Normalize.normalize(e)
    assert(n == RadExpr.Root(2, RadExpr.Lit(Rational(5))))

  test("normalize collects like terms"):
    import RadExpr.*
    val sqrt2 = Root(2, Lit(Rational(2)))
    // sqrt(2) + sqrt(2) => 2*sqrt(2)
    val e = Add(sqrt2, sqrt2)
    val n = Normalize.normalize(e)
    assert(n == Mul(Lit(Rational(2)), sqrt2))


class EvalTest extends AnyFunSuite:

  test("eval literal"):
    assert(Eval.eval(RadExpr.Lit(Rational(3, 2))) == 1.5)

  test("eval addition"):
    val e = RadExpr.Add(RadExpr.Lit(Rational(1)), RadExpr.Lit(Rational(2)))
    assert(Eval.eval(e) == 3.0)

  test("eval sqrt(4) = 2"):
    val e = RadExpr.Root(2, RadExpr.Lit(Rational(4)))
    assert(math.abs(Eval.eval(e) - 2.0) < 1e-10)

  test("eval matches evalComplex real part for real expressions"):
    val e = RadExpr.Add(
      RadExpr.Root(2, RadExpr.Lit(Rational(2))),
      RadExpr.Lit(Rational(1))
    )
    val evalD = Eval.eval(e)
    val evalC = Eval.evalComplex(e)
    assert(math.abs(evalD - evalC.re) < 1e-10)
    assert(math.abs(evalC.im) < 1e-10)

  test("evaluator typeclass matches direct eval"):
    val e = RadExpr.Add(
      RadExpr.Root(2, RadExpr.Lit(Rational(3))),
      RadExpr.Lit(Rational(1))
    )
    val viaEval = Eval.eval(e)
    val viaTC = Evaluator.eval[Double](e)
    assert(math.abs(viaEval - viaTC) < 1e-10)

  test("evaluator complex typeclass matches evalComplex"):
    import Eval.Complex
    val e = RadExpr.Add(
      RadExpr.Root(2, RadExpr.Lit(Rational(5))),
      RadExpr.Lit(Rational(2))
    )
    val viaEval = Eval.evalComplex(e)
    val viaTC = Evaluator.eval[Complex](e)
    assert(math.abs(viaEval.re - viaTC.re) < 1e-10)
    assert(math.abs(viaEval.im - viaTC.im) < 1e-10)


class TrigTest extends AnyFunSuite:

  test("cos(0) = 1"):
    Trig.cosExact(0, 1) match
      case Trig.TrigResult.Radical(e) =>
        assert(Eval.eval(e) == 1.0)
      case _ => fail("expected radical")

  test("cos(pi) = -1"):
    Trig.cosExact(1, 1) match
      case Trig.TrigResult.Radical(e) =>
        assert(Eval.eval(e) == -1.0)
      case _ => fail("expected radical")

  test("cos(pi/2) = 0"):
    Trig.cosExact(1, 2) match
      case Trig.TrigResult.Radical(e) =>
        assert(math.abs(Eval.eval(e)) < 1e-10)
      case _ => fail("expected radical")

  test("cos(pi/3) = 1/2"):
    Trig.cosExact(1, 3) match
      case Trig.TrigResult.Radical(e) =>
        assert(math.abs(Eval.eval(e) - 0.5) < 1e-10)
      case _ => fail("expected radical")

  test("sin(pi/2) = 1"):
    Trig.sinExact(1, 2) match
      case Trig.TrigResult.Radical(e) =>
        assert(math.abs(Eval.eval(e) - 1.0) < 1e-10)
      case _ => fail("expected radical")

  test("sin(0) = 0"):
    Trig.sinExact(0, 1) match
      case Trig.TrigResult.Radical(e) =>
        assert(math.abs(Eval.eval(e)) < 1e-10)
      case _ => fail("expected radical")


class TransformTest extends AnyFunSuite:

  test("transform identity"):
    val e = RadExpr.Add(RadExpr.Lit(Rational(1)), RadExpr.Root(2, RadExpr.Lit(Rational(2))))
    assert(RadExpr.transform[Rational](identity)(e) == e)

  test("transform doubles all literals"):
    val e = RadExpr.Add(RadExpr.Lit(Rational(1)), RadExpr.Lit(Rational(2)))
    val doubled = RadExpr.transform[Rational] {
      case RadExpr.Lit(r) => RadExpr.Lit(r * Rational(2))
      case other => other
    }(e)
    // After bottom-up: Lit(1)->Lit(2), Lit(2)->Lit(4), Add(Lit(2), Lit(4))
    assert(doubled == RadExpr.Add(RadExpr.Lit(Rational(2)), RadExpr.Lit(Rational(4))))


class OpaqueTypesTest extends AnyFunSuite:
  given Field[Rational] = summon[Field[Rational]]

  test("SquareFreePoly wraps and unwraps"):
    val p = Poly(Vector(Rational(1), Rational(2), Rational(1)))
    val sf = SquareFreePoly(p)
    assert(sf.underlying == p)

  test("MonicPoly wraps and unwraps"):
    val p = Poly(Vector(Rational(-1), Rational.one))
    val mp = MonicPoly(p)
    assert(mp.underlying == p)

  test("Factoring.squareFree returns SquareFreePoly"):
    val xm1 = Poly(Vector(-Rational.one, Rational.one))
    val xp1 = Poly(Vector(Rational.one, Rational.one))
    val f = Poly.mul(Poly.mul(xm1, xm1), xp1)
    val sfs = Factoring.squareFree(f)
    assert(sfs.nonEmpty)
    // Each factor should be accessible via .underlying
    sfs.foreach { case (sf, _) =>
      assert(!sf.underlying.isZero)
    }


class SqrtFormTest extends AnyFunSuite:

  test("SqrtForm matches a + b*sqrt(c)"):
    import RadExpr.*
    val e = Add(Lit(Rational(3)), Mul(Lit(Rational(2)), Root(2, Lit(Rational(5)))))
    e match
      case SqrtForm(a, b, c) =>
        assert(a == Rational(3))
        assert(b == Rational(2))
        assert(c == Rational(5))
      case _ => fail("should match SqrtForm")

  test("SqrtForm matches a + sqrt(c)"):
    import RadExpr.*
    val e = Add(Lit(Rational(1)), Root(2, Lit(Rational(7))))
    e match
      case SqrtForm(a, b, c) =>
        assert(a == Rational(1))
        assert(b == Rational.one)
        assert(c == Rational(7))
      case _ => fail("should match SqrtForm")

  test("SqrtForm does not match non-sqrt forms"):
    import RadExpr.*
    val e = Add(Lit(Rational(1)), Root(3, Lit(Rational(7))))
    e match
      case SqrtForm(_, _, _) => fail("should not match cube root")
      case _ => // ok
