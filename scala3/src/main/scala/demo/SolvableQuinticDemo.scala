package demo

import surd.*
import surd.Identify.*
import surd.GaloisSolve.*

/** Solvable quintic solver demo.
  *
  * Demonstrates Galois-theoretic identification and radical tower construction
  * for degree-5 polynomials over Q. Classifies each polynomial's Galois group
  * among the five transitive subgroups of S5, then -- when solvable --
  * constructs explicit radical expressions for the roots.
  */
object SolvableQuinticDemo:
  given Field[Rational] = summon[Field[Rational]]

  final case class Example(
      name: String,
      poly: Poly[Rational],
      notes: String
  )

  val examples: Vector[Example] = Vector(
    // C5 quintic: minimal polynomial of 2cos(2pi/11).
    Example(
      name = "x^5 + x^4 - 4x^3 - 3x^2 + 3x + 1",
      poly = Poly(Vector(
        Rational.one, Rational(3), Rational(-3),
        Rational(-4), Rational.one, Rational.one
      )),
      notes = "C5 -- minimal polynomial of 2cos(2pi/11)"
    ),
    // F20 (Frobenius) quintic: x^5 - 2.
    Example(
      name = "x^5 - 2",
      poly = Poly(Vector(
        Rational(-2), Rational.zero, Rational.zero,
        Rational.zero, Rational.zero, Rational.one
      )),
      notes = "F20 -- Gal(Q(5th-root 2, zeta5)/Q) = Z/5 x| Z/4"
    ),
    // D5 (dihedral) quintic of order 10.
    Example(
      name = "x^5 + 20x + 32",
      poly = Poly(Vector(
        Rational(32), Rational(20), Rational.zero,
        Rational.zero, Rational.zero, Rational.one
      )),
      notes = "D5 -- dihedral group of order 10"
    ),
    // S5 quintic (not solvable in radicals).
    Example(
      name = "x^5 - 4x + 2",
      poly = Poly(Vector(
        Rational(2), Rational(-4), Rational.zero,
        Rational.zero, Rational.zero, Rational.one
      )),
      notes = "S5 -- irreducible by Eisenstein (not solvable)"
    ),
    // S5 quintic (not solvable).
    Example(
      name = "x^5 - x - 1",
      poly = Poly(Vector(
        -Rational.one, -Rational.one, Rational.zero,
        Rational.zero, Rational.zero, Rational.one
      )),
      notes = "S5 (not solvable)"
    )
  )

  def main(args: Array[String]): Unit =
    println("Solvable Quintic Solver")
    println("=" * 60)
    println()

    for ex <- examples do
      println(s"--- ${ex.name} ---")
      println(s"  (${ex.notes})")

      val f = ex.poly

      // Compute discriminant
      val disc = Resolvent.discriminantOf(f)
      val discSq = Resolvent.isSquareRational(disc)
      println(s"  disc(f) = $disc")
      println(s"  disc is square: $discSq")

      // Identify Galois group
      identifyGaloisGroup5(f) match
        case None =>
          println("  (Galois group identification failed)")
        case Some(gr) =>
          val tg = gr.group
          println(s"  Galois group: ${tg.name} (order ${tg.order})")
          println(s"  Solvable: ${tg.solvable}")

          // Show numerical roots
          val realRoots = gr.roots.filter(r => math.abs(r.im) < 1e-6)
          println(s"  Real roots: ${realRoots.length}")
          realRoots.foreach(r => println(f"    ${r.re}%.10f"))

          if tg.solvable then
            // Try to construct radical expressions
            GaloisSolve.solve(f) match
              case SolveResult.Solved(radExprs, _) =>
                println("  Radical expressions:")
                for expr <- radExprs do
                  val v = Eval.evalComplex(expr)
                  println(s"    ${Pretty.pretty(expr)}")
                  println(f"    approx ${v.re}%.10f")
              case _ =>
                println("  (radical tower construction failed)")
          else
            println("  (not solvable -- no radical expression exists)")

      println()
