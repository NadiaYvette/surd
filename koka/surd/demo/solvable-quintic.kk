// Demo: solving solvable quintic polynomials.
//
// Identifies the Galois group of degree-5 polynomials and,
// when solvable, expresses the roots in radicals.
module surd/demo/solvable-quintic

import std/num/float64
import surd/rational
import surd/ring
import surd/poly
import surd/rad-expr
import surd/eval
import surd/pretty
import surd/identify
import surd/galois-solve
import surd/resolvent
import surd/transitive-group

// Test polynomials.
// x^5 - 5x + 12 has Galois group D5 (dihedral, solvable)
val poly1-coeffs : list<rational> = [from-int(12), surd/rational/zero, surd/rational/zero,
                                      Rational(-5, 1), surd/rational/zero, surd/rational/one]

// x^5 - 2 has Galois group F20 (Frobenius, solvable)
val poly2-coeffs : list<rational> = [Rational(-2, 1), surd/rational/zero, surd/rational/zero,
                                      surd/rational/zero, surd/rational/zero, surd/rational/one]

// x^5 - x - 1 has Galois group S5 (not solvable)
val poly3-coeffs : list<rational> = [Rational(-1, 1), Rational(-1, 1), surd/rational/zero,
                                      surd/rational/zero, surd/rational/zero, surd/rational/one]

// x^5 + x^4 - 4x^3 - 3x^2 + 3x + 1 = Phi_11(x)/(x-1), has cyclic Galois group
val poly4-coeffs : list<rational> = [surd/rational/one, from-int(3), Rational(-3, 1),
                                      Rational(-4, 1), surd/rational/one, surd/rational/one]

pub fun main() : <console,div,exn> ()
  println("=" ++ repeat-char('=', 60))
  println("  Solvable Quintic Polynomial Demo")
  println("=" ++ repeat-char('=', 60))
  println("")

  val polys = [
    ("x^5 - 5x + 12", poly1-coeffs),
    ("x^5 - 2", poly2-coeffs),
    ("x^5 - x - 1", poly3-coeffs),
    ("x^5 + x^4 - 4x^3 - 3x^2 + 3x + 1", poly4-coeffs)
  ]

  polys.foreach fn(entry)
    val name = entry.fst
    val cs = entry.snd
    val p = mk-poly(cs, rational-ring)
    println("Polynomial: " ++ name)
    println("  Coefficients: " ++ poly/show(p, rational-ring))

    // Compute discriminant
    val disc = discriminant(p)
    println("  Discriminant: " ++ disc.show)

    // Identify Galois group
    val g = identify-galois-group5(p)
    println("  Galois group: " ++ transitive-group5/show(g))
    println("  Solvable: " ++ (if is-solvable(g) then "yes" else "no"))
    println("  Group order: " ++ group-order5(g).show)

    // Try to solve
    match solve-by-radicals(p)
      Just(roots) ->
        println("  Roots in radicals:")
        roots.foreach fn(r)
          val approx = eval-double(r)
          println("    " ++ surd/pretty/pretty(r) ++ "  ~  " ++ approx.show)
      Nothing ->
        println("  Not solvable by radicals (or degree <= 4)")

    println("")

  println("Done.")

fun repeat-char(c : char, n : int) : string
  list(1, n).map(fn(_) c.string).join("")
