// Demo: solving solvable polynomials of prime degree.
//
// Identifies the Galois group of prime-degree polynomials and,
// when solvable, expresses the roots in radicals.
// Supports degree 5, 7, 11, 13, ... (all primes).
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

// Test polynomials of degree 5.
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

// Test polynomial of degree 7: x^7 - 2 (Galois group AGL(1,7), solvable)
val poly5-coeffs : list<rational> = [Rational(-2, 1), surd/rational/zero, surd/rational/zero,
                                      surd/rational/zero, surd/rational/zero, surd/rational/zero,
                                      surd/rational/zero, surd/rational/one]

// Test polynomial of degree 3: x^3 - 2 (cyclic, solvable, degree-3 prime)
val poly6-coeffs : list<rational> = [Rational(-2, 1), surd/rational/zero, surd/rational/zero,
                                      surd/rational/one]

pub fun main() : <console,div,exn> ()
  println("=" ++ repeat-char('=', 60))
  println("  Solvable Polynomial Solver Demo (all prime degrees)")
  println("=" ++ repeat-char('=', 60))
  println("")

  val polys = [
    ("x^5 - 5x + 12 (deg 5)", poly1-coeffs),
    ("x^5 - 2 (deg 5)", poly2-coeffs),
    ("x^5 - x - 1 (deg 5)", poly3-coeffs),
    ("x^5 + x^4 - 4x^3 - 3x^2 + 3x + 1 (deg 5, cyclic)", poly4-coeffs),
    ("x^7 - 2 (deg 7)", poly5-coeffs),
    ("x^3 - 2 (deg 3)", poly6-coeffs)
  ]

  polys.foreach fn(entry)
    val name = entry.fst
    val cs = entry.snd
    val p = mk-poly(cs, rational-ring)
    println("Polynomial: " ++ name)
    println("  Coefficients: " ++ poly/show(p, rational-ring))

    val deg = p.degree

    // Use the general identification path
    match identify-galois-group(p)
      Just(gr) ->
        println("  Galois group: " ++ gr.gr-group.tg-name)
        println("  Solvable: " ++ (if gr.gr-group.tg-solvable then "yes" else "no"))
        println("  Group order: " ++ gr.gr-group.tg-order.show)

        // Try to solve
        match solve-by-radicals(p)
          Just(roots) ->
            println("  Roots in radicals:")
            roots.foreach fn(r)
              val approx = eval-double(r)
              println("    " ++ surd/pretty/pretty(r) ++ "  ~  " ++ approx.show)
          Nothing ->
            println("  Not solvable by radicals")
      Nothing ->
        println("  Galois group identification not supported for degree " ++ deg.show)

        // Still try the legacy path for degree 5
        if deg == 5 then
          val g = identify-galois-group5(p)
          println("  (Legacy) Galois group: " ++ transitive-group5/show(g))
          println("  (Legacy) Solvable: " ++ (if transitive-group5/is-solvable(g) then "yes" else "no"))

    println("")

  // Print transitive groups for various prime degrees
  println("--- Transitive groups database ---")
  [3, 5, 7, 11, 13].foreach fn(p)
    val groups = trans-groups-of-degree(p)
    println("Degree " ++ p.show ++ ": " ++ groups.length.show ++ " transitive groups")
    groups.foreach fn(g)
      println("  " ++ g.tg-name ++ " (order " ++ g.tg-order.show
              ++ ", solvable=" ++ g.tg-solvable.show
              ++ ", factors=" ++ g.tg-composition-factors.map(show).join(",")
              ++ ")")
    println("")

  println("Done.")

fun repeat-char(c : char, n : int) : string
  list(1, n).map(fn(_) c.string).join("")
