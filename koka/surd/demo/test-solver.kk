// Quick test of the generalized solver pipeline.
module surd/demo/test-solver

import std/num/float64
import surd/rational
import surd/ring
import surd/poly
import surd/rad-expr
import surd/eval
import surd/identify
import surd/galois-solve
import surd/radical-tower
import surd/transitive-group

pub fun main() : <console,div,exn> ()
  // x^3 - 2: simplest solvable case
  val cs3 = [Rational(-2, 1), surd/rational/zero, surd/rational/zero, surd/rational/one]
  val p3 = mk-poly(cs3, rational-ring)
  println("Testing x^3 - 2:")

  // First check root computation
  val roots = compute-complex-roots(p3)
  println("  Roots found: " ++ roots.length.show)
  roots.foreach fn(r)
    println("    " ++ r.show)

  // Check that roots are actually roots
  roots.foreach fn(r)
    val z = r * r * r
    println("    r^3 = " ++ z.show)

  // Identify Galois group
  match identify-galois-group(p3)
    Just(gr) ->
      println("  Galois group: " ++ gr.gr-group.tg-name)
      println("  Solvable: " ++ gr.gr-group.tg-solvable.show)
      println("  GR roots: " ++ gr.gr-roots.length.show)
      gr.gr-roots.foreach fn(r)
        println("    " ++ r.show)
    Nothing ->
      println("  Could not identify Galois group")

  // Test cyclic ordering directly
  match identify-galois-group(p3)
    Just(gr) ->
      match solve-via-tower-n(gr, p3)
        Just(roots') ->
          println("  Solved! Roots:")
          roots'.foreach fn(r)
            println("    " ++ rad-rational/show(r))
        Nothing ->
          println("  solve-via-tower-n returned Nothing")
    Nothing -> ()

  // Try scoring orderings manually for x^3 - 2
  println("  Testing orderings for x^3 - 2:")
  val dep-roots3 = roots  // already depressed since x^2 coeff is 0
  val tg3 = match identify-galois-group(p3)
    Just(gr) -> gr.gr-group
    Nothing -> trans-groups-of-degree(3).head.unjust
  val orderings3 = [[0,1,2], [0,2,1]]
  orderings3.foreach fn(o)
    val sc = score-ordering-n(dep-roots3, o, 3, tg3)
    println("    ordering " ++ o.map(show).join(",") ++ " -> score " ++ sc.show)

  // Try solving
  match solve-by-radicals(p3)
    Just(roots') ->
      println("  Solved! Roots:")
      roots'.foreach fn(r)
        println("    " ++ rad-rational/show(r))
    Nothing ->
      println("  solve-by-radicals returned Nothing")

  // Also test a degree-5 example: x^5 - 2
  println("")
  println("Testing x^5 - 2:")
  val cs5 = [Rational(-2, 1), surd/rational/zero, surd/rational/zero,
             surd/rational/zero, surd/rational/zero, surd/rational/one]
  val p5 = mk-poly(cs5, rational-ring)

  val roots5 = compute-complex-roots(p5)
  println("  Roots found: " ++ roots5.length.show)
  roots5.foreach fn(r)
    val z = complex/pow-int(r, 5)
    println("    r = " ++ r.show ++ "  r^5 = " ++ z.show)

  match identify-galois-group(p5)
    Just(gr) ->
      println("  Galois group: " ++ gr.gr-group.tg-name)
      println("  Solvable: " ++ gr.gr-group.tg-solvable.show)
    Nothing ->
      println("  Could not identify Galois group")

  match solve-by-radicals(p5)
    Just(roots') ->
      println("  Solved! " ++ roots'.length.show ++ " roots")
    Nothing ->
      println("  solve-by-radicals returned Nothing")
