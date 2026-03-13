// Top-level interface for solving polynomials via Galois group computation.
//
// For degree <= 4: returns Nothing (caller uses Cardano/Ferrari).
// For degree 5: identifies the Galois group; if solvable, solves
// via radical tower.
// For degree > 5: returns Nothing (not yet supported).
module surd/galois-solve

import std/core/undiv
import std/num/float64
import surd/rational
import surd/ring
import surd/poly
import surd/rad-expr
import surd/eval
import surd/alg-num
import surd/identify
import surd/radical-tower

// Solve a polynomial by radicals, if possible.
// Returns Nothing for degree <= 4 (handled by Cardano/Ferrari)
// or for non-solvable quintics (A5, S5).
pub fun solve-by-radicals(f : poly<rational>) : <div,exn> maybe<list<rad-expr<rational>>>
  val d = f.degree
  if d <= 4 then Nothing  // Caller handles via convert module
  elif d == 5 then
    val g = identify-galois-group5(f)
    solve-via-tower(f, g)
  else Nothing  // Degree > 5 not supported yet

// Solve for a specific algebraic number.
// Returns the radical expression for the root closest to the given
// algebraic number, if the polynomial is solvable.
pub fun solve-alg-num(a : alg-num) : <div,exn> maybe<rad-expr<rational>>
  val p = a.an-min-poly
  match solve-by-radicals(p)
    Nothing -> Nothing
    Just(roots) ->
      // Pick the root closest to our algebraic number
      val target = alg-approx(a)
      val target-dbl = target.num.float64 / target.den.float64
      pick-closest-root(roots, target-dbl)

fun pick-closest-root(roots : list<rad-expr<rational>>, target : float64) : <div,exn> maybe<rad-expr<rational>>
  match roots
    [] -> Nothing
    [r] -> Just(r)
    Cons(r, rest) ->
      val d = abs(eval-double(r) - target)
      pick-loop(rest, r, d, target)

fun pick-loop(roots : list<rad-expr<rational>>, best : rad-expr<rational>,
              best-dist : float64, target : float64) : <div,exn> maybe<rad-expr<rational>>
  match roots
    [] -> Just(best)
    Cons(r, rest) ->
      val d = abs(eval-double(r) - target)
      if d < best-dist then pick-loop(rest, r, d, target)
      else pick-loop(rest, best, best-dist, target)
