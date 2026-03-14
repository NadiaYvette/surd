// Top-level interface for solving polynomials via Galois group computation.
//
// For degree <= 4: returns Nothing (caller uses Cardano/Ferrari).
// For prime degree >= 5: identifies the Galois group; if solvable,
// solves via radical tower with Lagrange resolvents.
// For composite degree > 4: returns Nothing (not yet supported).
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
import surd/transitive-group
import surd/prime-factors

// Solve a polynomial by radicals, if possible.
// Returns Nothing for degree <= 2 (trivial),
// for non-solvable groups (A_p, S_p with p >= 5), or for
// unsupported composite degrees.
// For degree 3-4, the caller may prefer Cardano/Ferrari from the
// convert module; use solve-by-radicals-high for degree >= 5 only.
pub fun solve-by-radicals(f : poly<rational>) : <div,exn> maybe<list<rad-expr<rational>>>
  val d = f.degree
  if d <= 2 then Nothing
  elif is-prime(d) then
    match identify-galois-group(f)
      Just(gr) ->
        if gr.gr-group.tg-solvable then
          solve-via-tower-n(gr, f)
        else Nothing
      Nothing -> Nothing
  else Nothing  // Composite degree > 4 not supported yet

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

// Identify the Galois group and solve, returning group name and roots.
pub fun identify-and-solve(f : poly<rational>) : <div,exn> maybe<(string, list<rad-expr<rational>>)>
  val d = f.degree
  if d <= 4 then Nothing
  elif is-prime(d) then
    match identify-galois-group(f)
      Just(gr) ->
        val tg = gr.gr-group
        if !tg.tg-solvable then Nothing
        else
          match solve-via-tower-n(gr, f)
            Just(roots) -> Just((tg.tg-name, roots))
            Nothing -> Nothing
      Nothing -> Nothing
  else Nothing

fun pick-closest-root(roots : list<rad-expr<rational>>, target : float64) : <div,exn> maybe<rad-expr<rational>>
  match roots
    [] -> Nothing
    [r] -> Just(r)
    Cons(r, rest) ->
      val d = float64/abs(eval-double(r) - target)
      pick-loop(rest, r, d, target)

fun pick-loop(roots : list<rad-expr<rational>>, best : rad-expr<rational>,
              best-dist : float64, target : float64) : <div,exn> maybe<rad-expr<rational>>
  match roots
    [] -> Just(best)
    Cons(r, rest) ->
      val d = float64/abs(eval-double(r) - target)
      if d < best-dist then pick-loop(rest, r, d, target)
      else pick-loop(rest, best, best-dist, target)
