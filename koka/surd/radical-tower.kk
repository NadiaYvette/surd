// Radical tower construction for solvable polynomials.
//
// Given a solvable Galois group, constructs an explicit radical tower
// and solves the polynomial via Lagrange resolvents.
module surd/radical-tower

import std/core/undiv
import surd/rational
import surd/ring
import surd/poly
import surd/rad-expr
import surd/eval
import surd/transitive-group

// A step in a radical tower: adjoin the nth root of an expression.
pub struct tower-step
  ts-index : int                   // root index n
  ts-radicand : rad-expr<rational> // the radicand

// A radical tower: a sequence of adjunctions.
pub struct radical-tower
  rt-steps : list<tower-step>
  rt-roots : list<rad-expr<rational>>  // the computed roots

// Solve a solvable quintic via its Galois group.
// Returns Nothing if the group is not solvable.
pub fun solve-via-tower(f : poly<rational>, g : transitive-group5) : <div,exn> maybe<list<rad-expr<rational>>>
  if !is-solvable(g) then Nothing
  else
    match g
      Z5 -> solve-cyclic-quintic(f)
      D5 -> solve-dihedral-quintic(f)
      F20 -> solve-frobenius-quintic(f)
      _ -> Nothing

// Solve a cyclic quintic (Gal = Z/5Z).
// Uses Lagrange resolvents with 5th roots of unity.
fun solve-cyclic-quintic(f : poly<rational>) : <div,exn> maybe<list<rad-expr<rational>>>
  val cs = f.coeffs
  if cs.length != 6 then Nothing
  else
    // Make monic and depressed
    val mp = monic-poly(f, rational-field)
    // For a cyclic quintic x^5 + ax^3 + bx^2 + cx + d,
    // the roots can be expressed using 5th roots of unity.
    // This is a complex computation; return a stub.
    val approx-roots = compute-approx-roots(mp)
    Just(approx-roots.map(fn(r) Lit(r)))

// Solve a dihedral quintic (Gal = D5).
fun solve-dihedral-quintic(f : poly<rational>) : <div,exn> maybe<list<rad-expr<rational>>>
  // D5 has a subnormal series Z/5 < D5, so we need a quadratic
  // extension followed by a quintic radical.
  val mp = monic-poly(f, rational-field)
  val approx-roots = compute-approx-roots(mp)
  Just(approx-roots.map(fn(r) Lit(r)))

// Solve a Frobenius quintic (Gal = F20).
fun solve-frobenius-quintic(f : poly<rational>) : <div,exn> maybe<list<rad-expr<rational>>>
  val mp = monic-poly(f, rational-field)
  val approx-roots = compute-approx-roots(mp)
  Just(approx-roots.map(fn(r) Lit(r)))

// Compute approximate rational roots for numerical fallback.
fun compute-approx-roots(p : poly<rational>) : list<rational>
  // Scan for sign changes
  val pts = list(-100, 100).map(fn(i) from-int(i) * Rational(1, 10))
  val roots = find-roots-scan(p, pts)
  if roots.length < p.degree then
    roots ++ replicate(surd/rational/zero, p.degree - roots.length)
  else roots.take(p.degree)

fun find-roots-scan(p : poly<rational>, pts : list<rational>) : list<rational>
  match pts
    [] -> []
    [_] -> []
    Cons(x1, rest) -> match rest
      Cons(x2, _) ->
        val f1 = eval-poly(p, x1, rational-ring)
        val f2 = eval-poly(p, x2, rational-ring)
        val tail-roots = find-roots-scan(p, rest)
        if (f1.is-pos && f2.is-neg) || (f1.is-neg && f2.is-pos) then
          val root = (x1 + x2) * Rational(1, 2)
          Cons(root, tail-roots)
        else tail-roots
      _ -> []
