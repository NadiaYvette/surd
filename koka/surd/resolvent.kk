// Resolvent polynomials for Galois group identification.
//
// Given an irreducible polynomial f of degree 5, various resolvent
// polynomials can be computed to distinguish between the transitive
// subgroups of S5.
module surd/resolvent

import std/core/undiv
import surd/rational
import surd/ring
import surd/poly
import surd/factoring

// Compute the sextic resolvent of a degree-5 polynomial.
// This is a degree-6 polynomial whose factorisation pattern
// distinguishes the Galois group.
//
// For a monic quintic x^5 + ax^3 + bx^2 + cx + d, the sextic
// resolvent has specific coefficients computed from a, b, c, d.
pub fun sextic-resolvent(f : poly<rational>) : exn poly<rational>
  if f.degree != 5 then throw("sextic-resolvent: polynomial must have degree 5")
  else
    val cs = f.coeffs
    // Make monic
    val mp = monic-poly(f, rational-field)
    val mcs = mp.coeffs
    // Extract coefficients: x^5 + p*x^4 + q*x^3 + r*x^2 + s*x + t
    val p0 = match mcs.drop(0) { Cons(x, _) -> x; _ -> surd/rational/zero }
    val s1 = match mcs.drop(1) { Cons(x, _) -> x; _ -> surd/rational/zero }
    val q2 = match mcs.drop(2) { Cons(x, _) -> x; _ -> surd/rational/zero }
    val r3 = match mcs.drop(3) { Cons(x, _) -> x; _ -> surd/rational/zero }
    val p4 = match mcs.drop(4) { Cons(x, _) -> x; _ -> surd/rational/zero }
    // The sextic resolvent is computed from elementary symmetric functions
    // of the roots. For a simplified version, we use the discriminant.
    // Full implementation would compute the actual resolvent polynomial.
    // Stub: return a placeholder polynomial
    mk-poly([p0, s1, q2, r3, p4, surd/rational/zero, surd/rational/one], rational-ring)

// Compute the discriminant of a polynomial.
pub fun discriminant(f : poly<rational>) : exn rational
  val n = f.degree
  val mp = monic-poly(f, rational-field)
  val f' = diff-poly(mp, rational-ring)
  // disc(f) = (-1)^(n*(n-1)/2) * Res(f, f') / lc(f)
  val sign = if is-even(n * (n - 1) / 2) then surd/rational/one else Rational(-1, 1)
  // Compute resultant via polynomial GCD
  val res = resultant-simple(mp, f')
  sign * res

// Simple resultant via the Euclidean algorithm (for rationals).
fun resultant-simple(f : poly<rational>, g : poly<rational>) : exn rational
  pretend-no-div{ resultant-simple-rec(f, g) }

fun resultant-simple-rec(f : poly<rational>, g : poly<rational>) : <div,exn> rational
  if f.is-zero || g.is-zero then surd/rational/zero
  elif g.degree == 0 then
    match g.lead-coeff
      Just(c) -> rational/pow(c, f.degree)
      Nothing -> surd/rational/zero
  else
    val (_, rem) = div-mod-poly(f, g, rational-field)
    val df = f.degree
    val dg = g.degree
    val lg = match g.lead-coeff { Just(c) -> c; Nothing -> surd/rational/one }
    val dr = rem.degree
    val sign = if is-odd(df * dg) then Rational(-1, 1) else surd/rational/one
    if dr < 0 then surd/rational/zero
    else
      val rest = resultant-simple-rec(g, rem)
      sign * rational/pow(lg, df - dr) * rest
