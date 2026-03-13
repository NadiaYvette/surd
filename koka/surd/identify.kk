// Galois group identification for degree-5 polynomials.
//
// Given an irreducible polynomial of degree 5 over Q, identifies
// its Galois group as one of the 5 transitive subgroups of S5.
module surd/identify

import std/core/undiv
import std/num/float64
import surd/rational
import surd/ring
import surd/poly
import surd/factoring
import surd/resolvent
import surd/transitive-group

// Identify the Galois group of an irreducible degree-5 polynomial.
//
// Algorithm:
// 1. Compute the discriminant D.
// 2. If D is a perfect square, Gal(f) is contained in A5.
// 3. Compute the sextic resolvent and check its factoring pattern.
// 4. Use the resolvent factoring to distinguish between groups.
pub fun identify-galois-group5(f : poly<rational>) : exn transitive-group5
  if f.degree != 5 then throw("identify-galois-group5: degree must be 5")
  else
    val d = discriminant(f)
    val d-is-square = is-perfect-square-rational(d)
    // Compute the sextic resolvent
    val res = sextic-resolvent(f)
    val res-factors = factor-square-free(res)
    val n-factors = res-factors.length
    val factor-degs = res-factors.map(fn(p) p.degree)
    // Decision tree based on discriminant and resolvent factoring
    if d-is-square then
      // Gal(f) <= A5
      if has-linear-factor(factor-degs) then
        if n-factors >= 4 then Z5
        else D5
      else A5Group
    else
      // Gal(f) not in A5
      if has-linear-factor(factor-degs) then F20
      else S5Group

fun has-linear-factor(degs : list<int>) : bool
  degs.any(fn(d) d == 1)

fun is-perfect-square-rational(r : rational) : bool
  if r.is-neg then False
  elif r.is-zero then True
  else
    match exact-int-sqrt(r.num)
      Just(_) -> match exact-int-sqrt(r.den)
        Just(_) -> True
        Nothing -> False
      Nothing -> False

fun exact-int-sqrt(n : int) : maybe<int>
  if n < 0 then Nothing
  elif n == 0 then Just(0)
  elif n == 1 then Just(1)
  else
    val s = float64/sqrt(n.float64).int
    if s * s == n then Just(s)
    elif (s + 1) * (s + 1) == n then Just(s + 1)
    else Nothing
