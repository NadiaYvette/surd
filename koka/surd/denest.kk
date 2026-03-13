// Radical denesting: simplifying nested radicals.
//
// sqrt(2 + sqrt(3)) can sometimes be expressed as a + b*sqrt(c).
// This module implements denesting for square roots and dispatches
// to higher-root denesting when applicable.
module surd/denest

import std/core/undiv
import std/num/float64
import surd/rational
import surd/rad-expr
import surd/normalize
import surd/eval

// Main entry point: try to denest a radical expression.
// Applies square root denesting (Borodin's algorithm) and basic
// simplifications. Returns the simplified expression.
pub fun denest(e : rad-expr<rational>) : <div,exn> rad-expr<rational>
  match e
    Root(2, inner) ->
      // Try square root denesting
      val inner' = denest(inner)
      match try-sqrt-denest(inner')
        Just(result) -> result
        Nothing -> Root(2, inner')
    Root(n, inner) ->
      val inner' = denest(inner)
      Root(n, inner')
    Neg(a) -> Neg(denest(a))
    RAdd(a, b) -> RAdd(denest(a), denest(b))
    RMul(a, b) -> RMul(denest(a), denest(b))
    Inv(a) -> Inv(denest(a))
    Pow(a, n) -> Pow(denest(a), n)
    _ -> e

// Try to denest sqrt(a + b*sqrt(c)) = sqrt(x) + sqrt(y)
// where x + y = a and x*y = b^2*c/4.
//
// This is Borodin's algorithm for denesting square roots of the form
// a + b*sqrt(c) where a,b are rational and c is a positive integer.
fun try-sqrt-denest(inner : rad-expr<rational>) : <div,exn> maybe<rad-expr<rational>>
  // Check if inner has the form a + b*sqrt(c)
  match decompose-sqrt-sum(inner)
    Just((a, b, c)) ->
      // sqrt(a + b*sqrt(c)) = sqrt(x) + sqrt(y)
      // where x + y = a and x*y = b^2*c/4
      // So x, y are roots of t^2 - a*t + b^2*c/4 = 0
      val disc-num = a * a - b * b * c
      if disc-num.is-neg then Nothing
      else
        // Check if discriminant is a perfect square
        val disc = disc-num
        match exact-rational-sqrt(disc)
          Just(sd) ->
            val x = (a + sd) * Rational(1, 2)
            val y = (a - sd) * Rational(1, 2)
            if x.is-neg || y.is-neg then Nothing
            else
              // sqrt(a + b*sqrt(c)) = sqrt(x) + sign(b)*sqrt(y)
              val sign = if b.is-neg then Neg else fn(e1) e1
              Just(RAdd(Root(2, Lit(x)), sign(Root(2, Lit(y)))))
          Nothing -> Nothing
    Nothing -> Nothing

// Decompose an expression into a + b*sqrt(c) form.
// Returns (a, b, c) where a, b are rational and c is rational.
fun decompose-sqrt-sum(e : rad-expr<rational>) : <div,exn> maybe<(rational, rational, rational)>
  match e
    RAdd(Lit(a), RMul(Lit(b), Root(2, Lit(c)))) ->
      Just((a, b, c))
    RAdd(Lit(a), Root(2, Lit(c))) ->
      Just((a, surd/rational/one, c))
    RAdd(Lit(a), Neg(RMul(Lit(b), Root(2, Lit(c))))) ->
      Just((a, b.negate, c))
    RAdd(Lit(a), Neg(Root(2, Lit(c)))) ->
      Just((a, Rational(-1, 1), c))
    RAdd(RMul(Lit(b), Root(2, Lit(c))), Lit(a)) ->
      Just((a, b, c))
    RAdd(Root(2, Lit(c)), Lit(a)) ->
      Just((a, surd/rational/one, c))
    _ -> Nothing

// Check if a rational is a perfect square and return the root.
fun exact-rational-sqrt(r : rational) : maybe<rational>
  if r.is-neg then Nothing
  elif r.is-zero then Just(surd/rational/zero)
  else
    val n = r.num
    val d = r.den
    match exact-int-sqrt(n)
      Just(sn) -> match exact-int-sqrt(d)
        Just(sd) -> Just(Rational(sn, sd))
        Nothing -> Nothing
      Nothing -> Nothing

// Check if an integer is a perfect square.
fun exact-int-sqrt(n : int) : maybe<int>
  if n < 0 then Nothing
  elif n == 0 then Just(0)
  elif n == 1 then Just(1)
  else
    val s = float64/sqrt(n.float64).int
    // Check s and s+1 (floating point might be off by 1)
    if s * s == n then Just(s)
    elif (s + 1) * (s + 1) == n then Just(s + 1)
    elif s > 0 && (s - 1) * (s - 1) == n then Just(s - 1)
    else Nothing
