// Express roots of unity as radical expressions.
//
// A primitive nth root of unity zeta_n = e^(2*pi*i/n) can ALWAYS be
// expressed in radicals, since cyclotomic extensions have abelian
// Galois groups.
//
// For small n and constructible cases, returns optimised closed-form
// expressions. For general n, uses the Gauss period descent.
module surd/root-of-unity

import std/core/undiv
import surd/rational
import surd/rad-expr
import surd/positive
import surd/prime-factors
import surd/trig-galois

// Known Fermat primes.
pub val fermat-primes : list<int> = [3, 5, 17, 257, 65537]

// Check if cos(2pi/n) is constructible (expressible using only square roots).
pub fun is-constructible(n : int) : bool
  match positive(n)
    Nothing -> False
    Just(pn) ->
      val fs = factorise(pn)
      val odd-factors = fs.filter(fn(f) f.fst != 2)
      odd-factors.all fn(f)
        f.snd == 1 && fermat-primes.any(fn(fp) fp == f.fst)

// Compute cos(2pi/n) as a radical expression.
// Always succeeds for n >= 1 with known small cases.
pub fun cos-of-unity(n : int) : <div,exn> maybe<rad-expr<rational>>
  if n <= 0 then Nothing
  elif n == 1 then Just(Lit(surd/rational/one))
  elif n == 2 then Just(Lit(Rational(-1, 1)))
  elif n == 3 then Just(Lit(Rational(-1, 2)))
  elif n == 4 then Just(Lit(surd/rational/zero))
  elif n == 5 then
    // cos(2pi/5) = (-1 + sqrt(5))/4
    val sq5 = Root(2, Lit(from-int(5)))
    Just(RMul(Lit(Rational(1, 4)), RAdd(Lit(Rational(-1, 1)), sq5)))
  elif n == 6 then Just(Lit(Rational(1, 2)))
  elif n == 8 then
    // cos(2pi/8) = cos(pi/4) = sqrt(2)/2
    Just(RMul(Lit(Rational(1, 2)), Root(2, Lit(from-int(2)))))
  elif n == 10 then
    // cos(2pi/10) = cos(pi/5) = (1 + sqrt(5))/4
    val sq5 = Root(2, Lit(from-int(5)))
    Just(RMul(Lit(Rational(1, 4)), RAdd(Lit(surd/rational/one), sq5)))
  elif n == 12 then
    // cos(2pi/12) = cos(pi/6) = sqrt(3)/2
    Just(RMul(Lit(Rational(1, 2)), Root(2, Lit(from-int(3)))))
  elif n == 16 then
    // cos(2pi/16) = sqrt(2 + sqrt(2))/2
    val inner = RAdd(Lit(from-int(2)), Root(2, Lit(from-int(2))))
    Just(RMul(Lit(Rational(1, 2)), Root(2, inner)))
  elif n == 17 then
    // cos(2pi/17) — the famous Gauss constructible 17-gon
    // = (-1 + sqrt(17) + sqrt(34 - 2*sqrt(17)) +
    //    2*sqrt(17 + 3*sqrt(17) - sqrt(34 - 2*sqrt(17)) - 2*sqrt(34 + 2*sqrt(17)))) / 16
    // Use Gauss descent for this
    cos-of-unity-via-gauss(n)
  elif n == 20 then
    // cos(2pi/20) = cos(pi/10) = sqrt(10 + 2*sqrt(5))/4
    val sq5 = Root(2, Lit(from-int(5)))
    val inner = RAdd(Lit(from-int(10)), RMul(Lit(from-int(2)), sq5))
    Just(RMul(Lit(Rational(1, 4)), Root(2, inner)))
  elif n == 24 then
    // cos(2pi/24) = cos(pi/12) = (sqrt(6) + sqrt(2))/4
    val sq6 = Root(2, Lit(from-int(6)))
    val sq2 = Root(2, Lit(from-int(2)))
    Just(RMul(Lit(Rational(1, 4)), RAdd(sq6, sq2)))
  else
    // General case: Gauss period descent
    cos-of-unity-via-gauss(n)

// Compute sin(2pi/n) as a radical expression.
pub fun sin-of-unity(n : int) : <div,exn> maybe<rad-expr<rational>>
  if n <= 0 then Nothing
  elif n == 1 then Just(Lit(surd/rational/zero))
  elif n == 2 then Just(Lit(surd/rational/zero))
  elif n == 3 then
    // sin(2pi/3) = sqrt(3)/2
    Just(RMul(Lit(Rational(1, 2)), Root(2, Lit(from-int(3)))))
  elif n == 4 then Just(Lit(surd/rational/one))
  elif n == 6 then
    // sin(2pi/6) = sin(pi/3) = sqrt(3)/2
    Just(RMul(Lit(Rational(1, 2)), Root(2, Lit(from-int(3)))))
  elif n == 8 then
    // sin(2pi/8) = sqrt(2)/2
    Just(RMul(Lit(Rational(1, 2)), Root(2, Lit(from-int(2)))))
  elif n == 12 then
    // sin(2pi/12) = 1/2
    Just(Lit(Rational(1, 2)))
  else
    // sin(2pi/n) = sqrt(1 - cos^2(2pi/n))
    match cos-of-unity(n)
      Just(c) ->
        val sin2 = RAdd(Lit(surd/rational/one), Neg(RMul(c, c)))
        Just(Root(2, sin2))
      Nothing -> Nothing

// Get all cos(2pi*k/n) values for k = 0..n-1.
pub fun all-cos-of-unity(n : int) : <div,exn> list<(int, rad-expr<rational>)>
  list(0, n - 1).filter-map fn(k)
    val g = gcd-int(k, n)
    val k' = k / g
    val n' = n / g
    match cos-of-unity(n')
      Just(e) -> Just((k, e))
      Nothing -> Nothing

// Get all sin(2pi*k/n) values for k = 0..n-1.
pub fun all-sin-of-unity(n : int) : <div,exn> list<(int, rad-expr<rational>)>
  list(0, n - 1).filter-map fn(k)
    val g = gcd-int(k, n)
    val k' = k / g
    val n' = n / g
    match sin-of-unity(n')
      Just(e) -> Just((k, e))
      Nothing -> Nothing
