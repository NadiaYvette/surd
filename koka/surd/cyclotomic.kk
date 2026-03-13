// Cyclotomic polynomials and related number-theoretic utilities.
//
// The nth cyclotomic polynomial Phi_n(x) is the minimal polynomial of
// primitive nth roots of unity over Q. It has degree phi(n) (Euler's totient).
module surd/cyclotomic

import std/core/undiv
import surd/rational
import surd/ring
import surd/poly
import surd/positive
import surd/prime-factors

// Compute the nth cyclotomic polynomial Phi_n(x) over Q.
//
// Uses the identity: x^n - 1 = prod_{d|n} Phi_d(x)
// So Phi_n(x) = (x^n - 1) / prod_{d|n, d<n} Phi_d(x)
pub fun cyclotomic(n : int) : <div,exn> poly<rational>
  if n <= 0 then throw("cyclotomic: non-positive argument")
  elif n == 1 then mk-poly([Rational(-1, 1), surd/rational/one], rational-ring) // x - 1
  else
    val xnm1 = x-to-the-n-minus1(n)
    val divs = proper-divisors(n)
    val denom = divs.foldl(const-poly(surd/rational/one, rational-ring)) fn(acc, d)
      mul-poly(acc, cyclotomic(d), rational-ring)
    div-mod-poly(xnm1, denom, rational-field).fst

// x^n - 1
fun x-to-the-n-minus1(n : int) : poly<rational>
  val cs = [Rational(-1, 1)] ++ replicate(surd/rational/zero, n - 1) ++ [surd/rational/one]
  mk-poly(cs, rational-ring)

// Proper divisors of n (all divisors except n itself), ascending.
fun proper-divisors(n : int) : list<int>
  list(1, n - 1).filter(fn(d) n % d == 0)

// Euler's totient function phi(n).
pub fun eulers-totient(n : int) : exn int
  if n <= 0 then throw("eulers-totient: non-positive argument")
  elif n == 1 then 1
  else
    val fs = factorise(unsafe-positive(n))
    fs.foldl(1) fn(acc, f)
      val p = f.fst
      val e = f.snd
      acc * (p - 1) * pow-int(p, e - 1)

// Integer power helper (non-negative exponent).
fun pow-int(base : int, exp : int) : int
  pretend-no-div{ pow-int-rec(base, exp) }

fun pow-int-rec(base : int, exp : int) : div int
  if exp <= 0 then 1
  elif exp == 1 then base
  elif is-even(exp) then
    val half = pow-int-rec(base, exp / 2)
    half * half
  else base * pow-int-rec(base, exp - 1)

// Moebius function mu(n).
pub fun moebius-mu(n : int) : exn int
  if n <= 0 then throw("moebius-mu: non-positive argument")
  elif n == 1 then 1
  else
    val fs = factorise(unsafe-positive(n))
    if fs.any(fn(f) f.snd > 1) then 0
    elif is-even(fs.length) then 1
    else -1
