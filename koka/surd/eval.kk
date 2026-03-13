// Numerical evaluation of radical expressions.
//
// eval-double: fast Double (float64) evaluation.
// eval-complex: Complex Double evaluation for expressions with complex intermediates.
module surd/eval

import std/core/undiv
import std/num/float64
import surd/rational
import surd/rad-expr

// A complex number as a pair of float64.
pub value struct complex
  re : float64
  im : float64

// Complex zero.
pub val complex-zero : complex = Complex(0.0, 0.0)

// Complex one.
pub val complex-one : complex = Complex(1.0, 0.0)

// Complex imaginary unit.
pub val complex-i : complex = Complex(0.0, 1.0)

// Complex from real.
pub fun complex-from-real(x : float64) : complex
  Complex(x, 0.0)

// Complex addition.
pub fun complex/(+)(a : complex, b : complex) : complex
  Complex(a.re + b.re, a.im + b.im)

// Complex subtraction.
pub fun complex/(-)(a : complex, b : complex) : complex
  Complex(a.re - b.re, a.im - b.im)

// Complex negation.
pub fun complex/negate(a : complex) : complex
  Complex(0.0 - a.re, 0.0 - a.im)

// Complex multiplication.
pub fun complex/(*)(a : complex, b : complex) : complex
  Complex(a.re * b.re - a.im * b.im, a.re * b.im + a.im * b.re)

// Complex magnitude squared.
pub fun magnitude-sq(z : complex) : float64
  z.re * z.re + z.im * z.im

// Complex magnitude.
pub fun magnitude(z : complex) : float64
  sqrt(magnitude-sq(z))

// Complex inverse.
pub fun complex/inv(z : complex) : complex
  val d = magnitude-sq(z)
  Complex(z.re / d, (0.0 - z.im) / d)

// Complex division.
pub fun complex/(/)(a : complex, b : complex) : complex
  a * inv(b)

// Complex integer power.
pub fun complex/pow-int(z : complex, n : int) : complex
  pretend-no-div{ complex-pow-rec(z, n) }

fun complex-pow-rec(z : complex, n : int) : div complex
  if n == 0 then complex-one
  elif n < 0 then complex-pow-rec(inv(z), 0 - n)
  elif n == 1 then z
  elif is-even(n) then
    val half = complex-pow-rec(z, n / 2)
    half * half
  else z * complex-pow-rec(z, n - 1)

// Principal nth root of a complex number via polar form.
// z = r * e^(i*theta) -> z^(1/n) = r^(1/n) * e^(i*theta/n)
pub fun complex-nth-root(n : int, z : complex) : complex
  val r = magnitude(z)
  val theta = atan2(z.im, z.re)
  val nf = n.float64
  val rn = float64/pow(r, 1.0 / nf)
  val an = theta / nf
  Complex(rn * cos(an), rn * sin(an))

// Show a complex number.
pub fun complex/show(z : complex) : string
  if z.im == 0.0 then z.re.show
  elif z.re == 0.0 then z.im.show ++ "i"
  elif z.im < 0.0 then z.re.show ++ " - " ++ (0.0 - z.im).show ++ "i"
  else z.re.show ++ " + " ++ z.im.show ++ "i"

// Convert a rational to float64.
fun rat-to-float64(r : rational) : float64
  r.num.float64 / r.den.float64

// ---------------------------------------------------------------
// Evaluation to float64
// ---------------------------------------------------------------

// Evaluate a radical expression to a float64.
// Note: expressions with even roots of negative numbers produce NaN.
// Use eval-complex for those.
pub fun eval-double(e : rad-expr<rational>) : div float64
  match e
    Lit(r)     -> rat-to-float64(r)
    Neg(a)     -> 0.0 - eval-double(a)
    RAdd(a, b) -> eval-double(a) + eval-double(b)
    RMul(a, b) -> eval-double(a) * eval-double(b)
    Inv(a)     -> 1.0 / eval-double(a)
    Root(n, a) -> float64/pow(eval-double(a), 1.0 / n.float64)
    Pow(a, n)  ->
      val v = eval-double(a)
      if n >= 0 then float64/pow(v, n.float64)
      else 1.0 / float64/pow(v, (0 - n).float64)

// ---------------------------------------------------------------
// Evaluation to Complex Double
// ---------------------------------------------------------------

// Evaluate a radical expression to a complex number.
// Handles expressions with complex intermediates (e.g., sqrt(-3)).
pub fun eval-complex(e : rad-expr<rational>) : div complex
  match e
    Lit(r)     -> complex-from-real(rat-to-float64(r))
    Neg(a)     -> eval-complex(a).negate
    RAdd(a, b) -> eval-complex(a) + eval-complex(b)
    RMul(a, b) -> eval-complex(a) * eval-complex(b)
    Inv(a)     -> inv(eval-complex(a))
    Root(n, a) -> complex-nth-root(n, eval-complex(a))
    Pow(a, n)  ->
      val v = eval-complex(a)
      if n >= 0 then pow-int(v, n)
      else inv(pow-int(v, 0 - n))
