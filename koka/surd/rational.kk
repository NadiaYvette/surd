// Rational number arithmetic with arbitrary-precision integers.
//
// Rationals are stored in lowest terms with a positive denominator.
module surd/rational

import std/core/undiv

// A rational number num/den, always in lowest terms with den > 0.
pub value struct rational
  num : int
  den : int

// GCD via Euclidean algorithm, always returns non-negative.
fun gcd-rec(a : int, b : int) : div int
  if b == 0 then a
  else gcd-rec(b, a % b)

pub fun gcd-int(a : int, b : int) : int
  pretend-no-div{ gcd-rec(abs(a), abs(b)) }

// Smart constructor: normalizes sign and reduces by GCD.
// Throws on zero denominator.
pub fun rational(n : int, d : int) : exn rational
  if d == 0 then throw("rational: zero denominator")
  elif n == 0 then Rational(0, 1)
  else
    val sign = if d < 0 then -1 else 1
    val g = gcd-int(abs(n), abs(d))
    Rational((sign * n) / g, (sign * d) / g)

// Construct a rational from an integer.
pub fun from-int(n : int) : rational
  Rational(n, 1)

// Numerator.
pub fun numer(r : rational) : int
  r.num

// Denominator (always positive).
pub fun denom(r : rational) : int
  r.den

// The zero rational.
pub val zero : rational = Rational(0, 1)

// The one rational.
pub val one : rational = Rational(1, 1)

// Addition.
pub fun rational/(+)(a : rational, b : rational) : rational
  val n = a.num * b.den + b.num * a.den
  val d = a.den * b.den
  if n == 0 then Rational(0, 1)
  else
    val g = gcd-int(abs(n), abs(d))
    Rational(n / g, d / g)

// Subtraction.
pub fun rational/(-)(a : rational, b : rational) : rational
  a + Rational(0 - b.num, b.den)

// Negation.
pub fun rational/negate(a : rational) : rational
  Rational(0 - a.num, a.den)

// Multiplication.
pub fun rational/(*)(a : rational, b : rational) : rational
  val n = a.num * b.num
  val d = a.den * b.den
  if n == 0 then Rational(0, 1)
  else
    val g = gcd-int(abs(n), abs(d))
    Rational(n / g, d / g)

// Division. Throws on division by zero.
pub fun rational/(/)(a : rational, b : rational) : exn rational
  if b.num == 0 then throw("rational division by zero")
  else a * Rational(b.den, b.num).normalize-sign

// Normalize sign (internal helper, den might be negative after Rational construction).
fun normalize-sign(r : rational) : rational
  if r.den < 0 then Rational(0 - r.num, 0 - r.den)
  else r

// Multiplicative inverse. Throws on zero.
pub fun rational/inv(r : rational) : exn rational
  if r.num == 0 then throw("rational inverse of zero")
  elif r.num < 0 then Rational(0 - r.den, 0 - r.num)
  else Rational(r.den, r.num)

// Absolute value.
pub fun rational/abs(r : rational) : rational
  Rational(abs(r.num), r.den)

// Signum: -1, 0, or 1.
pub fun rational/signum(r : rational) : rational
  if r.num > 0 then Rational(1, 1)
  elif r.num < 0 then Rational(-1, 1)
  else Rational(0, 1)

// Integer power.
pub fun rational/pow(r : rational, n : int) : rational
  pretend-no-div{ pow-rec(r, n) }

fun pow-rec(r : rational, n : int) : div rational
  if n == 0 then Rational(1, 1)
  elif n == 1 then r
  elif n < 0 then
    // For negative exponents, invert first
    val ri = Rational(r.den, r.num).normalize-sign
    pow-rec(ri, 0 - n)
  elif n % 2 == 0 then
    val half = pow-rec(r, n / 2)
    half * half
  else r * pow-rec(r, n - 1)

// Comparison: returns -1, 0, or 1.
pub fun rational/cmp(a : rational, b : rational) : order
  val lhs = a.num * b.den
  val rhs = b.num * a.den
  if lhs < rhs then Lt
  elif lhs > rhs then Gt
  else Eq

// Equality.
pub fun rational/(==)(a : rational, b : rational) : bool
  a.num == b.num && a.den == b.den

// Inequality.
pub fun rational/(!=)(a : rational, b : rational) : bool
  !(a == b)

// Less than.
pub fun rational/(<)(a : rational, b : rational) : bool
  a.cmp(b).is-lt

// Greater than.
pub fun rational/(>)(a : rational, b : rational) : bool
  a.cmp(b).is-gt

// Less than or equal.
pub fun rational/(<=)(a : rational, b : rational) : bool
  !a.cmp(b).is-gt

// Greater than or equal.
pub fun rational/(>=)(a : rational, b : rational) : bool
  !a.cmp(b).is-lt

// Minimum.
pub fun rational/min(a : rational, b : rational) : rational
  if a <= b then a else b

// Maximum.
pub fun rational/max(a : rational, b : rational) : rational
  if a >= b then a else b

// Display as "num/den" or just "num" when den is 1.
pub fun rational/show(r : rational) : string
  if r.den == 1 then r.num.show
  else r.num.show ++ "/" ++ r.den.show

// Check if the rational is zero.
pub fun rational/is-zero(r : rational) : bool
  r.num == 0

// Check if the rational is negative.
pub fun rational/is-neg(r : rational) : bool
  r.num < 0

// Check if the rational is positive.
pub fun rational/is-pos(r : rational) : bool
  r.num > 0

// Floor division: largest integer <= r.
pub fun rational/floor(r : rational) : int
  if r.den == 1 then r.num
  elif r.num >= 0 then r.num / r.den
  else (r.num - r.den + 1) / r.den

// Ceiling: smallest integer >= r.
pub fun rational/ceiling(r : rational) : int
  if r.den == 1 then r.num
  elif r.num >= 0 then (r.num + r.den - 1) / r.den
  else r.num / r.den
