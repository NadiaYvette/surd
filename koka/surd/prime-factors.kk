// Small prime factorisation and related utilities.
module surd/prime-factors

import std/core/undiv
import surd/positive

// Trial-division primality test.
pub fun is-prime(n : int) : bool
  if n < 2 then False
  elif n < 4 then True
  else pretend-no-div{ trial-divide(n, 3) }

fun trial-divide(n : int, d : int) : div bool
  if d * d > n then True
  elif n % d == 0 then False
  else trial-divide(n, d + 2)

// Factorise a positive integer into (prime, exponent) pairs, ascending.
//
// factorise(Positive(360)) == [(2,3), (3,2), (5,1)]
pub fun factorise(p : positive) : list<(int, int)>
  val n = p.unwrap
  if n == 1 then []
  else pretend-no-div{ factor-with(n, 2) }

fun factor-with(n : int, d : int) : div list<(int, int)>
  if n <= 1 then []
  elif d * d > n then [(n, 1)]  // n is prime
  elif n % d == 0 then
    val (count, rest) = extract-factor(n, d, 0)
    Cons((d, count), factor-with(rest, next-candidate(d)))
  else factor-with(n, next-candidate(d))

// Extract all factors of d from n, returning (exponent, remainder).
fun extract-factor(n : int, d : int, acc : int) : div (int, int)
  if n % d == 0 then extract-factor(n / d, d, acc + 1)
  else (acc, n)

// Next candidate divisor: 2 -> 3, then odd numbers.
fun next-candidate(d : int) : int
  if d == 2 then 3 else d + 2

// Distinct prime factors of a positive integer, in ascending order.
pub fun prime-factors(p : positive) : list<int>
  factorise(p).map(fst)

// Generate primes up to a limit via trial division.
pub fun primes-upto(limit : int) : list<int>
  if limit < 2 then []
  else pretend-no-div{ Cons(2, sieve-odds(3, limit)) }

fun sieve-odds(d : int, limit : int) : div list<int>
  if d > limit then []
  elif is-prime(d) then Cons(d, sieve-odds(d + 2, limit))
  else sieve-odds(d + 2, limit)
