// Dense univariate polynomial arithmetic over an arbitrary coefficient ring/field.
//
// Polynomials are represented as coefficient lists, low-degree first:
// [a0, a1, ..., an] means a0 + a1*x + ... + an*x^n.
// Invariant: trailing zeros are stripped; the zero polynomial is [].
module surd/poly

import std/core/undiv
import surd/ring

// A univariate polynomial with coefficients in k.
pub struct poly<k>
  coeffs : list<k>

// Strip trailing zeros from a coefficient list.
fun strip-zeros(cs : list<k>, eq : (k, k) -> bool, z : k) : list<k>
  cs.reverse.drop-while(fn(c) eq(c, z)).reverse

// Smart constructor: strip trailing zeros.
pub fun mk-poly(cs : list<k>, r : ring<k>) : poly<k>
  Poly(strip-zeros(cs, r.ring-eq, r.ring-zero))

// The zero polynomial.
pub fun zero-poly() : poly<k>
  Poly([])

// Check if polynomial is zero.
pub fun is-zero(p : poly<k>) : bool
  p.coeffs.is-empty

// A constant polynomial.
pub fun const-poly(c : k, r : ring<k>) : poly<k>
  if (r.ring-eq)(c, r.ring-zero) then Poly([])
  else Poly([c])

// The polynomial x.
pub fun mono-x(r : ring<k>) : poly<k>
  Poly([r.ring-zero, r.ring-one])

// Degree of the polynomial. Returns -1 for the zero polynomial.
pub fun degree(p : poly<k>) : int
  match p.coeffs
    [] -> -1
    cs -> cs.length - 1

// Leading coefficient. Returns Nothing for the zero polynomial.
pub fun lead-coeff(p : poly<k>) : maybe<k>
  p.coeffs.last

// Evaluate a polynomial at a point using Horner's method.
pub fun eval-poly(p : poly<k>, x : k, r : ring<k>) : k
  match p.coeffs
    [] -> r.ring-zero
    cs -> cs.foldr(r.ring-zero, fn(c, acc) (r.ring-add)(c, (r.ring-mul)(x, acc)))

// Multiply by a scalar.
pub fun scale-poly(s : k, p : poly<k>, r : ring<k>) : poly<k>
  if (r.ring-eq)(s, r.ring-zero) then Poly([])
  else mk-poly(p.coeffs.map(fn(c) (r.ring-mul)(s, c)), r)

// Zip two lists with a function, using a default for the shorter list.
fun zip-with-default(def : a, f : (a, a) -> a, xs : list<a>, ys : list<a>) : div list<a>
  match xs
    Nil -> match ys
      Nil -> Nil
      Cons(b, bs) -> Cons(f(def, b), zip-with-default(def, f, Nil, bs))
    Cons(a, as1) -> match ys
      Nil -> Cons(f(a, def), zip-with-default(def, f, as1, Nil))
      Cons(b, bs) -> Cons(f(a, b), zip-with-default(def, f, as1, bs))

// Addition.
pub fun add-poly(a : poly<k>, b : poly<k>, r : ring<k>) : poly<k>
  pretend-no-div{ mk-poly(zip-with-default(r.ring-zero, r.ring-add, a.coeffs, b.coeffs), r) }

// Subtraction.
pub fun sub-poly(a : poly<k>, b : poly<k>, r : ring<k>) : poly<k>
  pretend-no-div{ mk-poly(zip-with-default(r.ring-zero, r.ring-sub, a.coeffs, b.coeffs), r) }

// Negation.
pub fun neg-poly(p : poly<k>, r : ring<k>) : poly<k>
  Poly(p.coeffs.map(r.ring-neg))

// Polynomial multiplication via schoolbook algorithm.
pub fun mul-poly(a : poly<k>, b : poly<k>, r : ring<k>) : poly<k>
  match (a.coeffs, b.coeffs)
    ([], _) -> Poly([])
    (_, []) -> Poly([])
    (as1, bs) ->
      val rlen = as1.length + bs.length - 1
      val init = replicate(r.ring-zero, rlen)
      // Accumulate products
      val result = mul-accumulate(as1, bs, init, r, 0)
      mk-poly(result, r)

// Helper to accumulate multiplication terms.
fun mul-accumulate(as1 : list<k>, bs : list<k>, acc : list<k>, r : ring<k>, i : int) : list<k>
  match as1
    [] -> acc
    Cons(a, rest) ->
      val acc' = mul-add-row(a, bs, acc, r, i, 0)
      mul-accumulate(rest, bs, acc', r, i + 1)

// Add a*b_j to acc[i+j] for each j.
fun mul-add-row(a : k, bs : list<k>, acc : list<k>, r : ring<k>, i : int, j : int) : list<k>
  match bs
    [] -> acc
    Cons(b, rest) ->
      val product = (r.ring-mul)(a, b)
      val acc' = list-update(acc, i + j, fn(v) (r.ring-add)(v, product))
      mul-add-row(a, rest, acc', r, i, j + 1)

// Update element at index in a list.
fun list-update(xs : list<a>, idx : int, f : (a) -> a) : list<a>
  match xs
    [] -> []
    Cons(x, rest) ->
      if idx == 0 then Cons(f(x), rest)
      else Cons(x, list-update(rest, idx - 1, f))

// Create a list of n copies of a value.
fun rep(x : a, n : int) : list<a>
  list/replicate(x, n)

// Polynomial division with remainder: div-mod-poly(f, g) = (q, r)
// such that f = g*q + r and degree(r) < degree(g).
// Requires the leading coefficient of g to be invertible (field).
pub fun div-mod-poly(f : poly<k>, g : poly<k>, fld : field<k>) : exn (poly<k>, poly<k>)
  val r = fld.field-ring
  match g.coeffs
    [] -> throw("div-mod-poly: division by zero polynomial")
    _ ->
      if f.degree < g.degree then (zero-poly(), f)
      else
        val dg = g.degree
        val lc = g.lead-coeff.unjust
        pretend-no-div{ div-mod-loop(zero-poly(), f, g, dg, lc, fld) }

fun div-mod-loop(q : poly<k>, rem : poly<k>, g : poly<k>, dg : int, lc : k, fld : field<k>) : <div,exn> (poly<k>, poly<k>)
  val r = fld.field-ring
  if rem.degree < dg then (q, rem)
  else
    val dr = rem.degree
    val lr = rem.lead-coeff.unjust
    val c = (fld.field-div)(lr, lc)
    val d = dr - dg
    // term = c * x^d
    val term-coeffs = replicate(r.ring-zero, d) ++ [c]
    val term = Poly(term-coeffs)
    val rem' = sub-poly(rem, mul-poly(term, g, r), r)
    val q' = add-poly(q, term, r)
    div-mod-loop(q', rem', g, dg, lc, fld)

// GCD of two polynomials via the Euclidean algorithm, made monic.
pub fun gcd-poly(a : poly<k>, b : poly<k>, fld : field<k>) : exn poly<k>
  pretend-no-div{ gcd-poly-rec(a, b, fld) }

fun gcd-poly-rec(a : poly<k>, b : poly<k>, fld : field<k>) : <div,exn> poly<k>
  match b.coeffs
    [] -> monic-poly(a, fld)
    _  ->
      val (_, rem) = div-mod-poly(a, b, fld)
      gcd-poly-rec(b, rem, fld)

// Make a polynomial monic (leading coefficient 1).
pub fun monic-poly(p : poly<k>, fld : field<k>) : exn poly<k>
  val r = fld.field-ring
  match p.lead-coeff
    Nothing -> zero-poly()
    Just(lc) -> Poly(p.coeffs.map(fn(c) (fld.field-div)(c, lc)))

// Formal derivative.
pub fun diff-poly(p : poly<k>, r : ring<k>) : poly<k>
  match p.coeffs
    [] -> Poly([])
    Cons(_, cs) ->
      val result = cs.map-indexed(fn(i, c) (r.ring-mul)((r.ring-from-int)(i + 1), c))
      mk-poly(result, r)

// Compose two polynomials: compose-poly(f, g) = f(g(x)).
pub fun compose-poly(f : poly<k>, g : poly<k>, r : ring<k>) : poly<k>
  match f.coeffs
    [] -> zero-poly()
    cs -> cs.foldr(zero-poly()) fn(c, acc)
      add-poly(const-poly(c, r), mul-poly(g, acc, r), r)

// Square-free factorisation via Yun's algorithm.
// Returns a list of (factor, multiplicity) pairs.
// Requires characteristic 0 (i.e., a field of rationals or extension thereof).
pub fun square-free(f : poly<k>, fld : field<k>) : exn list<(poly<k>, int)>
  val r = fld.field-ring
  match f.coeffs
    [] -> []
    _  ->
      val f' = diff-poly(f, r)
      val c = gcd-poly(f, f', fld)
      val w = div-mod-poly(f, c, fld).fst
      pretend-no-div{ sqfree-loop(w, c, 1, fld) }

fun sqfree-loop(w : poly<k>, c : poly<k>, i : int, fld : field<k>) : <div,exn> list<(poly<k>, int)>
  val r = fld.field-ring
  if w.degree == 0 then
    if c.degree > 0 then [(c, i)]
    else []
  else
    val y = gcd-poly(w, c, fld)
    val z = div-mod-poly(w, y, fld).fst
    val c' = div-mod-poly(c, y, fld).fst
    val rest = sqfree-loop(y, c', i + 1, fld)
    if z.degree > 0 then Cons((z, i), rest)
    else rest

// Show a polynomial using the ring's show function.
pub fun poly/show(p : poly<k>, r : ring<k>) : string
  match p.coeffs
    [] -> "0"
    cs ->
      val terms = cs.map-indexed(fn(i, c)
        if (r.ring-eq)(c, r.ring-zero) then ""
        elif i == 0 then (r.ring-show)(c)
        elif (r.ring-eq)(c, r.ring-one) then
          if i == 1 then "x" else "x^" ++ i.show
        else
          val cs1 = (r.ring-show)(c)
          if i == 1 then cs1 ++ "*x" else cs1 ++ "*x^" ++ i.show
      )
      val non-empty = terms.filter(fn(s) s != "")
      match non-empty
        [] -> "0"
        _  -> non-empty.join(" + ")

// Equality of polynomials (coefficients must match exactly).
pub fun poly/(==)(a : poly<k>, b : poly<k>, r : ring<k>) : bool
  pretend-no-div{ poly-eq-helper(a.coeffs, b.coeffs, r.ring-eq) }

fun poly-eq-helper(xs : list<k>, ys : list<k>, eq : (k, k) -> bool) : div bool
  match xs
    Nil -> ys.is-empty
    Cons(a, as1) -> match ys
      Nil -> False
      Cons(b, bs) -> eq(a, b) && poly-eq-helper(as1, bs, eq)
