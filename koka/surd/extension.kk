// Algebraic field extensions K(alpha), where alpha is a root of an
// irreducible polynomial over K.
//
// Elements are represented as polynomials in alpha modulo the minimal
// polynomial. Arithmetic is polynomial arithmetic mod the minimal polynomial.
//
// Since Koka has no typeclasses, we use dict-passing: an extension field
// is represented by a field<poly<k>> dictionary.
module surd/extension

import std/core/undiv
import std/core/unsafe
import surd/rational
import surd/ring
import surd/poly

// An extension field K(alpha) = K[x] / (minpoly(x)).
// Stores the minimal polynomial and the base field dictionary.
pub struct ext-field<k>
  ef-minpoly : poly<k>
  ef-base-field : field<k>

// Reduce a polynomial modulo the minimal polynomial.
pub fun ext-reduce(p : poly<k>, ef : ext-field<k>) : exn poly<k>
  if p.degree < ef.ef-minpoly.degree then p
  else
    val (_, rem) = div-mod-poly(p, ef.ef-minpoly, ef.ef-base-field)
    rem

// Reduce, but guaranteed safe (minpoly is nonzero).
// Uses unsafe-total to discard the exn effect.
fun ext-reduce-safe(p : poly<k>, ef : ext-field<k>) : poly<k>
  unsafe-total fn() ext-reduce(p, ef)

// Construct the ring dictionary for an extension field K(alpha).
pub fun ext-ring(ef : ext-field<k>) : ring<poly<k>>
  val base-r = ef.ef-base-field.field-ring
  Ring(
    ring-zero = zero-poly(),
    ring-one  = const-poly(base-r.ring-one, base-r),
    ring-add  = fn(a, b) add-poly(a, b, base-r),
    ring-sub  = fn(a, b) sub-poly(a, b, base-r),
    ring-mul  = fn(a, b) ext-reduce-safe(mul-poly(a, b, base-r), ef),
    ring-neg  = fn(a) neg-poly(a, base-r),
    ring-from-int = fn(n) const-poly((base-r.ring-from-int)(n), base-r),
    ring-eq   = fn(a, b) poly/(==)(a, b, base-r),
    ring-show = fn(a) poly/show(a, base-r)
  )

// Extended GCD for polynomials: returns (g, s, t) such that s*a + t*b = g.
pub fun ext-gcd-poly(a : poly<k>, b : poly<k>, fld : field<k>) : exn (poly<k>, poly<k>, poly<k>)
  val r = fld.field-ring
  pretend-no-div{ ext-gcd-rec(a, b, fld, r) }

fun ext-gcd-rec(a : poly<k>, b : poly<k>, fld : field<k>, r : ring<k>) : <div,exn> (poly<k>, poly<k>, poly<k>)
  if b.is-zero then
    val g = monic-poly(a, fld)
    match a.lead-coeff
      Nothing -> (g, const-poly(r.ring-one, r), zero-poly())
      Just(lc) ->
        val inv-lc = (fld.field-inv)(lc)
        (g, const-poly(inv-lc, r), zero-poly())
  else
    val (q, rem) = div-mod-poly(a, b, fld)
    val (g, s1, t1) = ext-gcd-rec(b, rem, fld, r)
    val s = t1
    val t = sub-poly(s1, mul-poly(t1, q, r), r)
    (g, s, t)

// Compute the multiplicative inverse in the extension field using
// extended GCD: find s such that s * a = 1 (mod minpoly).
pub fun ext-inv(a : poly<k>, ef : ext-field<k>) : exn poly<k>
  val r = ef.ef-base-field.field-ring
  if a.is-zero then throw("ext-inv: division by zero in extension field")
  else
    val (_, s, _) = ext-gcd-poly(a, ef.ef-minpoly, ef.ef-base-field)
    ext-reduce(s, ef)

// Construct the full field dictionary for an extension field.
pub fun ext-field-dict(ef : ext-field<k>) : field<poly<k>>
  val r = ext-ring(ef)
  Field(
    field-ring = r,
    field-div = fn(a, b) ext-reduce(mul-poly(a, ext-inv(b, ef), ef.ef-base-field.field-ring), ef),
    field-inv = fn(a) ext-inv(a, ef)
  )

// The generator alpha of the extension (= the polynomial x).
pub fun ext-alpha(ef : ext-field<k>) : poly<k>
  mono-x(ef.ef-base-field.field-ring)

// Embed a base field element into the extension.
pub fun ext-embed(c : k, ef : ext-field<k>) : poly<k>
  const-poly(c, ef.ef-base-field.field-ring)
