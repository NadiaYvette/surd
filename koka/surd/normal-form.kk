// Normal form for radical expressions: Q-linear combinations of
// products of radical atoms.
//
// Every expression in Q[sqrt(2), sqrt(3), cbrt(5), ...] has a unique
// representation as a sum of (rational coefficient * monomial), where
// each monomial is a product of radical atoms raised to bounded powers.
//
// This representation gives automatic like-term collection, canonical
// ordering, and reliable structural equality.
module surd/normal-form

import std/core/undiv
import surd/rational
import surd/rad-expr
import surd/positive
import surd/prime-factors

// A radical atom: an irreducible nth root of a positive rational.
pub type atom
  // RatRoot(n, r): the principal nth root of r (r > 0, nth-power-free).
  RatRoot(root-index : int, radicand : rational)
  // sqrt(-1) = i. Kept separate since i^2 = -1 is the key reduction.
  ImagUnit
  // NestedRoot(n, e): nth root of e where e is a non-rational expression.
  NestedRoot(nest-index : int, nest-radicand : rad-expr<rational>)

// Compare atoms for ordering.
pub fun atom/cmp(a : atom, b : atom) : div order
  match a
    ImagUnit -> match b
      ImagUnit -> Eq
      _ -> Lt
    RatRoot(n1, r1) -> match b
      ImagUnit -> Gt
      RatRoot(n2, r2) ->
        match int/cmp(n1, n2)
          Eq -> rational/cmp(r1, r2)
          o -> o
      NestedRoot(_, _) -> Lt
    NestedRoot(n1, e1) -> match b
      NestedRoot(n2, e2) ->
        match int/cmp(n1, n2)
          Eq -> Eq  // structural comparison would need full rad-eq
          o -> o
      _ -> Gt

pub fun atom/(==)(a : atom, b : atom) : div bool
  atom/cmp(a, b).is-eq

// A monomial: product of atoms raised to positive powers.
// Represented as an association list sorted by atom.
pub struct monomial
  mono-atoms : list<(atom, int)>

pub val mono-one : monomial = Monomial([])

pub fun monomial/(==)(a : monomial, b : monomial) : div bool
  match (a.mono-atoms, b.mono-atoms)
    ([], []) -> True
    (Cons((a1, e1), rest1), Cons((a2, e2), rest2)) ->
      atom/(==)(a1, a2) && e1 == e2 && monomial/(==)(Monomial(rest1), Monomial(rest2))
    _ -> False

pub fun monomial/cmp(a : monomial, b : monomial) : div order
  cmp-atom-lists(a.mono-atoms, b.mono-atoms)

fun cmp-atom-lists(xs : list<(atom, int)>, ys : list<(atom, int)>) : div order
  match (xs, ys)
    ([], []) -> Eq
    ([], _) -> Lt
    (_, []) -> Gt
    (Cons((a1, e1), r1), Cons((a2, e2), r2)) ->
      match atom/cmp(a1, a2)
        Eq -> match int/cmp(e1, e2)
          Eq -> cmp-atom-lists(r1, r2)
          o -> o
        o -> o

// A normalized expression: Q-linear combination of monomials.
// Association list from monomials to rational coefficients (no zero coefficients).
pub struct norm-expr
  norm-terms : list<(monomial, rational)>

pub val norm-zero : norm-expr = Norm-expr([])

// Construct from a single rational.
pub fun norm-lit(r : rational) : norm-expr
  if r.is-zero then norm-zero
  else Norm-expr([(mono-one, r)])

// Construct from a single atom with exponent 1.
pub fun norm-atom(a : atom) : norm-expr
  Norm-expr([(Monomial([(a, 1)]), surd/rational/one)])

// Addition.
pub fun norm-add(a : norm-expr, b : norm-expr) : div norm-expr
  val merged = merge-terms(a.norm-terms, b.norm-terms)
  Norm-expr(merged.filter(fn(p) !p.snd.is-zero))

fun merge-terms(xs : list<(monomial, rational)>, ys : list<(monomial, rational)>) : div list<(monomial, rational)>
  match xs
    [] -> ys
    Cons((m, c), rest) ->
      val ys' = add-to-terms(ys, m, c)
      merge-terms(rest, ys')

fun add-to-terms(ts : list<(monomial, rational)>, m : monomial, c : rational) : div list<(monomial, rational)>
  match ts
    [] -> [(m, c)]
    Cons((m2, c2), rest) ->
      if monomial/(==)(m, m2) then Cons((m2, c2 + c), rest)
      else Cons((m2, c2), add-to-terms(rest, m, c))

// Negation.
pub fun norm-neg(a : norm-expr) : norm-expr
  Norm-expr(a.norm-terms.map(fn(p) (p.fst, p.snd.negate)))

// Subtraction.
pub fun norm-sub(a : norm-expr, b : norm-expr) : div norm-expr
  norm-add(a, norm-neg(b))

// Scale by a rational.
pub fun norm-scale(c : rational, a : norm-expr) : norm-expr
  if c.is-zero then norm-zero
  else Norm-expr(a.norm-terms.map(fn(p) (p.fst, p.snd * c)))

// Multiply two monomials.
fun mono-mul(a : monomial, b : monomial) : div monomial
  val merged = merge-mono-atoms(a.mono-atoms, b.mono-atoms)
  Monomial(merged.filter(fn(p) p.snd > 0))

fun merge-mono-atoms(xs : list<(atom, int)>, ys : list<(atom, int)>) : div list<(atom, int)>
  match xs
    [] -> ys
    Cons((a, e), rest) ->
      val ys' = add-atom-exp(ys, a, e)
      merge-mono-atoms(rest, ys')

fun add-atom-exp(ts : list<(atom, int)>, a : atom, e : int) : div list<(atom, int)>
  match ts
    [] -> [(a, e)]
    Cons((a2, e2), rest) ->
      if atom/(==)(a, a2) then
        val new-e = reduce-exp(a, e + e2)
        if new-e.fst == 0 then rest
        else Cons((a2, new-e.fst), rest)
      else Cons((a2, e2), add-atom-exp(rest, a, e))

// Reduce exponent modulo the root index (e.g., i^2 = -1, (sqrt(r))^2 = r).
// Returns (new-exponent, coefficient-factor).
fun reduce-exp(a : atom, e : int) : (int, rational)
  match a
    ImagUnit -> (e % 2, surd/rational/one)  // simplified
    RatRoot(n, _) -> (e % n, surd/rational/one)
    NestedRoot(n, _) -> (e % n, surd/rational/one)

// Multiplication.
pub fun norm-mul(a : norm-expr, b : norm-expr) : div norm-expr
  val terms = a.norm-terms.flatmap fn(pa)
    b.norm-terms.map fn(pb)
      val m = mono-mul(pa.fst, pb.fst)
      val c = pa.snd * pb.snd
      (m, c)
  // Collect like monomials
  val collected = terms.foldl([]) fn(acc : list<(monomial, rational)>, p)
    add-to-terms(acc, p.fst, p.snd)
  Norm-expr(collected.filter(fn(p) !p.snd.is-zero))

// Check if the expression is zero.
pub fun norm-is-zero(a : norm-expr) : bool
  a.norm-terms.is-empty

// Convert RadExpr to NormExpr.
pub fun to-norm-expr(e : rad-expr<rational>) : <div,exn> norm-expr
  match e
    Lit(r) -> norm-lit(r)
    Neg(a) -> norm-neg(to-norm-expr(a))
    RAdd(a, b) -> norm-add(to-norm-expr(a), to-norm-expr(b))
    RMul(a, b) -> norm-mul(to-norm-expr(a), to-norm-expr(b))
    Inv(a) ->
      val ne = to-norm-expr(a)
      // Simple case: single monomial with rational coefficient
      match ne.norm-terms
        [(m, c)] ->
          if m.mono-atoms.is-empty then norm-lit(surd/rational/one / c)
          else
            // Invert monomial by negating exponents (approximate)
            val inv-atoms = m.mono-atoms.map(fn(p) (p.fst, 0 - p.snd))
            Norm-expr([(Monomial(inv-atoms), surd/rational/one / c)])
        _ ->
          // Multi-term: create NestedRoot as fallback
          Norm-expr([(Monomial([(NestedRoot(1, Inv(e)), 1)]), surd/rational/one)])
    Root(n, a) ->
      val inner = to-norm-expr(a)
      match inner.norm-terms
        [(m, c)] ->
          if m.mono-atoms.is-empty then
            // Root of a rational: create RatRoot atom
            if c.is-zero then norm-zero
            elif c.is-neg then
              // Factor out i for even roots
              if is-even(n) then
                val i-term = norm-atom(ImagUnit)
                val pos-root = mk-rat-root-atom(n, c.negate)
                norm-mul(i-term, pos-root)
              else
                // Odd root of negative: negate
                norm-neg(mk-rat-root-atom(n, c.negate))
            else mk-rat-root-atom(n, c)
          else
            // Root of a non-rational expression
            Norm-expr([(Monomial([(NestedRoot(n, from-norm-expr(inner)), 1)]), surd/rational/one)])
        _ ->
          Norm-expr([(Monomial([(NestedRoot(n, from-norm-expr(inner)), 1)]), surd/rational/one)])
    Pow(a, n) ->
      val base = to-norm-expr(a)
      if n == 0 then norm-lit(surd/rational/one)
      elif n == 1 then base
      elif n < 0 then to-norm-expr(Inv(Pow(a, 0 - n)))
      else pretend-no-div{ norm-pow-rec(base, n) }

fun norm-pow-rec(base : norm-expr, n : int) : div norm-expr
  if n <= 1 then base
  elif is-even(n) then
    val half = norm-pow-rec(base, n / 2)
    norm-mul(half, half)
  else norm-mul(base, norm-pow-rec(base, n - 1))

// Create a RatRoot atom from a positive rational, extracting perfect powers.
fun mk-rat-root-atom(n : int, r : rational) : norm-expr
  if r == surd/rational/one then norm-lit(surd/rational/one)
  else
    // Extract perfect nth powers from numerator and denominator
    val num-abs = abs(r.num)
    val den-abs = r.den
    val (num-out, num-in) = extract-nth-power-int(n, num-abs)
    val (den-out, den-in) = extract-nth-power-int(n, den-abs)
    val outer = Rational(num-out, den-out)
    val inner = Rational(num-in, den-in)
    if inner == surd/rational/one then norm-lit(outer)
    else norm-scale(outer, norm-atom(RatRoot(n, inner)))

fun extract-nth-power-int(n : int, m : int) : (int, int)
  if m <= 1 then (1, m)
  else
    match positive(m)
      Nothing -> (1, 1)
      Just(p) ->
        val fs = factorise(p)
        val extracted = fs.foldl(1, fn(acc, f) acc * pow-int-local(f.fst, f.snd / n))
        val remainder = fs.foldl(1, fn(acc, f) acc * pow-int-local(f.fst, f.snd % n))
        (extracted, remainder)

fun pow-int-local(base : int, exp : int) : int
  pretend-no-div{ pow-int-local-rec(base, exp) }

fun pow-int-local-rec(base : int, exp : int) : div int
  if exp <= 0 then 1
  elif exp == 1 then base
  elif is-even(exp) then
    val half = pow-int-local-rec(base, exp / 2)
    half * half
  else base * pow-int-local-rec(base, exp - 1)

// Convert NormExpr back to RadExpr.
pub fun from-norm-expr(ne : norm-expr) : <div,exn> rad-expr<rational>
  match ne.norm-terms
    [] -> Lit(surd/rational/zero)
    terms ->
      val exprs = terms.map(fn(p) mono-to-expr(p.fst, p.snd))
      exprs.nf-foldl1(fn(a, b) RAdd(a, b))

fun nf-foldl1(xs : list<a>, f : (a, a) -> a) : exn a
  match xs
    [x] -> x
    Cons(x, rest) -> rest.foldl(x, f)
    _ -> throw("nf-foldl1: empty list")

fun mono-to-expr(m : monomial, c : rational) : <div,exn> rad-expr<rational>
  val atom-exprs = m.mono-atoms.map fn(p)
    val base = atom-to-expr(p.fst)
    if p.snd == 1 then base
    else Pow(base, p.snd)
  val body = match atom-exprs
    [] -> Lit(surd/rational/one)
    _ -> atom-exprs.nf-foldl1(fn(a, b) RMul(a, b))
  if c == surd/rational/one then body
  elif c == Rational(-1, 1) then Neg(body)
  elif m.mono-atoms.is-empty then Lit(c)
  else RMul(Lit(c), body)

fun atom-to-expr(a : atom) : rad-expr<rational>
  match a
    RatRoot(n, r) -> Root(n, Lit(r))
    ImagUnit -> Root(2, Lit(Rational(-1, 1)))
    NestedRoot(n, e) -> Root(n, e)
