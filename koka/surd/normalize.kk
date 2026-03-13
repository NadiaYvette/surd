// Normalization passes for radical expressions over rationals.
//
// These are explicit transformations the user applies as needed.
// They do NOT denest -- see denesting modules for that.
module surd/normalize

import std/core/undiv
import surd/rational
import surd/rad-expr
import surd/positive
import surd/prime-factors

// Apply all normalization passes, iterated to a fixed point (up to 10 times).
pub fun normalize(e : rad-expr<rational>) : <div,exn> rad-expr<rational>
  fix-n(10, normalize-once, e)

// A single normalization pass (all sub-passes composed once).
pub fun normalize-once(e : rad-expr<rational>) : <div,exn> rad-expr<rational>
  e.flatten-arith.fold-constants.simplify-powers.extract-perfect-powers
    .sort-commutative.distribute.collect-coefficients.collect-terms

// Iterate a function until fixed point or fuel runs out.
fun fix-n(n : int, f : (rad-expr<rational>) -> <div,exn> rad-expr<rational>, x : rad-expr<rational>) : <div,exn> rad-expr<rational>
  if n <= 0 then x
  else
    val x' = f(x)
    if rad-eq(x', x, fn(a : rational, b : rational) a == b) then x
    else fix-n(n - 1, f, x')

// Flatten nested Add/Mul (associativity) and cancel double negations/inversions.
pub fun flatten-arith(e : rad-expr<k>) : div rad-expr<k>
  match e
    Neg(Neg(a))     -> flatten-arith(a)
    Neg(a)          -> Neg(flatten-arith(a))
    RAdd(a, b)      -> RAdd(flatten-arith(a), flatten-arith(b))
    RMul(a, b)      -> RMul(flatten-arith(a), flatten-arith(b))
    Inv(Inv(a))     -> flatten-arith(a)
    Inv(a)          -> Inv(flatten-arith(a))
    Root(n, a)      -> Root(n, flatten-arith(a))
    Pow(a, n)       -> Pow(flatten-arith(a), n)
    _               -> e

// Fold constant subexpressions: evaluate pure-literal subtrees.
pub fun fold-constants(e : rad-expr<rational>) : <div,exn> rad-expr<rational>
  match e
    Lit(r) -> Lit(r)
    Neg(a) ->
      val a' = fold-constants(a)
      match a'
        Lit(r) -> Lit(r.negate)
        Neg(x) -> x
        _      -> Neg(a')
    RAdd(a, b) ->
      val a' = fold-constants(a)
      val b' = fold-constants(b)
      match (a', b')
        (Lit(r), Lit(s))  -> Lit(r + s)
        (Lit(r), _)       -> if r.is-zero then b' else RAdd(a', b')
        (_, Lit(s))        -> if s.is-zero then a' else RAdd(a', b')
        (_, Neg(b2))       -> if rad-eq(a', b2, fn(x : rational, y : rational) x == y) then Lit(surd/rational/zero) else RAdd(a', Neg(b2))
        _                  -> RAdd(a', b')
    RMul(a, b) ->
      val a' = fold-constants(a)
      val b' = fold-constants(b)
      match (a', b')
        (Lit(r), Lit(s))  -> Lit(r * s)
        (Lit(r), _)       ->
          if r.is-zero then Lit(surd/rational/zero)
          elif r == surd/rational/one then b'
          elif r == Rational(-1, 1) then Neg(b')
          else RMul(a', b')
        (_, Lit(s))       ->
          if s.is-zero then Lit(surd/rational/zero)
          elif s == surd/rational/one then a'
          elif s == Rational(-1, 1) then Neg(a')
          else RMul(a', b')
        _                 -> RMul(a', b')
    Inv(a) ->
      val a' = fold-constants(a)
      match a'
        Lit(r) ->
          if r.is-zero then Inv(a')
          else Lit(surd/rational/one / r)
        Inv(x) -> x
        _      -> Inv(a')
    Root(n, a) ->
      val a' = fold-constants(a)
      match a'
        Lit(r) ->
          if r.is-zero then Lit(surd/rational/zero)
          elif r == surd/rational/one then Lit(surd/rational/one)
          else Root(n, a')
        _ -> Root(n, a')
    Pow(a, n) ->
      val a' = fold-constants(a)
      match a'
        Lit(r) -> Lit(rational/pow(r, n))
        _ ->
          if n == 0 then Lit(surd/rational/one)
          elif n == 1 then a'
          else Pow(a', n)

// Simplify power expressions.
pub fun simplify-powers(e : rad-expr<k>) : div rad-expr<k>
  match e
    // (sqrt(a))^2 = a via Mul pattern
    RMul(Root(2, a), Root(2, b)) ->
      if rad-eq-noeq(a, b) then simplify-powers(a)
      else RMul(simplify-powers(Root(2, a)), simplify-powers(Root(2, b)))
    Pow(Pow(a, m), n)   -> simplify-powers(Pow(a, m * n))
    Pow(Root(n, a), m)  ->
      if m == n then simplify-powers(a)
      else Pow(simplify-powers(Root(n, a)), m)
    Root(m, Root(n, a)) -> Root(m * n, simplify-powers(a))
    Root(n, Pow(a, m))  ->
      if m == n then simplify-powers(a)
      else Root(n, simplify-powers(Pow(a, m)))
    Neg(a)              -> Neg(simplify-powers(a))
    RAdd(a, b)          -> RAdd(simplify-powers(a), simplify-powers(b))
    RMul(a, b)          -> RMul(simplify-powers(a), simplify-powers(b))
    Inv(a)              -> Inv(simplify-powers(a))
    Root(n, a)          -> Root(n, simplify-powers(a))
    Pow(a, n)           -> Pow(simplify-powers(a), n)
    _                   -> e

// Structural equality without requiring eq on k (uses pointer-like identity).
// This is a fallback for simplify-powers where k is unconstrained.
fun rad-eq-noeq(a : rad-expr<k>, b : rad-expr<k>) : div bool
  match a
    Lit(_) -> match b
      Lit(_) -> False  // Can't compare literals without eq
      _      -> False
    Neg(x) -> match b
      Neg(y) -> rad-eq-noeq(x, y)
      _      -> False
    RAdd(a1, a2) -> match b
      RAdd(b1, b2) -> rad-eq-noeq(a1, b1) && rad-eq-noeq(a2, b2)
      _            -> False
    RMul(a1, a2) -> match b
      RMul(b1, b2) -> rad-eq-noeq(a1, b1) && rad-eq-noeq(a2, b2)
      _            -> False
    Inv(x) -> match b
      Inv(y) -> rad-eq-noeq(x, y)
      _      -> False
    Root(n1, x) -> match b
      Root(n2, y) -> n1 == n2 && rad-eq-noeq(x, y)
      _           -> False
    Pow(x, n1) -> match b
      Pow(y, n2) -> n1 == n2 && rad-eq-noeq(x, y)
      _          -> False

// Extract perfect nth powers from under radicals.
// Root(n, Lit(a^n * b)) -> Mul(Lit(a), Root(n, Lit(b)))
pub fun extract-perfect-powers(e : rad-expr<rational>) : div rad-expr<rational>
  match e
    Root(n, Lit(r)) ->
      if r.is-pos then
        val numr = r.num
        val denr = r.den
        val (num-out, num-in) = extract-nth-power(n, abs(numr))
        val (den-out, den-in) = extract-nth-power(n, abs(denr))
        // If den-in > 1, rationalize: multiply radicand by den-in^(n-1), divide outside by den-in
        val result = if den-in == 1 then
            val outer = Rational(num-out, den-out)
            val inner = Rational(num-in, 1)
            (outer, inner)
          else
            val new-inner-raw = num-in * pow-int(den-in, n - 1)
            val new-outer-base = Rational(num-out, den-out * den-in)
            // Re-extract from new-inner in case den-in^(n-1) introduced new powers
            val (num-out2, num-in2) = extract-nth-power(n, abs(new-inner-raw))
            val outer = Rational(num-out2, 1) * new-outer-base
            val inner = Rational(num-in2, 1)
            (outer, inner)
        val outer-coeff = result.fst
        val inner-rat   = result.snd
        if outer-coeff == surd/rational/one && inner-rat == surd/rational/one then Lit(surd/rational/one)
        elif outer-coeff == surd/rational/one then Root(n, Lit(inner-rat))
        elif inner-rat == surd/rational/one then Lit(outer-coeff)
        else RMul(Lit(outer-coeff), Root(n, Lit(inner-rat)))
      elif r.is-neg && is-odd(n) then
        Neg(extract-perfect-powers(Root(n, Lit(r.negate))))
      else Root(n, Lit(r))
    Root(n, a)  -> Root(n, extract-perfect-powers(a))
    Neg(a)      -> Neg(extract-perfect-powers(a))
    RAdd(a, b)  -> RAdd(extract-perfect-powers(a), extract-perfect-powers(b))
    RMul(a, b)  -> RMul(extract-perfect-powers(a), extract-perfect-powers(b))
    Inv(a)      -> Inv(extract-perfect-powers(a))
    Pow(a, n)   -> Pow(extract-perfect-powers(a), n)
    _           -> e

// Integer power helper.
fun pow-int(base : int, exp : int) : int
  pretend-no-div{ pow-int-rec(base, exp) }

fun pow-int-rec(base : int, exp : int) : div int
  if exp <= 0 then 1
  elif exp == 1 then base
  elif is-even(exp) then
    val half = pow-int-rec(base, exp / 2)
    half * half
  else base * pow-int-rec(base, exp - 1)

// Given n and a positive integer m, extract the largest kth power
// that divides m. Returns (extracted, remainder) where m = extracted^n * remainder.
fun extract-nth-power(n : int, m : int) : (int, int)
  match positive(m)
    Nothing -> (1, 1)
    Just(p) ->
      val fs = factorise(p)
      val extracted = fs.foldl(1, fn(acc, f) acc * pow-int(f.fst, f.snd / n))
      val remainder = fs.foldl(1, fn(acc, f) acc * pow-int(f.fst, f.snd % n))
      (extracted, remainder)

// Collect rational coefficients in products.
// Flattens a product tree, multiplies all Lit factors together,
// and produces Mul(Lit(coeff), body) or just body if coeff = 1.
pub fun collect-coefficients(e : rad-expr<rational>) : <div,exn> rad-expr<rational>
  match e
    RMul(_, _) ->
      val factors = flatten-mul(e)
      val parts = partition-lits(factors.map(fn(x) collect-coefficients(x)))
      val coeff = parts.fst.foldl(surd/rational/one, fn(a, b) a * b)
      val body  = build-mul(parts.snd)
      apply-coeff-mul(coeff, body)
    Neg(a) ->
      val a' = collect-coefficients(a)
      match a'
        Lit(r) -> Lit(r.negate)
        _      -> Neg(a')
    RAdd(a, b) -> RAdd(collect-coefficients(a), collect-coefficients(b))
    Inv(a)     -> Inv(collect-coefficients(a))
    Root(n, a) -> Root(n, collect-coefficients(a))
    Pow(a, n)  -> Pow(collect-coefficients(a), n)
    _          -> e

// Flatten a product tree into a list of factors.
fun flatten-mul(e : rad-expr<rational>) : div list<rad-expr<rational>>
  match e
    RMul(a, b) -> flatten-mul(a) ++ flatten-mul(b)
    _          -> [e]

// Partition a list of expressions into (literal-values, non-literals).
fun partition-lits(xs : list<rad-expr<rational>>) : <div,exn> (list<rational>, list<rad-expr<rational>>)
  match xs
    Nil -> ([], [])
    Cons(Lit(r), rest) ->
      val (ls, rs) = partition-lits(rest)
      (Cons(r, ls), rs)
    Cons(Inv(Lit(r)), rest) ->
      if r.is-zero then
        val (ls, rs) = partition-lits(rest)
        (ls, Cons(Inv(Lit(r)), rs))
      else
        val (ls, rs) = partition-lits(rest)
        (Cons(surd/rational/one / r, ls), rs)
    Cons(x, rest) ->
      val (ls, rs) = partition-lits(rest)
      (ls, Cons(x, rs))

// Build a product from a list of factors.
fun build-mul(xs : list<rad-expr<rational>>) : rad-expr<rational>
  match xs
    []    -> Lit(surd/rational/one)
    [x]   -> x
    Cons(x, rest) -> rest.foldl(x, fn(a, b) RMul(a, b))

// Apply a coefficient to a body expression.
fun apply-coeff-mul(c : rational, body : rad-expr<rational>) : rad-expr<rational>
  if c.is-zero then Lit(surd/rational/zero)
  elif c == surd/rational/one then body
  elif c == Rational(-1, 1) then Neg(body)
  else match body
    Lit(r) -> Lit(c * r)
    _      -> RMul(Lit(c), body)

// Collect like terms in sums.
// Flattens a sum tree, groups summands by their non-coefficient "base" part,
// adds up coefficients within each group, and rebuilds.
pub fun collect-terms(e : rad-expr<rational>) : div rad-expr<rational>
  match e
    RAdd(_, _) ->
      val terms = flatten-add(e)
      val processed = terms.map(fn(x) collect-terms(x))
      val grouped = group-terms(processed)
      val rebuilt = grouped.filter-map fn((base, c))
        if c.is-zero then Nothing
        else Just(apply-coeff-add(c, base))
      build-add(rebuilt)
    Neg(a)     -> Neg(collect-terms(a))
    RMul(a, b) -> RMul(collect-terms(a), collect-terms(b))
    Inv(a)     -> Inv(collect-terms(a))
    Root(n, a) -> Root(n, collect-terms(a))
    Pow(a, n)  -> Pow(collect-terms(a), n)
    _          -> e

// Flatten an add tree into a list of summands.
fun flatten-add(e : rad-expr<rational>) : div list<rad-expr<rational>>
  match e
    RAdd(a, b) -> flatten-add(a) ++ flatten-add(b)
    _          -> [e]

// Split a term into (coefficient, base).
fun split-coeff(e : rad-expr<rational>) : div (rational, rad-expr<rational>)
  match e
    RMul(Lit(c), body) -> (c, body)
    Neg(x) ->
      val (c, b) = split-coeff(x)
      (c.negate, b)
    Lit(r) -> (r, Lit(surd/rational/one))
    _      -> (surd/rational/one, e)

// Group terms by base, summing coefficients.
// Returns a list of (base, total-coefficient) pairs.
// Uses a simple association list for grouping.
fun group-terms(terms : list<rad-expr<rational>>) : div list<(rad-expr<rational>, rational)>
  val eq = fn(a : rational, b : rational) a == b
  terms.foldl([], fn(groups, term)
    val (c, base) = split-coeff(term)
    insert-or-add(groups, base, c, eq)
  )

// Insert or add to an association list keyed by structural equality.
fun insert-or-add(groups : list<(rad-expr<rational>, rational)>, base : rad-expr<rational>,
                  c : rational, eq : (rational, rational) -> bool) : div list<(rad-expr<rational>, rational)>
  match groups
    Nil -> [(base, c)]
    Cons((b, coeff), rest) ->
      if rad-eq(base, b, eq) then Cons((b, coeff + c), rest)
      else Cons((b, coeff), insert-or-add(rest, base, c, eq))

// Apply a coefficient to a base for add-term rebuilding.
fun apply-coeff-add(c : rational, base : rad-expr<rational>) : div rad-expr<rational>
  if c == surd/rational/one && rad-eq(base, Lit(surd/rational/one), fn(a : rational, b : rational) a == b) then Lit(surd/rational/one)
  elif rad-eq(base, Lit(surd/rational/one), fn(a : rational, b : rational) a == b) then Lit(c)
  elif c == surd/rational/one then base
  elif c == Rational(-1, 1) then Neg(base)
  else RMul(Lit(c), base)

// Build a sum from a list of summands.
fun build-add(xs : list<rad-expr<rational>>) : rad-expr<rational>
  match xs
    []    -> Lit(surd/rational/zero)
    [x]   -> x
    Cons(x, rest) -> rest.foldl(x, fn(a, b) RAdd(a, b))

// Sort children of commutative operators into canonical order.
pub fun sort-commutative(e : rad-expr<rational>) : div rad-expr<rational>
  match e
    RAdd(_, _) ->
      val terms = flatten-add(e).map(fn(x) sort-commutative(x))
      val sorted = merge-sort-exprs(terms)
      build-add(sorted)
    RMul(_, _) ->
      val factors = flatten-mul(e).map(fn(x) sort-commutative(x))
      val sorted = merge-sort-exprs(factors)
      build-mul(sorted)
    Neg(a)     -> Neg(sort-commutative(a))
    Inv(a)     -> Inv(sort-commutative(a))
    Root(n, a) -> Root(n, sort-commutative(a))
    Pow(a, n)  -> Pow(sort-commutative(a), n)
    _          -> e

// Compare two expressions for sorting purposes.
// Returns Lt, Eq, or Gt. Ordering: Lit < Neg < Root < Pow < RAdd < RMul < Inv.
fun expr-cmp(a : rad-expr<rational>, b : rad-expr<rational>) : div order
  match a
    Lit(r1) -> match b
      Lit(r2) -> rational/cmp(r1, r2)
      _       -> Lt
    Neg(x1) -> match b
      Lit(_)  -> Gt
      Neg(x2) -> expr-cmp(x1, x2)
      _       -> Lt
    Root(n1, x1) -> match b
      Lit(_)       -> Gt
      Neg(_)       -> Gt
      Root(n2, x2) -> match cmp(n1, n2)
        Eq -> expr-cmp(x1, x2)
        o  -> o
      _ -> Lt
    Pow(x1, n1) -> match b
      Lit(_)      -> Gt
      Neg(_)      -> Gt
      Root(_, _)  -> Gt
      Pow(x2, n2) -> match expr-cmp(x1, x2)
        Eq -> cmp(n1, n2)
        o  -> o
      _ -> Lt
    RAdd(a1, a2) -> match b
      Lit(_)      -> Gt
      Neg(_)      -> Gt
      Root(_, _)  -> Gt
      Pow(_, _)   -> Gt
      RAdd(b1, b2) -> match expr-cmp(a1, b1)
        Eq -> expr-cmp(a2, b2)
        o  -> o
      _ -> Lt
    RMul(a1, a2) -> match b
      Inv(_) -> Lt
      RMul(b1, b2) -> match expr-cmp(a1, b1)
        Eq -> expr-cmp(a2, b2)
        o  -> o
      _ -> Gt
    Inv(x1) -> match b
      Inv(x2) -> expr-cmp(x1, x2)
      _       -> Gt

// Merge sort for expression lists using expr-cmp.
fun merge-sort-exprs(xs : list<rad-expr<rational>>) : div list<rad-expr<rational>>
  match xs
    []    -> []
    [_]   -> xs
    _     ->
      val n = xs.length / 2
      val left  = xs.take(n)
      val right = xs.drop(n)
      merge-exprs(merge-sort-exprs(left), merge-sort-exprs(right))

fun merge-exprs(xs : list<rad-expr<rational>>, ys : list<rad-expr<rational>>) : div list<rad-expr<rational>>
  match xs
    Nil -> ys
    Cons(x, xs') -> match ys
      Nil -> xs
      Cons(y, ys') ->
        if expr-cmp(x, y).is-gt then Cons(y, merge-exprs(xs, ys'))
        else Cons(x, merge-exprs(xs', ys))

// Distribute scalar multiplication over addition.
pub fun distribute(e : rad-expr<rational>) : div rad-expr<rational>
  match e
    // Lit * (a + b + ...) -> Lit*a + Lit*b + ...
    RMul(Lit(c), r) ->
      if is-sum(r) then
        val terms = flatten-s(r)
        rebuild-add-ne(terms.map(fn(t) RMul(Lit(c), t)))
      else RMul(Lit(c), distribute(r))
    RMul(l, Lit(c)) ->
      if is-sum(l) then
        val terms = flatten-s(l)
        rebuild-add-ne(terms.map(fn(t) RMul(t, Lit(c))))
      else RMul(distribute(l), Lit(c))
    // (a + b) * x or x * (a + b) for tiny sums
    RMul(l, r) ->
      if is-tiny-sum(l) && !is-sum(r) then
        val terms = flatten-s(l)
        rebuild-add-ne(terms.map(fn(t) distribute(RMul(t, r))))
      elif !is-sum(l) && is-tiny-sum(r) then
        val terms = flatten-s(r)
        rebuild-add-ne(terms.map(fn(t) distribute(RMul(l, t))))
      else RMul(distribute(l), distribute(r))
    Neg(a)     -> Neg(distribute(a))
    RAdd(a, b) -> RAdd(distribute(a), distribute(b))
    Inv(a)     -> Inv(distribute(a))
    Root(n, a) -> Root(n, distribute(a))
    Pow(a, n)  -> Pow(distribute(a), n)
    _          -> e

// Is the expression a sum?
fun is-sum(e : rad-expr<rational>) : bool
  match e
    RAdd(_, _) -> True
    Neg(a)     -> is-sum(a)
    _          -> False

// Is the expression a sum with at most 2 terms?
fun is-tiny-sum(e : rad-expr<rational>) : div bool
  is-sum(e) && flatten-s(e).length <= 2

// Flatten a sum, distributing negation.
fun flatten-s(e : rad-expr<rational>) : div list<rad-expr<rational>>
  match e
    RAdd(a, b) -> flatten-s(a) ++ flatten-s(b)
    Neg(a)     -> flatten-s(a).map(fn(t) Neg(t))
    _          -> [e]

// Rebuild a sum from a non-empty list.
fun rebuild-add-ne(xs : list<rad-expr<rational>>) : rad-expr<rational>
  match xs
    []    -> Lit(surd/rational/zero)  // should not happen
    [x]   -> x
    Cons(x, rest) -> rest.foldl(x, fn(a, b) RAdd(a, b))
