// Pretty-printing radical expressions in human-readable mathematical notation.
//
// Includes precedence-based rendering and CSE (common subexpression elimination).
module surd/pretty

import std/core/undiv
import surd/rational
import surd/rad-expr

// Precedence levels for parenthesization.
val prec-add  = 1
val prec-mul  = 2
val prec-neg  = 3
val prec-pow  = 4
val prec-atom = 5

// Render a radical expression as a human-readable string.
pub fun pretty(e : rad-expr<rational>) : div string
  pretty-prec(0, e)

// Render with explicit precedence context.
pub fun pretty-prec(p : int, e : rad-expr<rational>) : div string
  match e
    Lit(r)   -> pretty-rat(r)
    Neg(x)   -> match x
      // Neg of a sum: distribute sign into terms
      RAdd(_, _) ->
        parens-if(p > prec-add, render-terms-basic(flatten-add-basic(x).map(neg-term)))
      // Neg of product with literal coefficient: absorb sign
      RMul(Lit(c), rest) ->
        pretty-prec(p, RMul(Lit(c.negate), rest))
      // Neg of literal: negate
      Lit(r) ->
        pretty-prec(p, Lit(r.negate))
      // Otherwise: prefix minus
      _ ->
        parens-if(p > prec-neg, "-" ++ pretty-prec(prec-neg, x))
    RAdd(_, _) ->
      parens-if(p > prec-add, render-terms-basic(flatten-add-basic(e)))
    RMul(a, Inv(b)) ->
      parens-if(p > prec-mul, pretty-prec(prec-mul, a) ++ "/" ++ pretty-prec(prec-pow, b))
    RMul(_, _) ->
      parens-if(p > prec-mul, render-factors-basic(flatten-mul-basic(e)))
    Inv(x) ->
      parens-if(p > prec-mul, "1/" ++ pretty-prec(prec-pow, x))
    Root(2, Lit(r)) ->
      if r == Rational(-1, 1) then "i"
      else "\x221A" ++ pretty-radicand-basic(Lit(r))  // sqrt symbol
    Root(2, x) -> "\x221A" ++ pretty-radicand-basic(x)
    Root(3, x) -> "\x221B" ++ pretty-radicand-basic(x)  // cube root symbol
    Root(n, x) -> n.show ++ "\x221A" ++ pretty-radicand-basic(x)
    Pow(_, 0) -> "1"
    Pow(x, n) ->
      if n < 0 then pretty-prec(p, Inv(Pow(x, 0 - n)))
      elif n == 1 then pretty-prec(p, x)
      else parens-if(p > prec-pow, pretty-prec(prec-pow, x) ++ "^" ++ n.show)

// Render a rational number.
fun pretty-rat(r : rational) : string
  if r.den == 1 then r.num.show
  elif r.num < 0 then "(-" ++ abs(r.num).show ++ "/" ++ r.den.show ++ ")"
  else "(" ++ r.num.show ++ "/" ++ r.den.show ++ ")"

// Conditional parenthesization.
fun parens-if(cond : bool, s : string) : string
  if cond then "(" ++ s ++ ")" else s

// Render a radicand (the thing under the radical sign).
fun pretty-radicand-basic(e : rad-expr<rational>) : div string
  match e
    Lit(r) ->
      if r.num >= 0 && r.den == 1 then r.num.show
      else "(" ++ pretty(e) ++ ")"
    Root(_, _) -> pretty-prec(prec-pow, e)
    _ -> "(" ++ pretty(e) ++ ")"

// Flatten an add tree into signed terms: list of (positive?, term).
fun flatten-add-basic(e : rad-expr<rational>) : div list<(bool, rad-expr<rational>)>
  match e
    RAdd(a, b) -> flatten-add-basic(a) ++ flatten-add-basic(b)
    Neg(x) -> flatten-add-basic(x).map(neg-term)
    Lit(r) ->
      if r.is-neg then [(False, Lit(r.negate))]
      else [(True, e)]
    RMul(Neg(a), b) -> [(False, RMul(a, b))]
    RMul(_, _) -> match flatten-mul-basic(e)
      Cons(Lit(r), rest) ->
        if r.is-neg then [(False, rebuild-mul(Cons(Lit(r.negate), rest)))]
        else [(True, e)]
      _ -> [(True, e)]
    _ -> [(True, e)]

// Negate a signed term.
fun neg-term(t : (bool, rad-expr<rational>)) : (bool, rad-expr<rational>)
  (!t.fst, t.snd)

// Render a list of signed terms as a sum string.
fun render-terms-basic(terms : list<(bool, rad-expr<rational>)>) : div string
  match terms
    [] -> "0"
    Cons((s, t), rest) ->
      val hd = if s then pretty-prec(prec-add, t)
               else "-" ++ pretty-prec(prec-mul, t)
      rest.foldl(hd) fn(acc, term)
        match term
          (True, x)  -> acc ++ " + " ++ pretty-prec(prec-add, x)
          (False, x) -> acc ++ " - " ++ pretty-prec(prec-mul, x)

// Flatten a product tree into a list of factors.
fun flatten-mul-basic(e : rad-expr<rational>) : div list<rad-expr<rational>>
  match e
    RMul(a, b) -> flatten-mul-basic(a) ++ flatten-mul-basic(b)
    _          -> [e]

// Rebuild a product from a list of factors.
fun rebuild-mul(xs : list<rad-expr<rational>>) : rad-expr<rational>
  match xs
    []    -> Lit(surd/rational/one)
    [x]   -> x
    Cons(x, rest) -> rest.foldl(x, fn(a, b) RMul(a, b))

// Render a list of factors as a product string.
fun render-factors-basic(fs : list<rad-expr<rational>>) : div string
  match fs
    []  -> "1"
    [x] -> pretty-prec(prec-mul, x)
    Cons(Lit(c), rest) ->
      if c == surd/rational/one then join-with("\xB7", rest.map(fn(x) pretty-prec(prec-pow, x)))
      elif c == Rational(-1, 1) then "-" ++ join-with("\xB7", rest.map(fn(x) pretty-prec(prec-pow, x)))
      else pretty-rat(c) ++ "\xB7" ++ join-with("\xB7", rest.map(fn(x) pretty-prec(prec-pow, x)))
    _ -> join-with("\xB7", fs.map(fn(x) pretty-prec(prec-pow, x)))

// Join strings with a separator.
fun join-with(sep : string, xs : list<string>) : string
  match xs
    []    -> ""
    [x]   -> x
    Cons(x, rest) -> rest.foldl(x, fn(a, b) a ++ sep ++ b)

// ---------------------------------------------------------------
// CSE pretty-printing
// ---------------------------------------------------------------

// Render with CSE: repeated subexpressions are shown as named intermediates.
pub fun pretty-cse(e : rad-expr<rational>) : div string
  val counts = count-subs(e)
  // Keep subexpressions that appear 2+ times and are worth naming
  val shared = counts.filter(fn(p) p.snd >= 2 && worth-naming(p.fst)).map(fst)
  // Sort by size (smaller first)
  val sorted = sort-by-size(shared)
  // Assign names
  val name-list = sorted.map-indexed(fn(i, _) var-name(i))
  val name-map  = sorted.zip(name-list)
  // Build bindings
  val bindings = sorted.zip(name-list).map fn((sub, name))
    val avail = name-map.filter(fn(p) !rad-eq(p.fst, sub, fn(a : rational, b : rational) a == b))
    "  " ++ name ++ " = " ++ render-with(avail, 0, sub)
  val body = render-with(name-map, 0, e)
  match bindings
    [] -> body
    _  -> "let\n" ++ bindings.join("\n") ++ "\n" ++ "in " ++ body

// Generate variable names: a, b, ..., z, a1, b1, ...
fun var-name(i : int) : string
  val c = ('a'.int + (i % 26)).char
  if i < 26 then c.string
  else c.string ++ (i / 26).show

// Count occurrences of every subexpression.
fun count-subs(e : rad-expr<rational>) : div list<(rad-expr<rational>, int)>
  val counts = count-subs-acc([], e)
  counts

fun count-subs-acc(m : list<(rad-expr<rational>, int)>, e : rad-expr<rational>) : div list<(rad-expr<rational>, int)>
  val m' = inc-count(m, e)
  match e
    Lit(_) -> m'
    Neg(a) -> count-subs-acc(m', a)
    RAdd(a, b) -> count-subs-acc(count-subs-acc(m', a), b)
    RMul(a, b) -> count-subs-acc(count-subs-acc(m', a), b)
    Inv(a) -> count-subs-acc(m', a)
    Root(_, a) -> count-subs-acc(m', a)
    Pow(a, _) -> count-subs-acc(m', a)

// Increment count for an expression in the association list.
fun inc-count(m : list<(rad-expr<rational>, int)>, e : rad-expr<rational>) : div list<(rad-expr<rational>, int)>
  match m
    Nil -> [(e, 1)]
    Cons((k, v), rest) ->
      if rad-eq(k, e, fn(a : rational, b : rational) a == b) then Cons((k, v + 1), rest)
      else Cons((k, v), inc-count(rest, e))

// Is a subexpression complex enough to be worth naming?
fun worth-naming(e : rad-expr<rational>) : bool
  match e
    Lit(_)          -> False
    Neg(Lit(_))     -> False
    Root(_, Lit(_)) -> False
    _               -> True

// Sort subexpressions by size (number of nodes), smallest first.
fun sort-by-size(xs : list<rad-expr<rational>>) : div list<rad-expr<rational>>
  xs.sort-by(fn(a, b) cmp(node-count(a), node-count(b)))

// Sort a list by a comparison function (insertion sort, fine for small lists).
fun sort-by(xs : list<a>, cmpf : (a, a) -> div order) : div list<a>
  match xs
    Nil -> Nil
    Cons(x, rest) -> insert-sorted(x, sort-by(rest, cmpf), cmpf)

fun insert-sorted(x : a, xs : list<a>, cmpf : (a, a) -> div order) : div list<a>
  match xs
    Nil -> [x]
    Cons(y, rest) ->
      if cmpf(x, y).is-gt then Cons(y, insert-sorted(x, rest, cmpf))
      else Cons(x, xs)

// Render with CSE name substitution.
fun render-with(names : list<(rad-expr<rational>, string)>, p : int, e : rad-expr<rational>) : div string
  // Check if e has a name
  match lookup-name(names, e)
    Just(name) -> parens-if(p > prec-atom, name)
    Nothing -> render-with-inner(names, p, e)

fun lookup-name(names : list<(rad-expr<rational>, string)>, e : rad-expr<rational>) : div maybe<string>
  match names
    Nil -> Nothing
    Cons((k, v), rest) ->
      if rad-eq(k, e, fn(a : rational, b : rational) a == b) then Just(v)
      else lookup-name(rest, e)

fun render-with-inner(names : list<(rad-expr<rational>, string)>, p : int, e : rad-expr<rational>) : div string
  match e
    Lit(r)   -> pretty-rat(r)
    Neg(x)   -> parens-if(p > prec-neg, "-" ++ render-with(names, prec-neg, x))
    RAdd(_, _) ->
      val terms = flatten-add-basic(e)
      parens-if(p > prec-add, render-terms-with(names, terms))
    RMul(a, Inv(b)) ->
      parens-if(p > prec-mul, render-with(names, prec-mul, a) ++ "/" ++ render-with(names, prec-pow, b))
    RMul(_, _) ->
      val factors = flatten-mul-basic(e)
      parens-if(p > prec-mul, render-factors-with(names, factors))
    Inv(x)   -> parens-if(p > prec-mul, "1/" ++ render-with(names, prec-pow, x))
    Root(2, Lit(r)) ->
      if r == Rational(-1, 1) then "i"
      else "\x221A" ++ render-radicand-with(names, Lit(r))
    Root(2, x) -> "\x221A" ++ render-radicand-with(names, x)
    Root(3, x) -> "\x221B" ++ render-radicand-with(names, x)
    Root(n, x) -> n.show ++ "\x221A" ++ render-radicand-with(names, x)
    Pow(_, 0) -> "1"
    Pow(x, n) ->
      if n < 0 then render-with(names, p, Inv(Pow(x, 0 - n)))
      elif n == 1 then render-with(names, p, x)
      else parens-if(p > prec-pow, render-with(names, prec-pow, x) ++ "^" ++ n.show)

fun render-radicand-with(names : list<(rad-expr<rational>, string)>, e : rad-expr<rational>) : div string
  match e
    Lit(r) ->
      if r.num >= 0 && r.den == 1 then r.num.show
      else "(" ++ render-with(names, 0, e) ++ ")"
    Root(_, _) -> render-with(names, prec-pow, e)
    _ ->
      match lookup-name(names, e)
        Just(_) -> render-with(names, prec-pow, e)
        Nothing -> "(" ++ render-with(names, 0, e) ++ ")"

fun render-terms-with(names : list<(rad-expr<rational>, string)>, terms : list<(bool, rad-expr<rational>)>) : div string
  match terms
    [] -> "0"
    Cons((s, t), rest) ->
      val hd = if s then render-with(names, prec-add, t)
               else "-" ++ render-with(names, prec-mul, t)
      rest.foldl(hd) fn(acc, term)
        match term
          (True, x)  -> acc ++ " + " ++ render-with(names, prec-add, x)
          (False, x) -> acc ++ " - " ++ render-with(names, prec-mul, x)

fun render-factors-with(names : list<(rad-expr<rational>, string)>, fs : list<rad-expr<rational>>) : div string
  match fs
    []  -> "1"
    [x] -> render-with(names, prec-mul, x)
    Cons(Lit(c), rest) ->
      if c == surd/rational/one then join-with("\xB7", rest.map(fn(x) render-with(names, prec-pow, x)))
      elif c == Rational(-1, 1) then "-" ++ join-with("\xB7", rest.map(fn(x) render-with(names, prec-pow, x)))
      else pretty-rat(c) ++ "\xB7" ++ join-with("\xB7", rest.map(fn(x) render-with(names, prec-pow, x)))
    _ -> join-with("\xB7", fs.map(fn(x) render-with(names, prec-pow, x)))
