// LaTeX rendering of radical expressions.
module surd/latex

import std/core/undiv
import surd/rational
import surd/rad-expr

// Precedence levels.
val prec-add  = 1
val prec-mul  = 2
val prec-neg  = 3
val prec-pow  = 4

// Render a radical expression as a LaTeX math-mode string.
pub fun latex(e : rad-expr<rational>) : div string
  latex-prec(0, e)

// Render with explicit precedence context.
pub fun latex-prec(p : int, e : rad-expr<rational>) : div string
  match e
    Lit(r) -> latex-rat(r)
    Neg(x) -> match x
      // Neg of a sum: distribute sign into terms
      RAdd(_, _) ->
        parens-if(p > prec-add, render-terms(flatten-add(x).map(neg-term)))
      // Neg of product with literal coefficient: absorb sign
      RMul(Lit(c), rest) ->
        latex-prec(p, RMul(Lit(c.negate), rest))
      // Neg of literal: negate
      Lit(r) ->
        latex-prec(p, Lit(r.negate))
      // Otherwise: prefix minus
      _ ->
        parens-if(p > prec-neg, "-" ++ latex-prec(prec-neg, x))
    RAdd(_, _) ->
      parens-if(p > prec-add, render-terms(flatten-add(e)))
    // a / b  (with Inv on the right)
    RMul(a, Inv(b)) ->
      parens-if(p > prec-mul, "\\frac{" ++ latex-prec(0, a) ++ "}{" ++ latex-prec(0, b) ++ "}")
    // (1/a) * b  ->  b/a
    RMul(Inv(a), b) ->
      parens-if(p > prec-mul, "\\frac{" ++ latex-prec(0, b) ++ "}{" ++ latex-prec(0, a) ++ "}")
    Inv(x) ->
      parens-if(p > prec-mul, "\\frac{1}{" ++ latex-prec(0, x) ++ "}")
    Root(2, Lit(r)) ->
      if r == Rational(-1, 1) then "\\mathrm{i}"
      else "\\sqrt{" ++ latex-radicand(Lit(r)) ++ "}"
    Root(2, x) -> "\\sqrt{" ++ latex-radicand(x) ++ "}"
    Root(n, x) -> "\\sqrt[" ++ n.show ++ "]{" ++ latex-radicand(x) ++ "}"
    Pow(_, 0) -> "1"
    Pow(x, n) ->
      if n < 0 then latex-prec(p, Inv(Pow(x, 0 - n)))
      elif n == 1 then latex-prec(p, x)
      else parens-if(p > prec-pow, latex-base(x) ++ "^{" ++ n.show ++ "}")
    RMul(_, _) ->
      parens-if(p > prec-mul, render-factors(flatten-mul(e)))

// Render the base of a power expression.
// Roots and compound expressions need grouping for the exponent.
fun latex-base(e : rad-expr<rational>) : div string
  match e
    Root(_, _) -> "\\left(" ++ latex-prec(0, e) ++ "\\right)"
    RAdd(_, _) -> "\\left(" ++ latex-prec(0, e) ++ "\\right)"
    RMul(_, _) -> "\\left(" ++ latex-prec(0, e) ++ "\\right)"
    Neg(_)     -> "\\left(" ++ latex-prec(0, e) ++ "\\right)"
    Inv(_)     -> "\\left(" ++ latex-prec(0, e) ++ "\\right)"
    _          -> latex-prec(prec-pow, e)

// Render a radicand (inside \sqrt{...}).
// No outer parens needed since braces provide grouping.
fun latex-radicand(e : rad-expr<rational>) : div string
  match e
    Lit(r) ->
      if r.den == 1 then r.num.show
      elif r.num < 0 then "-\\frac{" ++ abs(r.num).show ++ "}{" ++ r.den.show ++ "}"
      else "\\frac{" ++ r.num.show ++ "}{" ++ r.den.show ++ "}"
    _ -> latex-prec(0, e)

// Render a rational number in LaTeX.
fun latex-rat(r : rational) : string
  if r.den == 1 then r.num.show
  elif r.num < 0 then "-\\frac{" ++ abs(r.num).show ++ "}{" ++ r.den.show ++ "}"
  else "\\frac{" ++ r.num.show ++ "}{" ++ r.den.show ++ "}"

// Conditional parenthesization with \left( \right).
fun parens-if(cond : bool, s : string) : string
  if cond then "\\left(" ++ s ++ "\\right)" else s

// Flatten an add tree into signed terms: list of (positive?, term).
fun flatten-add(e : rad-expr<rational>) : div list<(bool, rad-expr<rational>)>
  match e
    RAdd(a, b) -> flatten-add(a) ++ flatten-add(b)
    Neg(x) -> flatten-add(x).map(neg-term)
    Lit(r) ->
      if r.is-neg then [(False, Lit(r.negate))]
      else [(True, e)]
    RMul(Neg(a), b) -> [(False, RMul(a, b))]
    RMul(_, _) -> match flatten-mul(e)
      Cons(Lit(r), rest) ->
        if r.is-neg then [(False, rebuild-mul(Cons(Lit(r.negate), rest)))]
        else [(True, e)]
      _ -> [(True, e)]
    _ -> [(True, e)]

// Negate a signed term.
fun neg-term(t : (bool, rad-expr<rational>)) : (bool, rad-expr<rational>)
  (!t.fst, t.snd)

// Render a list of signed terms as a sum string.
fun render-terms(terms : list<(bool, rad-expr<rational>)>) : div string
  match terms
    [] -> "0"
    Cons((s, t), rest) ->
      val hd = if s then latex-prec(prec-add, t)
               else "-" ++ latex-prec(prec-mul, t)
      rest.foldl(hd) fn(acc, term)
        match term
          (True, x)  -> acc ++ " + " ++ latex-prec(prec-add, x)
          (False, x) -> acc ++ " - " ++ latex-prec(prec-mul, x)

// Flatten a product tree into a list of factors.
fun flatten-mul(e : rad-expr<rational>) : div list<rad-expr<rational>>
  match e
    RMul(a, b) -> flatten-mul(a) ++ flatten-mul(b)
    _          -> [e]

// Rebuild a product from a list of factors.
fun rebuild-mul(xs : list<rad-expr<rational>>) : rad-expr<rational>
  match xs
    []    -> Lit(surd/rational/one)
    [x]   -> x
    Cons(x, rest) -> rest.foldl(x, fn(a, b) RMul(a, b))

// Render a list of factors as a product string.
fun render-factors(fs : list<rad-expr<rational>>) : div string
  match fs
    []  -> "1"
    [x] -> latex-prec(prec-mul, x)
    Cons(Lit(c), rest) ->
      if c == surd/rational/one then join-mul(rest.map(fn(x) latex-prec(prec-pow, x)))
      elif c == Rational(-1, 1) then "-" ++ join-mul(rest.map(fn(x) latex-prec(prec-pow, x)))
      else latex-rat(c) ++ " \\cdot " ++ join-mul(rest.map(fn(x) latex-prec(prec-pow, x)))
    _ -> join-mul(fs.map(fn(x) latex-prec(prec-pow, x)))

// Join strings with \cdot separator.
fun join-mul(xs : list<string>) : string
  match xs
    []    -> ""
    [x]   -> x
    Cons(x, rest) -> rest.foldl(x, fn(a, b) a ++ " \\cdot " ++ b)
