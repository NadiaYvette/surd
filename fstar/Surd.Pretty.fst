/// Text pretty-printing of radical expressions with precedence-based parenthesization.
module Surd.Pretty

open Surd.Ring
open Surd.Rational
open Surd.Types
open FStar.List.Tot

// ---------------------------------------------------------------------------
// Precedence levels
// ---------------------------------------------------------------------------

let prec_add : int = 1
let prec_mul : int = 2
let prec_neg : int = 3
let prec_pow : int = 4
let prec_atom : int = 5

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Wrap in parentheses if the condition holds.
let parens_if (b: bool) (s: string) : string =
  if b then "(" ^ s ^ ")" else s

/// Render a rational number.
let pretty_rat (r: rational) : string =
  if r.den = 1 then string_of_int r.num
  else if r.num < 0 then
    "(-" ^ string_of_int (abs_int r.num) ^ "/" ^ string_of_int r.den ^ ")"
  else
    "(" ^ string_of_int r.num ^ "/" ^ string_of_int r.den ^ ")"

/// Join strings with a separator.
let rec join_with (sep: string) (xs: list string) : Tot string (decreases xs) =
  match xs with
  | [] -> ""
  | [x] -> x
  | x :: rest -> x ^ sep ^ join_with sep rest

// ---------------------------------------------------------------------------
// Flatten Add/Mul for rendering
// ---------------------------------------------------------------------------

/// Flatten a Mul tree.
let rec flatten_mul_pp (e: rad_expr rational) : Tot (list (rad_expr rational)) (decreases e) =
  match e with
  | Mul a b -> flatten_mul_pp a @ flatten_mul_pp b
  | e -> [e]

/// Flatten an Add tree into (positive?, term) pairs for +/- rendering.
let rec flatten_add_pp (e: rad_expr rational)
  : Tot (list (bool & rad_expr rational)) (decreases e) =
  match e with
  | Add a b -> flatten_add_pp a @ flatten_add_pp b
  | Neg a -> map (fun ((s, t) : bool & rad_expr rational) -> (not s, t)) (flatten_add_pp a)
  | Lit r -> if rat_lt r rat_zero then [(false, Lit (rat_neg r))] else [(true, Lit r)]
  | Mul (Neg a) b -> [(false, Mul a b)]
  | e -> [(true, e)]

let neg_term (pair: bool & rad_expr rational) : (bool & rad_expr rational) =
  let (s, t) = pair in (not s, t)

// ---------------------------------------------------------------------------
// Main pretty-printer (Dv effect due to map over flattened sub-expressions)
// ---------------------------------------------------------------------------

/// Dv-compatible list map.
val map_dv : #a:Type -> #b:Type -> (a -> Dv b) -> list a -> Dv (list b)
let rec map_dv #a #b f xs =
  match xs with
  | [] -> []
  | x :: rest -> f x :: map_dv f rest

/// Render a radical expression as a human-readable string.
/// `p` is the current precedence context.
val pretty_prec : int -> rad_expr rational -> Dv string

/// Render a radicand.
val pretty_radicand : rad_expr rational -> Dv string

/// Render a list of (sign, term) pairs as addition/subtraction.
val render_terms : list (bool & rad_expr rational) -> Dv string

/// Render remaining terms in a sum.
val render_rest : list (bool & rad_expr rational) -> Dv string

/// Render a list of factors as multiplication.
val render_factors : list (rad_expr rational) -> Dv string

let rec pretty_prec p e =
  match e with
  | Lit r -> pretty_rat r
  | Neg e' ->
    (match e' with
     | Add _ _ ->
       parens_if (p > prec_add)
         (render_terms (map neg_term (flatten_add_pp e')))
     | Mul (Lit c) rest ->
       pretty_prec p (Mul (Lit (rat_neg c)) rest)
     | Lit r ->
       pretty_prec p (Lit (rat_neg r))
     | _ ->
       parens_if (p > prec_neg) ("-" ^ pretty_prec prec_neg e'))
  | Add _ _ ->
    parens_if (p > prec_add) (render_terms (flatten_add_pp e))
  | Mul a (Inv b) ->
    parens_if (p > prec_mul) (pretty_prec prec_mul a ^ "/" ^ pretty_prec prec_pow b)
  | Mul _ _ ->
    parens_if (p > prec_mul) (render_factors (flatten_mul_pp e))
  | Inv e' ->
    parens_if (p > prec_mul) ("1/" ^ pretty_prec prec_pow e')
  | Root 2 (Lit r) ->
    if rat_eq r (rat_neg rat_one) then "i"
    else "\xE2\x88\x9A" ^ pretty_radicand (Lit r)
  | Root 2 e' -> "\xE2\x88\x9A" ^ pretty_radicand e'
  | Root 3 e' -> "\xE2\x88\x9B" ^ pretty_radicand e'
  | Root n e' -> string_of_int n ^ "\xE2\x88\x9A" ^ pretty_radicand e'
  | Pow _ 0 -> "1"
  | Pow e' n ->
    if n < 0 then pretty_prec p (Inv (Pow e' (0 - n)))
    else if n = 1 then pretty_prec p e'
    else parens_if (p > prec_pow) (pretty_prec prec_pow e' ^ "^" ^ string_of_int n)

and pretty_radicand e =
  match e with
  | Lit r ->
    if rat_ge r rat_zero && r.den = 1 then string_of_int r.num
    else "(" ^ pretty_prec 0 e ^ ")"
  | Root _ _ -> pretty_prec prec_pow e
  | _ -> "(" ^ pretty_prec 0 e ^ ")"

and render_terms ts =
  match ts with
  | [] -> "0"
  | (s, t) :: rest ->
    let hd = if s then pretty_prec prec_add t else "-" ^ pretty_prec prec_mul t in
    hd ^ render_rest rest

and render_rest ts =
  match ts with
  | [] -> ""
  | (true, e) :: rest -> " + " ^ pretty_prec prec_add e ^ render_rest rest
  | (false, e) :: rest -> " - " ^ pretty_prec prec_mul e ^ render_rest rest

and render_factors fs =
  match fs with
  | [] -> "1"
  | [x] -> pretty_prec prec_mul x
  | Lit c :: rest ->
    if rat_eq c rat_one then
      join_with "\xC2\xB7" (map_dv (pretty_prec prec_pow) rest)
    else if rat_eq c (rat_neg rat_one) then
      "-" ^ join_with "\xC2\xB7" (map_dv (pretty_prec prec_pow) rest)
    else
      pretty_rat c ^ "\xC2\xB7" ^ join_with "\xC2\xB7" (map_dv (pretty_prec prec_pow) rest)
  | _ -> join_with "\xC2\xB7" (map_dv (pretty_prec prec_pow) fs)

/// Top-level pretty-printer.
val pretty : rad_expr rational -> Dv string
let pretty e = pretty_prec 0 e
