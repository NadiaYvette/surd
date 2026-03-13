/// LaTeX rendering of radical expressions.
module Surd.LaTeX

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

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Wrap in LaTeX parens if the condition holds.
let parens_if (b: bool) (s: string) : string =
  if b then "\\left(" ^ s ^ "\\right)" else s

/// Render a rational as LaTeX.
let latex_rat (r: rational) : string =
  if r.den = 1 then string_of_int r.num
  else if r.num < 0 then
    "-\\frac{" ^ string_of_int (abs_int r.num) ^ "}{" ^ string_of_int r.den ^ "}"
  else
    "\\frac{" ^ string_of_int r.num ^ "}{" ^ string_of_int r.den ^ "}"

/// Join strings with \cdot.
let rec join_mul (xs: list string) : Tot string (decreases xs) =
  match xs with
  | [] -> ""
  | [x] -> x
  | x :: rest -> x ^ " \\cdot " ^ join_mul rest

/// Dv-compatible list map.
val map_dv : #a:Type -> #b:Type -> (a -> Dv b) -> list a -> Dv (list b)
let rec map_dv #a #b f xs =
  match xs with
  | [] -> []
  | x :: rest -> f x :: map_dv f rest

// ---------------------------------------------------------------------------
// Flatten Add/Mul for rendering
// ---------------------------------------------------------------------------

let rec flatten_mul_lx (e: rad_expr rational) : Tot (list (rad_expr rational)) (decreases e) =
  match e with
  | Mul a b -> flatten_mul_lx a @ flatten_mul_lx b
  | e -> [e]

let rec flatten_add_lx (e: rad_expr rational)
  : Tot (list (bool & rad_expr rational)) (decreases e) =
  match e with
  | Add a b -> flatten_add_lx a @ flatten_add_lx b
  | Neg a -> map (fun ((s, t) : bool & rad_expr rational) -> (not s, t)) (flatten_add_lx a)
  | Lit r -> if rat_lt r rat_zero then [(false, Lit (rat_neg r))] else [(true, Lit r)]
  | Mul (Neg a) b -> [(false, Mul a b)]
  | e -> [(true, e)]

let neg_term_lx (pair: bool & rad_expr rational) : (bool & rad_expr rational) =
  let (s, t) = pair in (not s, t)

// ---------------------------------------------------------------------------
// Main LaTeX renderer (Dv effect due to map over flattened sub-expressions)
// ---------------------------------------------------------------------------

val latex_prec : int -> rad_expr rational -> Dv string
val latex_base : rad_expr rational -> Dv string
val latex_radicand : rad_expr rational -> Dv string
val render_terms_lx : list (bool & rad_expr rational) -> Dv string
val render_rest_lx : list (bool & rad_expr rational) -> Dv string
val render_factors_lx : list (rad_expr rational) -> Dv string

let rec latex_prec p e =
  match e with
  | Lit r -> latex_rat r
  | Neg e' ->
    (match e' with
     | Add _ _ ->
       parens_if (p > prec_add)
         (render_terms_lx (map neg_term_lx (flatten_add_lx e')))
     | Mul (Lit c) rest ->
       latex_prec p (Mul (Lit (rat_neg c)) rest)
     | Lit r ->
       latex_prec p (Lit (rat_neg r))
     | _ ->
       parens_if (p > prec_neg) ("-" ^ latex_prec prec_neg e'))
  | Add _ _ ->
    parens_if (p > prec_add) (render_terms_lx (flatten_add_lx e))
  | Mul a (Inv b) ->
    parens_if (p > prec_mul)
      ("\\frac{" ^ latex_prec 0 a ^ "}{" ^ latex_prec 0 b ^ "}")
  | Mul (Inv a) b ->
    parens_if (p > prec_mul)
      ("\\frac{" ^ latex_prec 0 b ^ "}{" ^ latex_prec 0 a ^ "}")
  | Inv e' ->
    parens_if (p > prec_mul) ("\\frac{1}{" ^ latex_prec 0 e' ^ "}")
  | Root 2 (Lit r) ->
    if rat_eq r (rat_neg rat_one) then "\\mathrm{i}"
    else "\\sqrt{" ^ latex_radicand (Lit r) ^ "}"
  | Root 2 e' -> "\\sqrt{" ^ latex_radicand e' ^ "}"
  | Root n e' -> "\\sqrt[" ^ string_of_int n ^ "]{" ^ latex_radicand e' ^ "}"
  | Pow _ 0 -> "1"
  | Pow e' n ->
    if n < 0 then latex_prec p (Inv (Pow e' (0 - n)))
    else if n = 1 then latex_prec p e'
    else parens_if (p > prec_pow) (latex_base e' ^ "^{" ^ string_of_int n ^ "}")
  | Mul _ _ ->
    parens_if (p > prec_mul) (render_factors_lx (flatten_mul_lx e))

and latex_base e =
  match e with
  | Root _ _ -> "\\left(" ^ latex_prec 0 e ^ "\\right)"
  | Add _ _ -> "\\left(" ^ latex_prec 0 e ^ "\\right)"
  | Mul _ _ -> "\\left(" ^ latex_prec 0 e ^ "\\right)"
  | Neg _ -> "\\left(" ^ latex_prec 0 e ^ "\\right)"
  | Inv _ -> "\\left(" ^ latex_prec 0 e ^ "\\right)"
  | _ -> latex_prec prec_pow e

and latex_radicand e =
  match e with
  | Lit r ->
    if r.den = 1 then string_of_int r.num
    else if r.num < 0 then "-\\frac{" ^ string_of_int (abs_int r.num) ^ "}{" ^ string_of_int r.den ^ "}"
    else "\\frac{" ^ string_of_int r.num ^ "}{" ^ string_of_int r.den ^ "}"
  | _ -> latex_prec 0 e

and render_terms_lx ts =
  match ts with
  | [] -> "0"
  | (s, t) :: rest ->
    let hd = if s then latex_prec prec_add t else "-" ^ latex_prec prec_mul t in
    hd ^ render_rest_lx rest

and render_rest_lx ts =
  match ts with
  | [] -> ""
  | (true, e) :: rest -> " + " ^ latex_prec prec_add e ^ render_rest_lx rest
  | (false, e) :: rest -> " - " ^ latex_prec prec_mul e ^ render_rest_lx rest

and render_factors_lx fs =
  match fs with
  | [] -> "1"
  | [x] -> latex_prec prec_mul x
  | Lit c :: rest ->
    if rat_eq c rat_one then
      join_mul (map_dv (latex_prec prec_pow) rest)
    else if rat_eq c (rat_neg rat_one) then
      "-" ^ join_mul (map_dv (latex_prec prec_pow) rest)
    else
      latex_rat c ^ " \\cdot " ^ join_mul (map_dv (latex_prec prec_pow) rest)
  | _ -> join_mul (map_dv (latex_prec prec_pow) fs)

/// Top-level LaTeX renderer.
val latex : rad_expr rational -> Dv string
let latex e = latex_prec 0 e
