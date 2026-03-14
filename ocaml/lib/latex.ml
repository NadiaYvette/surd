(** LaTeX rendering of radical expressions. *)

open Rad_expr
module R = Rational

(** {2 Precedence levels} *)

let prec_add = 1
let prec_mul = 2
let prec_neg = 3
let prec_pow = 4

(** {2 Helpers} *)

let parens_if cond s =
  if cond then "\\left(" ^ s ^ "\\right)" else s

let latex_rat r =
  let n = R.num r in
  let d = R.den r in
  if Z.equal d Z.one then Z.to_string n
  else if Z.sign n < 0 then
    "-\\frac{" ^ Z.to_string (Z.abs n) ^ "}{" ^ Z.to_string d ^ "}"
  else
    "\\frac{" ^ Z.to_string n ^ "}{" ^ Z.to_string d ^ "}"

let join_mul = function
  | [] -> ""
  | [x] -> x
  | x :: xs -> x ^ String.concat "" (List.map (fun s -> " \\cdot " ^ s) xs)

(** {2 Flattening helpers} *)

let rec flatten_add = function
  | Add (a, b) -> flatten_add a @ flatten_add b
  | Neg e -> List.map (fun (s, t) -> (not s, t)) (flatten_add e)
  | Lit r when R.is_neg r -> [(false, Lit (R.neg r))]
  | Mul (Neg a, b) -> [(false, Mul (a, b))]
  | e -> [(true, e)]

let rec flatten_mul = function
  | Mul (a, b) -> flatten_mul a @ flatten_mul b
  | e -> [e]

(** {2 Rendering} *)

(** Render the base of a power expression with appropriate grouping. *)
let latex_base pp_fn = function
  | Root _ | Add _ | Mul _ | Neg _ | Inv _ as e ->
    "\\left(" ^ pp_fn 0 e ^ "\\right)"
  | e -> pp_fn prec_pow e

(** Render a radicand (inside \\sqrt{...}). *)
let latex_radicand pp_fn = function
  | Lit r ->
    let n = R.num r in
    let d = R.den r in
    if Z.equal d Z.one then Z.to_string n
    else if Z.sign n < 0 then
      "-\\frac{" ^ Z.to_string (Z.abs n) ^ "}{" ^ Z.to_string d ^ "}"
    else
      "\\frac{" ^ Z.to_string n ^ "}{" ^ Z.to_string d ^ "}"
  | e -> pp_fn 0 e

let render_terms pp_fn terms =
  let latex_mul_or_atom = pp_fn prec_mul in
  match terms with
  | [] -> "0"
  | (s, t) :: rest ->
    let hd = if s then pp_fn prec_add t else "-" ^ latex_mul_or_atom t in
    hd ^ String.concat "" (List.map (fun (sign, e) ->
      if sign then " + " ^ pp_fn prec_add e
      else " - " ^ latex_mul_or_atom e) rest)

let render_factors pp_fn = function
  | [] -> "1"
  | [x] -> pp_fn prec_mul x
  | Lit c :: rest when R.is_one c ->
    join_mul (List.map (pp_fn prec_pow) rest)
  | Lit c :: rest when R.equal c R.minus_one ->
    "-" ^ join_mul (List.map (pp_fn prec_pow) rest)
  | Lit c :: rest ->
    latex_rat c ^ " \\cdot " ^ join_mul (List.map (pp_fn prec_pow) rest)
  | fs ->
    join_mul (List.map (pp_fn prec_pow) fs)

(** Render a radical expression as a LaTeX math-mode string. *)
let rec latex_prec p = function
  | Lit r -> latex_rat r
  | Neg e -> begin match e with
    | Add _ ->
      parens_if (p > prec_add)
        (render_terms latex_prec
          (List.map (fun (s, t) -> (not s, t)) (flatten_add e)))
    | Mul (Lit c, rest) ->
      latex_prec p (Mul (Lit (R.neg c), rest))
    | Lit r ->
      latex_prec p (Lit (R.neg r))
    | _ ->
      parens_if (p > prec_neg) ("-" ^ latex_prec prec_neg e)
    end
  | Add _ as e ->
    parens_if (p > prec_add) (render_terms latex_prec (flatten_add e))
  | Mul (a, Inv b) ->
    parens_if (p > prec_mul)
      ("\\frac{" ^ latex_prec 0 a ^ "}{" ^ latex_prec 0 b ^ "}")
  | Mul (Inv a, b) ->
    parens_if (p > prec_mul)
      ("\\frac{" ^ latex_prec 0 b ^ "}{" ^ latex_prec 0 a ^ "}")
  | Inv e ->
    parens_if (p > prec_mul)
      ("\\frac{1}{" ^ latex_prec 0 e ^ "}")
  | Root (2, Lit r) when R.equal r R.minus_one -> "\\mathrm{i}"
  | Root (2, e) -> "\\sqrt{" ^ latex_radicand latex_prec e ^ "}"
  | Root (n, e) ->
    "\\sqrt[" ^ string_of_int n ^ "]{" ^ latex_radicand latex_prec e ^ "}"
  | Pow (_, 0) -> "1"
  | Pow (e, n) when n < 0 ->
    latex_prec p (Inv (Pow (e, -n)))
  | Pow (e, 1) -> latex_prec p e
  | Pow (e, n) ->
    parens_if (p > prec_pow)
      (latex_base latex_prec e ^ "^{" ^ string_of_int n ^ "}")
  | Mul _ as e ->
    parens_if (p > prec_mul) (render_factors latex_prec (flatten_mul e))

(** Render a radical expression as a LaTeX math-mode string. *)
let latex e = latex_prec 0 e
