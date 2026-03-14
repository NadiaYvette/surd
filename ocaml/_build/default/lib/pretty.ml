(** Pretty-printing radical expressions in human-readable mathematical notation.

    Includes common subexpression elimination (CSE): repeated subexpressions
    are named and shown as [let] bindings. *)

open Rad_expr
module R = Rational

(** {2 Precedence levels} *)

let prec_add = 1
let prec_mul = 2
let prec_neg = 3
let prec_pow = 4
let _prec_atom = 5

(** {2 Helpers} *)

let parens_if cond s = if cond then "(" ^ s ^ ")" else s

let pretty_rat r =
  let n = R.num r in
  let d = R.den r in
  if Z.equal d Z.one then Z.to_string n
  else if Z.sign n < 0 then
    Printf.sprintf "(-%s/%s)" (Z.to_string (Z.abs n)) (Z.to_string d)
  else
    Printf.sprintf "(%s/%s)" (Z.to_string n) (Z.to_string d)

let join_with sep = function
  | [] -> ""
  | [x] -> x
  | x :: xs -> x ^ String.concat "" (List.map (fun s -> sep ^ s) xs)

(** {2 Basic pretty-printing (no CSE)} *)

let rec flatten_add_basic = function
  | Add (a, b) -> flatten_add_basic a @ flatten_add_basic b
  | Neg e -> List.map (fun (s, t) -> (not s, t)) (flatten_add_basic e)
  | Lit r when R.is_neg r -> [(false, Lit (R.neg r))]
  | Mul (Neg a, b) -> [(false, Mul (a, b))]
  | e -> [(true, e)]

let flatten_mul_basic = function
  | Mul _ as e ->
    let rec go = function
      | Mul (a, b) -> go a @ go b
      | e -> [e]
    in go e
  | e -> [e]

let is_simple_basic = function
  | Lit _ | Root _ -> true
  | _ -> false

let pretty_radicand_basic prec_fn e =
  match e with
  | Lit r when R.is_pos r && R.is_integer r ->
    Z.to_string (R.num r)
  | Lit r when R.is_zero r -> "0"
  | _ ->
    if is_simple_basic e then prec_fn prec_pow e
    else "(" ^ prec_fn 0 e ^ ")"

(** Render a radical expression as a human-readable string. *)
let rec pretty_prec p = function
  | Lit r -> pretty_rat r
  | Neg e -> begin match e with
    | Add _ ->
      parens_if (p > prec_add)
        (render_terms_basic (List.map (fun (s, t) -> (not s, t))
          (flatten_add_basic e)))
    | Mul (Lit c, rest) ->
      pretty_prec p (Mul (Lit (R.neg c), rest))
    | Lit r ->
      pretty_prec p (Lit (R.neg r))
    | _ ->
      parens_if (p > prec_neg) ("-" ^ pretty_prec prec_neg e)
    end
  | Add _ as e ->
    parens_if (p > prec_add)
      (render_terms_basic (flatten_add_basic e))
  | Mul (a, Inv b) ->
    parens_if (p > prec_mul)
      (pretty_prec prec_mul a ^ "/" ^ pretty_prec prec_pow b)
  | Mul _ as e ->
    parens_if (p > prec_mul)
      (render_factors_basic (flatten_mul_basic e))
  | Inv e ->
    parens_if (p > prec_mul) ("1/" ^ pretty_prec prec_pow e)
  | Root (2, Lit r) when R.equal r R.minus_one -> "i"
  | Root (2, e) ->
    "\xe2\x88\x9a" ^ pretty_radicand_basic pretty_prec e  (* sqrt unicode *)
  | Root (3, e) ->
    "\xe2\x88\x9b" ^ pretty_radicand_basic pretty_prec e  (* cbrt unicode *)
  | Root (n, e) ->
    string_of_int n ^ "\xe2\x88\x9a" ^ pretty_radicand_basic pretty_prec e
  | Pow (_, 0) -> "1"
  | Pow (e, n) when n < 0 ->
    pretty_prec p (Inv (Pow (e, -n)))
  | Pow (e, 1) -> pretty_prec p e
  | Pow (e, n) ->
    parens_if (p > prec_pow)
      (pretty_prec prec_pow e ^ "^" ^ string_of_int n)

and render_terms_basic = function
  | [] -> "0"
  | (s, t) :: rest ->
    let hd = if s then pretty_prec prec_add t
             else "-" ^ pretty_prec prec_mul t in
    hd ^ String.concat "" (List.map (fun (sign, e) ->
      if sign then " + " ^ pretty_prec prec_add e
      else " - " ^ pretty_prec prec_mul e) rest)

and render_factors_basic = function
  | [] -> "1"
  | [x] -> pretty_prec prec_mul x
  | Lit c :: rest when R.is_one c ->
    join_with "\xc2\xb7" (List.map (pretty_prec prec_pow) rest)
  | Lit c :: rest when R.equal c R.minus_one ->
    "-" ^ join_with "\xc2\xb7" (List.map (pretty_prec prec_pow) rest)
  | Lit c :: rest ->
    pretty_rat c ^ "\xc2\xb7" ^ join_with "\xc2\xb7" (List.map (pretty_prec prec_pow) rest)
  | fs ->
    join_with "\xc2\xb7" (List.map (pretty_prec prec_pow) fs)

(** Render a radical expression as a human-readable string. *)
let pretty e = pretty_prec 0 e

(** {2 CSE pretty-printing} *)

module ExprMap = Map.Make(struct
  type t = R.t Rad_expr.t
  let compare = Rad_expr.compare R.compare
end)

(** Count subexpression occurrences. *)
let count_subs expr =
  let rec go m e =
    let m' = match ExprMap.find_opt e m with
      | Some n -> ExprMap.add e (n + 1) m
      | None -> ExprMap.add e 1 m
    in
    match e with
    | Lit _ -> m'
    | Neg a -> go m' a
    | Add (a, b) -> go (go m' a) b
    | Mul (a, b) -> go (go m' a) b
    | Inv a -> go m' a
    | Root (_, a) -> go m' a
    | Pow (a, _) -> go m' a
  in
  go ExprMap.empty expr

(** Is a subexpression complex enough to be worth naming? *)
let worth_naming = function
  | Lit _ -> false
  | Neg (Lit _) -> false
  | Root (_, Lit _) -> false
  | _ -> true

(** Variable name from index: a, b, ..., z, a1, b1, ... *)
let var_name i =
  let c = Char.chr (Char.code 'a' + i mod 26) in
  if i < 26 then String.make 1 c
  else String.make 1 c ^ string_of_int (i / 26)

(** Render with CSE: repeated subexpressions are shown as named intermediates. *)
let pretty_cse expr =
  let counts = count_subs expr in
  let shared = ExprMap.fold (fun k v acc ->
    if v >= 2 && worth_naming k then k :: acc else acc) counts [] in
  let sorted = List.sort (fun a b ->
    Int.compare (Rad_expr.size a) (Rad_expr.size b)) shared in
  let name_list = List.mapi (fun i _ -> var_name i) sorted in
  let name_map = List.fold_left2 (fun m sub name ->
    ExprMap.add sub name m) ExprMap.empty sorted name_list in
  let rec render_with names p e =
    match ExprMap.find_opt e names with
    | Some name -> parens_if (p > _prec_atom) name
    | None -> render_node names p e
  and render_node names p = function
    | Lit r -> pretty_rat r
    | Neg e -> parens_if (p > prec_neg) ("-" ^ render_with names prec_neg e)
    | Add _ as e ->
      let terms = flatten_add_basic e in
      parens_if (p > prec_add)
        (render_terms_with names terms)
    | Mul (a, Inv b) ->
      parens_if (p > prec_mul)
        (render_with names prec_mul a ^ "/" ^ render_with names prec_pow b)
    | Mul _ as e ->
      parens_if (p > prec_mul)
        (render_factors_with names (flatten_mul_basic e))
    | Inv e ->
      parens_if (p > prec_mul) ("1/" ^ render_with names prec_pow e)
    | Root (2, Lit r) when R.equal r R.minus_one -> "i"
    | Root (2, e) -> "\xe2\x88\x9a" ^ render_radicand names e
    | Root (3, e) -> "\xe2\x88\x9b" ^ render_radicand names e
    | Root (n, e) -> string_of_int n ^ "\xe2\x88\x9a" ^ render_radicand names e
    | Pow (_, 0) -> "1"
    | Pow (e, n) when n < 0 -> render_node names p (Inv (Pow (e, -n)))
    | Pow (e, 1) -> render_with names p e
    | Pow (e, n) ->
      parens_if (p > prec_pow)
        (render_with names prec_pow e ^ "^" ^ string_of_int n)
  and render_radicand names e =
    match e with
    | Lit r when R.is_pos r && R.is_integer r -> Z.to_string (R.num r)
    | Lit r when R.is_zero r -> "0"
    | _ ->
      let is_simple = match e with
        | Lit _ | Root _ -> true
        | _ -> ExprMap.mem e names
      in
      if is_simple then render_with names prec_pow e
      else "(" ^ render_with names 0 e ^ ")"
  and render_terms_with names = function
    | [] -> "0"
    | (s, t) :: rest ->
      let hd = if s then render_with names prec_add t
               else "-" ^ render_with names prec_mul t in
      hd ^ String.concat "" (List.map (fun (sign, e) ->
        if sign then " + " ^ render_with names prec_add e
        else " - " ^ render_with names prec_mul e) rest)
  and render_factors_with names = function
    | [] -> "1"
    | [x] -> render_with names prec_mul x
    | Lit c :: rest when R.is_one c ->
      join_with "\xc2\xb7" (List.map (render_with names prec_pow) rest)
    | Lit c :: rest when R.equal c R.minus_one ->
      "-" ^ join_with "\xc2\xb7" (List.map (render_with names prec_pow) rest)
    | Lit c :: rest ->
      pretty_rat c ^ "\xc2\xb7" ^ join_with "\xc2\xb7" (List.map (render_with names prec_pow) rest)
    | fs ->
      join_with "\xc2\xb7" (List.map (render_with names prec_pow) fs)
  in
  let bindings = List.map2 (fun sub name ->
    let avail = ExprMap.remove sub name_map in
    "  " ^ name ^ " = " ^ render_with avail 0 sub) sorted name_list in
  let body = render_with name_map 0 expr in
  match bindings with
  | [] -> body
  | _ -> "let\n" ^ String.concat "\n" bindings ^ "\nin " ^ body
