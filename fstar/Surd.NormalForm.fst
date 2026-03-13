/// Monomial normal form for radical expressions.
///
/// Represents expressions as sums of monomials where each monomial is a
/// product of atoms raised to integer powers, with a rational coefficient.
module Surd.NormalForm

open FStar.List.Tot
open Surd.Ring
open Surd.Rational
open Surd.Types

/// An atom is an irreducible building block.
noeq type atom =
  | ARatRoot   : n:int{n >= 2} -> rational -> atom
  | AImagUnit  : atom
  | ANestedRoot : n:int{n >= 2} -> rad_expr rational -> atom

/// Structural equality on atoms.
let atom_eq (a b: atom) : bool =
  match a, b with
  | ARatRoot n1 r1, ARatRoot n2 r2 -> n1 = n2 && rat_eq r1 r2
  | AImagUnit, AImagUnit -> true
  | ANestedRoot n1 _, ANestedRoot n2 _ -> n1 = n2
  | _, _ -> false

/// Structural comparison on atoms for ordering.
let atom_cmp (a b: atom) : int =
  match a, b with
  | AImagUnit, AImagUnit -> 0
  | AImagUnit, _ -> 0 - 1
  | _, AImagUnit -> 1
  | ARatRoot n1 r1, ARatRoot n2 r2 ->
    if n1 < n2 then 0 - 1 else if n1 > n2 then 1
    else if rat_lt r1 r2 then 0 - 1 else if rat_eq r1 r2 then 0 else 1
  | ARatRoot _ _, ANestedRoot _ _ -> 0 - 1
  | ANestedRoot _ _, ARatRoot _ _ -> 1
  | ANestedRoot n1 _, ANestedRoot n2 _ ->
    if n1 < n2 then 0 - 1 else if n1 > n2 then 1 else 0

/// A monomial: product of atoms raised to integer powers.
type monomial = list (atom & int)

/// Equality on monomials.
let rec monomial_eq (a b: monomial) : Tot bool (decreases a) =
  match a, b with
  | [], [] -> true
  | (a1, e1) :: rest_a, (b1, e1') :: rest_b ->
    atom_eq a1 b1 && e1 = e1' && monomial_eq rest_a rest_b
  | _, _ -> false

/// A normal form expression: sum of (monomial, rational_coefficient) pairs.
type norm_expr = list (monomial & rational)

/// The zero normal form.
let nf_zero : norm_expr = []

/// The one normal form.
let nf_one : norm_expr = [([], rat_one)]

/// A literal rational in normal form.
let nf_lit (r: rational) : norm_expr =
  if rat_eq r rat_zero then nf_zero
  else [([], r)]

/// Multiply two monomials: merge atom lists, summing exponents.
let rec merge_atoms (a b: monomial) : Tot monomial (decreases a) =
  match a with
  | [] -> b
  | (atom_a, exp_a) :: rest_a ->
    let rec insert_atom (at: atom) (ex: int) (m: monomial) : Tot monomial (decreases m) =
      match m with
      | [] -> [(at, ex)]
      | (at', ex') :: rest ->
        if atom_eq at at' then
          let new_exp = ex + ex' in
          if new_exp = 0 then rest
          else (at', new_exp) :: rest
        else (at', ex') :: insert_atom at ex rest
    in
    merge_atoms rest_a (insert_atom atom_a exp_a b)

/// Insert or update a monomial in a norm_expr, summing coefficients.
let rec nf_insert (m: monomial) (c: rational) (nf: norm_expr) : Tot norm_expr (decreases nf) =
  match nf with
  | [] -> if rat_eq c rat_zero then [] else [(m, c)]
  | (m', c') :: rest ->
    if monomial_eq m m' then
      let new_c = rat_add c c' in
      if rat_eq new_c rat_zero then rest
      else (m, new_c) :: rest
    else (m', c') :: nf_insert m c rest

/// Multiply two normal forms.
let nf_mul (a b: norm_expr) : norm_expr =
  let rec go_a (xs: norm_expr) : Tot norm_expr (decreases xs) =
    match xs with
    | [] -> nf_zero
    | (ma, ca) :: rest_a ->
      let rec go_b (ys: norm_expr) (acc: norm_expr) : Tot norm_expr (decreases ys) =
        match ys with
        | [] -> acc
        | (mb, cb) :: rest_b ->
          go_b rest_b (nf_insert (merge_atoms ma mb) (rat_mul ca cb) acc)
      in
      let products = go_b b nf_zero in
      let rest_products = go_a rest_a in
      (* Merge products and rest_products *)
      fold_left (fun (acc: norm_expr) ((m, c) : monomial & rational) -> nf_insert m c acc)
        rest_products products
  in
  go_a a

/// Add two normal forms.
let nf_add (a b: norm_expr) : norm_expr =
  fold_left (fun (acc: norm_expr) ((m, c) : monomial & rational) -> nf_insert m c acc) b a

/// Negate a normal form.
let nf_neg (a: norm_expr) : norm_expr =
  map (fun ((m, c) : monomial & rational) -> (m, rat_neg c)) a

/// Subtract normal forms.
let nf_sub (a b: norm_expr) : norm_expr = nf_add a (nf_neg b)

/// Scale a normal form by a rational.
let nf_scale (r: rational) (a: norm_expr) : norm_expr =
  if rat_eq r rat_zero then nf_zero
  else map (fun ((m, c) : monomial & rational) -> (m, rat_mul r c)) a

/// Convert a rad_expr to normal form.
val to_norm_expr : rad_expr rational -> Dv norm_expr
let rec to_norm_expr e =
  match e with
  | Lit r -> nf_lit r
  | Neg a -> nf_neg (to_norm_expr a)
  | Add a b -> nf_add (to_norm_expr a) (to_norm_expr b)
  | Mul a b -> nf_mul (to_norm_expr a) (to_norm_expr b)
  | Inv _ -> [([], rat_one)]  (* stub *)
  | Root n (Lit r) ->
    if rat_eq r rat_zero then nf_zero
    else if rat_eq r (rat_neg rat_one) && n = 2 then
      [([(AImagUnit, 1)], rat_one)]
    else if rat_gt r rat_zero then
      [([(ARatRoot n r, 1)], rat_one)]
    else if n % 2 = 1 then
      nf_neg [([(ARatRoot n (rat_neg r), 1)], rat_one)]
    else
      [([(AImagUnit, 1); (ARatRoot n (rat_neg r), 1)], rat_one)]
  | Root n a ->
    [([(ANestedRoot n a, 1)], rat_one)]
  | Pow a n ->
    if n = 0 then nf_one
    else if n > 0 then
      let base = to_norm_expr a in
      let rec pow_nf (x: norm_expr) (k: nat) : Dv norm_expr =
        if k = 0 then nf_one
        else if k = 1 then x
        else
          let half = pow_nf x (k / 2) in
          let sq = nf_mul half half in
          if k % 2 = 0 then sq
          else nf_mul sq x
      in
      pow_nf base n
    else nf_one

/// Convert a normal form back to a rad_expr.
let from_norm_expr (nf: norm_expr) : rad_expr rational =
  let atom_to_expr (a: atom) (exp: int) : rad_expr rational =
    let base = match a with
      | ARatRoot n r -> Root n (Lit r)
      | AImagUnit -> Root 2 (Lit (rat_neg rat_one))
      | ANestedRoot n e -> Root n e
    in
    if exp = 1 then base
    else if exp = 0 then Lit rat_one
    else Pow base exp
  in
  let mono_to_expr ((m, c) : monomial & rational) : rad_expr rational =
    let factors = map (fun ((a, e) : atom & int) -> atom_to_expr a e) m in
    let body = match factors with
      | [] -> Lit rat_one
      | [x] -> x
      | x :: rest -> fold_left Mul x rest
    in
    if rat_eq c rat_one then body
    else if rat_eq c (rat_neg rat_one) then Neg body
    else Mul (Lit c) body
  in
  match nf with
  | [] -> Lit rat_zero
  | [term] -> mono_to_expr term
  | term :: rest ->
    fold_left (fun acc t -> Add acc (mono_to_expr t))
              (mono_to_expr term) rest

/// Round-trip through normal form for simplification.
val nf_simplify : rad_expr rational -> Dv (rad_expr rational)
let nf_simplify e = from_norm_expr (to_norm_expr e)
