/// Polynomial factoring over Q via rational root theorem.
module Surd.Factoring

open FStar.List.Tot
open Surd.Ring
open Surd.Rational
open Surd.Poly

/// Evaluate a polynomial at a rational point.
let eval_at (p: poly rational) (x: rational) : rational =
  eval_poly #rational #ring_rational p x

/// Generate all divisors (positive and negative) of an integer.
val all_divisors : int -> Dv (list int)
let all_divisors n =
  let n' = if n >= 0 then n else 0 - n in
  if n' = 0 then [0]
  else
    let rec go (d: pos) : Dv (list int) =
      if d > n' then []
      else if n' % d = 0 then d :: (0 - d) :: go (d + 1)
      else go (d + 1)
    in
    go 1

/// Find rational roots via rational root theorem.
/// If p(x) = a_n*x^n + ... + a_0 has rational root p/q in lowest terms,
/// then p | a_0 and q | a_n.
val rational_roots : poly rational -> Dv (list rational)
let rational_roots p =
  match p with
  | [] -> []
  | _ ->
    let a0 = match p with | c :: _ -> c | [] -> rat_zero in
    let an = match rev p with | c :: _ -> c | [] -> rat_one in
    if rat_eq a0 rat_zero then
      [rat_zero]  (* x = 0 is a root *)
    else
      let num_divs = all_divisors a0.num in
      let den_divs = all_divisors an.num in
      let rec try_roots (ns: list int) (ds: list int) : Dv (list rational) =
        match ns with
        | [] -> []
        | n :: nrest ->
          let rec try_dens (ds': list int) : Dv (list rational) =
            match ds' with
            | [] -> try_roots nrest ds
            | d :: drest ->
              if d = 0 then try_dens drest
              else
                let r = mk_rational n d in
                if rat_eq (eval_at p r) rat_zero then
                  r :: try_roots nrest ds  (* found one, skip rest of dens *)
                else try_dens drest
          in
          try_dens ds
      in
      try_roots num_divs den_divs

/// Factor a polynomial over Q via rational root theorem + recursion.
val factor_poly : poly rational -> Dv (list (poly rational))
let rec factor_poly p =
  let dp = degree p in
  if dp <= 0 then
    (match p with
     | [] -> []
     | _ -> [p])
  else if dp = 1 then [monic_poly #rational #field_rational p]
  else
    let roots = rational_roots p in
    match roots with
    | [] -> [p]
    | r :: _ ->
      let factor : poly rational = [rat_neg r; rat_one] in
      let (q, _) = div_mod_poly #rational #field_rational p factor in
      factor :: factor_poly q

/// Check if a polynomial is irreducible over Q.
val is_irreducible : poly rational -> Dv bool
let is_irreducible p =
  let dp = degree p in
  if dp <= 1 then true
  else
    let factors = factor_poly p in
    match factors with
    | [_] -> true
    | _ -> false

/// Factor out the content from a polynomial over Q.
/// Returns (content, primitive_part) where content * primitive_part = p.
val primitive_part : poly rational -> rational & poly rational
let primitive_part p =
  match p with
  | [] -> (rat_zero, [])
  | c :: _ ->
    (* Use first nonzero coefficient as initial content *)
    let init = rat_abs c in
    if rat_eq init rat_zero then (rat_one, p)
    else
      (* Content = gcd of all coefficients (as rationals) *)
      (* Simplified: just use leading coefficient for scaling *)
      let lc = match rev p with | [] -> rat_one | x :: _ -> x in
      let lc_abs = rat_abs lc in
      if rat_eq lc_abs rat_zero then (rat_one, p)
      else (lc_abs, map (fun c -> rat_div_total c lc_abs) p)
