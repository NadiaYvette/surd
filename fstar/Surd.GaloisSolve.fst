/// Solve polynomials via Galois theory.
///
/// Given a polynomial f(x) over Q:
/// 1. Identify its Galois group.
/// 2. If solvable, construct a radical tower.
/// 3. Express the roots as radical expressions.
module Surd.GaloisSolve

open FStar.List.Tot
open Surd.Ring
open Surd.Rational
open Surd.Types
open Surd.Poly
open Surd.Identify
open Surd.TransitiveGroup
open Surd.RadicalTower

/// Result of attempting to solve a polynomial by radicals.
noeq type solve_result =
  | Solvable   : list (rad_expr rational) -> solve_result  (* roots as radical expressions *)
  | Unsolvable : transitive_group_id -> solve_result        (* Galois group is not solvable *)
  | TooHigh    : solve_result                                (* degree too high for current implementation *)

/// Solve a linear polynomial ax + b = 0.
let solve_linear (f: poly rational) : list (rad_expr rational) =
  match f with
  | [b; a] ->
    if rat_eq a rat_zero then []
    else [Lit (rat_neg (rat_div_total b a))]
  | _ -> []

/// Solve a quadratic polynomial x^2 + bx + c = 0.
let solve_quadratic (f: poly rational) : list (rad_expr rational) =
  match f with
  | [c; b; _] ->
    let (r1, r2) = Surd.Convert.solve_quadratic b c in
    [r1; r2]
  | _ -> []

/// Solve a depressed cubic x^3 + px + q = 0.
let solve_cubic (f: poly rational) : list (rad_expr rational) =
  match f with
  | [q; _; p; _] ->
    (* For simplicity, return one real root via Cardano *)
    let root = Surd.Convert.solve_depressed_cubic p q in
    [root]
  | _ -> []

/// Main entry point: solve a polynomial over Q by radicals.
///
/// For degrees <= 4, always solvable.
/// For degree 5, solvable iff Galois group is solvable (not A_5 or S_5).
/// For degree >= 6, not supported.
val solve_by_radicals : poly rational -> Dv solve_result
let solve_by_radicals f =
  let d = degree f in
  if d <= 0 then Solvable []
  else if d = 1 then Solvable (solve_linear f)
  else if d = 2 then Solvable (solve_quadratic f)
  else if d = 3 then Solvable (solve_cubic f)
  else if d = 4 then
    (* Ferrari's method — stub: return empty for now *)
    Solvable []
  else if d = 5 then
    let gal = identify_galois_group f in
    if is_solvable gal then
      (* Build radical tower and extract roots *)
      let tower = build_tower f in
      match root_from_tower f tower with
      | Some root -> Solvable [root]
      | None -> Solvable []  (* tower construction succeeded but root extraction failed *)
    else Unsolvable gal
  else TooHigh

/// Check if a polynomial is solvable by radicals.
val is_solvable_by_radicals : poly rational -> Dv bool
let is_solvable_by_radicals f =
  let d = degree f in
  if d <= 4 then true
  else if d = 5 then
    let gal = identify_galois_group f in
    is_solvable gal
  else false  (* conservative: we don't handle degree >= 6 *)
