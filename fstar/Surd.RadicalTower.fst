/// Radical tower construction for solvable polynomials.
module Surd.RadicalTower

open FStar.List.Tot
open Surd.Ring
open Surd.Rational
open Surd.Types
open Surd.Poly

/// A step in a radical tower: adjoin the nth root of some expression.
noeq type tower_step = {
  ts_index: int;
  ts_radicand: rad_expr rational;
}

/// A radical tower: sequence of adjunction steps.
type radical_tower = list tower_step

/// Build a radical tower for a quadratic x^2 + bx + c.
let quadratic_tower (b c: rational) : radical_tower =
  let disc = rat_sub (rat_mul b b) (rat_mul (rat_of_int 4) c) in
  [{ ts_index = 2; ts_radicand = Lit disc }]

/// Build a radical tower for a depressed cubic x^3 + px + q.
let cubic_tower (p q: rational) : radical_tower =
  let disc = rat_add (rat_mul (rat_of_int 4) (rat_mul (rat_mul p p) p))
                     (rat_mul (rat_of_int 27) (rat_mul q q)) in
  let disc_over_108 = rat_div_total disc (rat_of_int 108) in
  let step1 = { ts_index = 2; ts_radicand = Lit disc_over_108 } in
  let neg_q_half = rat_mul (mk_rational (0-1) 2) q in
  let step2 = { ts_index = 3;
                ts_radicand = Add (Lit neg_q_half) (Root 2 (Lit disc_over_108)) } in
  [step1; step2]

/// Build a radical tower for a polynomial of degree <= 4.
val build_tower : poly rational -> Dv radical_tower
let build_tower f =
  let d = degree f in
  if d <= 1 then []
  else if d = 2 then
    match f with
    | [c; b; _] -> quadratic_tower b c
    | _ -> []
  else if d = 3 then
    match f with
    | [q; _; p; _] ->
      if rat_eq p rat_zero then cubic_tower rat_zero q
      else cubic_tower p q
    | _ -> []
  else []

/// Express a root using the radical tower.
val root_from_tower : poly rational -> radical_tower -> Dv (option (rad_expr rational))
let root_from_tower f tower =
  let d = degree f in
  if d = 2 then
    match f with
    | [c; b; _] ->
      let disc_expr =
        match tower with
        | [step] -> Root 2 step.ts_radicand
        | _ -> Lit rat_zero
      in
      Some (Mul (Lit (mk_rational 1 2))
                (Add (Lit (rat_neg b)) disc_expr))
    | _ -> None
  else if d = 3 then
    match f with
    | [q; _; _; _] ->
      (match tower with
       | [_; step2] ->
         let u = Root 3 step2.ts_radicand in
         Some (Add u (Mul (Lit (rat_neg (mk_rational 1 3))) (Inv u)))
       | _ -> None)
    | _ -> None
  else None
