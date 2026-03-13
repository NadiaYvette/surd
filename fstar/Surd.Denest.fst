/// Denesting dispatcher for radical expressions.
///
/// Attempts to simplify nested radicals using Borodin's sqrt denesting
/// and related techniques.
module Surd.Denest

open FStar.List.Tot
open Surd.Ring
open Surd.Rational
open Surd.Types
open Surd.Normalize
open Surd.Expr

/// Integer square root check: returns Some k if k^2 = n, None otherwise.
val isqrt_check : nat -> Dv (option nat)
let isqrt_check n =
  let rec go (k: nat) : Dv (option nat) =
    if op_Multiply k k = n then Some k
    else if op_Multiply k k > n then None
    else go (k + 1)
  in
  go 0

/// Try to denest sqrt(a + b*sqrt(c)) into sqrt(x) +/- sqrt(y) form.
///
/// If a^2 - b^2*c = d^2 (perfect square), then:
///   sqrt(a + b*sqrt(c)) = sqrt((a+d)/2) +/- sqrt((a-d)/2)
val try_sqrt_denest : rational -> rational -> rational -> Dv (option (rad_expr rational))
let try_sqrt_denest a b c =
  let disc = rat_sub (rat_mul a a) (rat_mul (rat_mul b b) c) in
  if rat_gt disc rat_zero then
    let disc_num = Surd.Rational.abs_int disc.num in
    let disc_den = disc.den in
    let sq_num = isqrt_check disc_num in
    let sq_den = isqrt_check disc_den in
    match sq_num, sq_den with
    | Some sn, Some sd ->
      if sd > 0 then
        let d = mk_rational sn sd in
        let half = mk_rational 1 2 in
        let x = rat_mul half (rat_add a d) in
        let y = rat_mul half (rat_sub a d) in
        if rat_gt x rat_zero && rat_ge y rat_zero then
          if rat_eq y rat_zero then Some (Root 2 (Lit x))
          else
            let sign_expr = if rat_gt b rat_zero
              then Add (Root 2 (Lit x)) (Root 2 (Lit y))
              else Add (Root 2 (Lit x)) (Neg (Root 2 (Lit y)))
            in
            Some sign_expr
        else None
      else None
    | _, _ -> None
  else None

/// Attempt to denest a radical expression.
val denest : rad_expr rational -> Dv (rad_expr rational)
let rec denest e =
  match e with
  | Root 2 (Add (Lit a) (Mul (Lit b) (Root 2 (Lit c)))) ->
    (match try_sqrt_denest a b c with
     | Some result -> result
     | None -> Root 2 (Add (Lit a) (Mul (Lit b) (Root 2 (Lit c)))))
  | Root 2 (Add (Lit a) (Root 2 (Lit c))) ->
    (match try_sqrt_denest a rat_one c with
     | Some result -> result
     | None -> Root 2 (Add (Lit a) (Root 2 (Lit c))))
  | Root n a -> Root n (denest a)
  | Neg a -> Neg (denest a)
  | Add a b -> Add (denest a) (denest b)
  | Mul a b -> Mul (denest a) (denest b)
  | Inv a -> Inv (denest a)
  | Pow a n -> Pow (denest a) n
  | e -> e

/// Denest and normalize in sequence.
val denest_and_normalize : rad_expr rational -> Dv (rad_expr rational)
let denest_and_normalize e =
  normalize (denest e)
