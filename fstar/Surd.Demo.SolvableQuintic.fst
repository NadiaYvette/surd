/// Demo: Solvable quintic polynomials.
///
/// Demonstrates identification of Galois groups and radical solutions
/// for quintic polynomials. Shows examples of both solvable (F_20, D_5, C_5)
/// and unsolvable (S_5, A_5) quintics.
module Surd.Demo.SolvableQuintic

open FStar.List.Tot
open Surd.Ring
open Surd.Rational
open Surd.Types
open Surd.Poly
open Surd.Pretty
open Surd.GaloisSolve
open Surd.TransitiveGroup
open Surd.Identify

/// Example quintic polynomials.
noeq type quintic_example = {
  qe_name: string;
  qe_poly: poly rational;
  qe_description: string;
}

/// Format a Galois group identifier.
let format_group (g: transitive_group_id) : string =
  match g with
  | TG_Cn n -> "C_" ^ string_of_int n
  | TG_Dn n -> "D_" ^ string_of_int n
  | TG_Sn n -> "S_" ^ string_of_int n
  | TG_An n -> "A_" ^ string_of_int n
  | TG_F20 -> "F_20 (Frobenius)"
  | TG_V4 -> "V_4 (Klein)"
  | TG_Other n ord -> "T(" ^ string_of_int n ^ ", order " ^ string_of_int ord ^ ")"

/// Format a solve result.
val format_result : solve_result -> Dv string
let format_result sr =
  match sr with
  | Solvable roots ->
    let rec go (rs: list (rad_expr rational)) (i: nat) : Dv string =
      match rs with
      | [] -> ""
      | r :: rest ->
        "    x_" ^ string_of_int i ^ " = " ^ pretty r ^ "\n" ^ go rest (i + 1)
    in
    "  Solvable by radicals!\n" ^ go roots 0
  | Unsolvable g ->
    "  NOT solvable by radicals.\n" ^
    "  Galois group: " ^ format_group g ^ " (not solvable)\n"
  | TooHigh ->
    "  Degree too high for current implementation.\n"

/// Standard examples.
let example_quintics : list quintic_example = [
  (* x^5 - 5x + 12: Galois group S_5 (unsolvable) *)
  { qe_name = "x^5 - 5x + 12";
    qe_poly = mk_poly #rational #ring_rational
      [rat_of_int 12; rat_zero; rat_zero; rat_zero; rat_neg (rat_of_int 5); rat_one];
    qe_description = "Generic quintic (likely S_5)" };
  (* x^5 - 1: cyclotomic, Galois group C_4 (solvable) *)
  { qe_name = "x^5 - 1";
    qe_poly = mk_poly #rational #ring_rational
      [rat_neg rat_one; rat_zero; rat_zero; rat_zero; rat_zero; rat_one];
    qe_description = "Cyclotomic (roots of unity)" };
  (* x^5 - 2: Galois group F_20 (solvable) *)
  { qe_name = "x^5 - 2";
    qe_poly = mk_poly #rational #ring_rational
      [rat_neg (rat_of_int 2); rat_zero; rat_zero; rat_zero; rat_zero; rat_one];
    qe_description = "Pure radical extension" };
  (* x^5 - x - 1: Galois group S_5 (unsolvable) — the Bring-Jerrard quintic *)
  { qe_name = "x^5 - x - 1";
    qe_poly = mk_poly #rational #ring_rational
      [rat_neg rat_one; rat_neg rat_one; rat_zero; rat_zero; rat_zero; rat_one];
    qe_description = "Bring-Jerrard form" };
  (* x^5 + x^4 - 4x^3 - 3x^2 + 3x + 1: Galois group C_5 (solvable) *)
  { qe_name = "x^5 + x^4 - 4x^3 - 3x^2 + 3x + 1";
    qe_poly = mk_poly #rational #ring_rational
      [rat_one; rat_of_int 3; rat_neg (rat_of_int 3);
       rat_neg (rat_of_int 4); rat_one; rat_one];
    qe_description = "cos(2*pi/11) minimal polynomial" }
]

/// Process one example.
val process_example : quintic_example -> Dv string
let process_example ex =
  let galois = identify_galois_group ex.qe_poly in
  let result = solve_by_radicals ex.qe_poly in
  ex.qe_name ^ "\n" ^
  "  " ^ ex.qe_description ^ "\n" ^
  "  Galois group: " ^ format_group galois ^ "\n" ^
  "  Solvable: " ^ (if is_solvable galois then "yes" else "no") ^ "\n" ^
  format_result result

/// Run all examples.
val run_demo : unit -> Dv string
let run_demo () =
  let header = "=== Solvable Quintic Demo ===\n\n" in
  let rec go (exs: list quintic_example) : Dv string =
    match exs with
    | [] -> ""
    | ex :: rest -> process_example ex ^ "\n" ^ go rest
  in
  header ^ go example_quintics
