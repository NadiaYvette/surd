/// Demo: Elliptic integral reduction.
///
/// Demonstrates reduction of integrals involving square roots of
/// cubics and quartics to standard Legendre elliptic integrals.
module Surd.Demo.EllipticIntegral

open Surd.Ring
open Surd.Rational
open Surd.Types
open Surd.Poly
open Surd.Pretty
open Surd.EllipticIntegration

/// Example elliptic integrals.
noeq type elliptic_example = {
  ee_name: string;
  ee_integrand: elliptic_integrand;
  ee_description: string;
}

/// Format an elliptic form for display.
let format_elliptic_form (ef: elliptic_form) : string =
  match ef with
  | EllipticK k -> "K(" ^ pretty_rat k ^ ")"
  | EllipticE k -> "E(" ^ pretty_rat k ^ ")"
  | EllipticPi n k -> "Pi(" ^ pretty_rat n ^ ", " ^ pretty_rat k ^ ")"
  | IncompleteK phi k -> "F(" ^ pretty_rat phi ^ ", " ^ pretty_rat k ^ ")"
  | IncompleteE phi k -> "E(" ^ pretty_rat phi ^ ", " ^ pretty_rat k ^ ")"

/// Standard examples.
let example_elliptic_integrals : list elliptic_example = [
  { ee_name = "integral dx/sqrt(x^3 - x)";
    ee_integrand = {
      ei_polynomial = mk_poly #rational #ring_rational
        [rat_zero; rat_neg rat_one; rat_zero; rat_one];  (* x^3 - x *)
      ei_rational_part = [rat_one]  (* 1 *)
    };
    ee_description = "Elliptic integral of first kind" };
  { ee_name = "integral dx/sqrt(4x^3 - x - 1)";
    ee_integrand = {
      ei_polynomial = mk_poly #rational #ring_rational
        [rat_neg rat_one; rat_neg rat_one; rat_zero; rat_of_int 4];  (* 4x^3 - x - 1 *)
      ei_rational_part = [rat_one]
    };
    ee_description = "Weierstrass form" };
  { ee_name = "integral sqrt(1 - x^4) dx";
    ee_integrand = {
      ei_polynomial = mk_poly #rational #ring_rational
        [rat_one; rat_zero; rat_zero; rat_zero; rat_neg rat_one];  (* 1 - x^4 *)
      ei_rational_part = [rat_one]
    };
    ee_description = "Lemniscatic integral (quartic)" }
]

/// Process one example.
val process_example : elliptic_example -> Dv string
let process_example ex =
  let classification = classify_integrand ex.ee_integrand in
  let result = reduce_to_standard ex.ee_integrand in
  let result_str = match result with
    | Some r ->
      let forms = FStar.List.Tot.map format_elliptic_form r.er_forms in
      let rec join (xs: list string) : Tot string (decreases xs) =
        match xs with
        | [] -> "(none)"
        | [x] -> x
        | x :: rest -> x ^ " + " ^ join rest
      in
      join forms
    | None -> "(reduction not available)"
  in
  ex.ee_name ^ "\n" ^
  "  Type: " ^ classification ^ "\n" ^
  "  " ^ ex.ee_description ^ "\n" ^
  "  Standard form: " ^ result_str ^ "\n"

/// Run all examples.
val run_demo : unit -> Dv string
let run_demo () =
  let header = "=== Elliptic Integral Demo ===\n\n" in
  let rec go (exs: list elliptic_example) : Dv string =
    match exs with
    | [] -> ""
    | ex :: rest -> process_example ex ^ "\n" ^ go rest
  in
  header ^ go example_elliptic_integrals
