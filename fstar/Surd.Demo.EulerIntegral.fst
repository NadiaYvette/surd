/// Demo: Euler substitution for integrals involving square roots of quadratics.
///
/// Demonstrates symbolic computation of integrals of the form
/// integral R(x, sqrt(ax^2 + bx + c)) dx via Euler substitution.
module Surd.Demo.EulerIntegral

open Surd.Ring
open Surd.Rational
open Surd.Types
open Surd.Poly
open Surd.Pretty
open Surd.EulerIntegration

/// Example integrals to demonstrate.
noeq type integral_example = {
  ie_name: string;
  ie_quadratic: quadratic_form;
  ie_description: string;
}

/// Standard examples.
let example_integrals : list integral_example = [
  { ie_name = "integral dx/sqrt(x^2 + 1)";
    ie_quadratic = { qf_a = rat_one; qf_b = rat_zero; qf_c = rat_one };
    ie_description = "Euler I substitution: sqrt(x^2+1) = t - x" };
  { ie_name = "integral dx/sqrt(1 - x^2)";
    ie_quadratic = { qf_a = rat_neg rat_one; qf_b = rat_zero; qf_c = rat_one };
    ie_description = "Euler II substitution: sqrt(1-x^2) = tx + 1 (arcsin)" };
  { ie_name = "integral dx/sqrt(x^2 - 1)";
    ie_quadratic = { qf_a = rat_one; qf_b = rat_zero; qf_c = rat_neg rat_one };
    ie_description = "Euler I substitution: sqrt(x^2-1) = t - x" };
  { ie_name = "integral dx/sqrt(2x + 3)";
    ie_quadratic = { qf_a = rat_zero; qf_b = rat_of_int 2; qf_c = rat_of_int 3 };
    ie_description = "Linear case: direct substitution u = 2x+3" }
]

/// Compute and display the Euler substitution for an example.
val process_example : integral_example -> Dv string
let process_example ex =
  let result = compute_euler ex.ie_quadratic in
  let subst_name = match result.er_substitution with
    | EulerI _ -> "Euler I"
    | EulerII _ -> "Euler II"
    | EulerIII _ -> "Euler III"
  in
  ex.ie_name ^ "\n" ^
  "  Method: " ^ subst_name ^ "\n" ^
  "  " ^ ex.ie_description ^ "\n" ^
  "  Discriminant: " ^ Surd.Pretty.pretty_rat (qf_discriminant ex.ie_quadratic) ^ "\n"

/// Run all examples.
val run_demo : unit -> Dv string
let run_demo () =
  let header = "=== Euler Substitution Demo ===\n\n" in
  let rec go (exs: list integral_example) : Dv string =
    match exs with
    | [] -> ""
    | ex :: rest -> process_example ex ^ "\n" ^ go rest
  in
  header ^ go example_integrals
