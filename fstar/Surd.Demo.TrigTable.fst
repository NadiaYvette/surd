/// Demo: Generate a table of exact trigonometric values.
///
/// Computes cos(k*pi/n) and sin(k*pi/n) for standard angles and
/// displays them as both text and LaTeX radical expressions.
module Surd.Demo.TrigTable

open Surd.Ring
open Surd.Rational
open Surd.Types
open Surd.Trig
open Surd.Pretty
open Surd.LaTeX
open Surd.Normalize

/// Standard denominators for the trig table.
let standard_denominators : list int = [1; 2; 3; 4; 5; 6; 8; 10; 12]

/// Generate all angles k/n * pi for a given denominator n, k = 0..n.
let angles_for_denom (n: int) : list (int & int) =
  let rec go (k: nat) : Tot (list (int & int)) (decreases (n + 1 - k)) =
    if k > n then []
    else (k, n) :: go (k + 1)
  in
  if n <= 0 then []
  else go 0

/// A row in the trig table.
noeq type trig_row = {
  tr_angle_num: int;     (* numerator of angle/pi *)
  tr_angle_den: int;     (* denominator *)
  tr_cos_text: string;   (* cos value as text *)
  tr_sin_text: string;   (* sin value as text *)
  tr_cos_latex: string;  (* cos value as LaTeX *)
  tr_sin_latex: string;  (* sin value as LaTeX *)
}

/// Compute one row of the trig table.
val compute_row : int -> int -> Dv trig_row
let compute_row p q =
  assume (q > 0);
  let cos_e = normalize (cos_exact p q) in
  let sin_e = normalize (sin_exact p q) in
  { tr_angle_num = p;
    tr_angle_den = q;
    tr_cos_text = pretty cos_e;
    tr_sin_text = pretty sin_e;
    tr_cos_latex = latex cos_e;
    tr_sin_latex = latex sin_e }

/// Compute the full trig table.
val compute_table : unit -> Dv (list trig_row)
let compute_table () =
  let rec go_denoms (ds: list int) : Dv (list trig_row) =
    match ds with
    | [] -> []
    | n :: rest ->
      let angles = angles_for_denom n in
      let rec go_angles (as_: list (int & int)) : Dv (list trig_row) =
        match as_ with
        | [] -> []
        | (p, q) :: rest_a ->
          compute_row p q :: go_angles rest_a
      in
      go_angles angles @ go_denoms rest
  in
  go_denoms standard_denominators

/// Format the table as text.
val format_text_table : list trig_row -> Dv string
let format_text_table rows =
  let header = "  angle/pi  |  cos  |  sin\n" ^
               "------------+-------+------\n" in
  let rec go (rs: list trig_row) : Dv string =
    match rs with
    | [] -> ""
    | r :: rest ->
      let angle = if r.tr_angle_den = 1 then string_of_int r.tr_angle_num
                  else string_of_int r.tr_angle_num ^ "/" ^ string_of_int r.tr_angle_den in
      "  " ^ angle ^ "  |  " ^ r.tr_cos_text ^ "  |  " ^ r.tr_sin_text ^ "\n" ^
      go rest
  in
  header ^ go rows

/// Format the table as LaTeX.
val format_latex_table : list trig_row -> Dv string
let format_latex_table rows =
  let header = "\\begin{array}{c|c|c}\n" ^
               "\\theta/\\pi & \\cos\\theta & \\sin\\theta \\\\ \\hline\n" in
  let footer = "\\end{array}\n" in
  let rec go (rs: list trig_row) : Dv string =
    match rs with
    | [] -> ""
    | r :: rest ->
      let angle = if r.tr_angle_den = 1 then string_of_int r.tr_angle_num
                  else "\\frac{" ^ string_of_int r.tr_angle_num ^ "}{" ^
                       string_of_int r.tr_angle_den ^ "}" in
      angle ^ " & " ^ r.tr_cos_latex ^ " & " ^ r.tr_sin_latex ^ " \\\\\n" ^
      go rest
  in
  header ^ go rows ^ footer
