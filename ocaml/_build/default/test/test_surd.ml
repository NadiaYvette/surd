(** Comprehensive test suite for the surd library. *)

module R = Surd.Rational
module E = Surd.Rad_expr
module P = Surd.Poly.RatPoly

let tests_passed = ref 0
let tests_failed = ref 0
let test_errors = ref []

let check name cond =
  if cond then begin
    incr tests_passed;
    Printf.printf "  PASS  %s\n%!" name
  end else begin
    incr tests_failed;
    test_errors := name :: !test_errors;
    Printf.printf "  FAIL  %s\n%!" name
  end

let check_float name expected actual eps =
  let diff = Float.abs (expected -. actual) in
  check name (diff < eps)

let section name =
  Printf.printf "\n=== %s ===\n%!" name

(* ------------------------------------------------------------------ *)
(* Rational                                                           *)
(* ------------------------------------------------------------------ *)

let test_rational () =
  section "Rational";
  check "zero" (R.is_zero R.zero);
  check "one" (R.is_one R.one);
  check "add" (R.equal (R.add (R.of_int 1) (R.of_int 2)) (R.of_int 3));
  check "sub" (R.equal (R.sub (R.of_int 5) (R.of_int 3)) (R.of_int 2));
  check "mul" (R.equal (R.mul (R.of_int 3) (R.of_int 4)) (R.of_int 12));
  check "div" (R.equal (R.div (R.of_int 6) (R.of_int 3)) (R.of_int 2));
  check "inv" (R.equal (R.inv (R.of_int 3)) (R.of_ints 1 3));
  check "neg" (R.equal (R.neg (R.of_int 3)) (R.of_int (-3)));
  check "normalisation" (R.equal (R.of_ints 6 4) (R.of_ints 3 2));
  check "to_string int" (R.to_string (R.of_int 42) = "42");
  check "to_string frac" (R.to_string (R.of_ints 3 7) = "(3/7)");
  check "pow_int" (R.equal (R.pow_int (R.of_int 2) 3) (R.of_int 8));
  check "pow_z neg" (R.equal (R.pow_z (R.of_int 2) (-1)) (R.of_ints 1 2));
  check "compare" (R.compare (R.of_int 1) (R.of_int 2) < 0);
  check "is_integer" (R.is_integer (R.of_int 5));
  check "not is_integer" (not (R.is_integer (R.of_ints 1 3)));
  ()

(* ------------------------------------------------------------------ *)
(* Positive & PrimeFactors                                            *)
(* ------------------------------------------------------------------ *)

let test_positive_and_primes () =
  section "Positive & PrimeFactors";
  check "of_int valid" (Surd.Positive.of_int 5 <> None);
  check "of_int zero" (Surd.Positive.of_int 0 = None);
  check "of_int neg" (Surd.Positive.of_int (-1) = None);
  check "to_int" (Surd.Positive.to_int (Surd.Positive.of_int_exn 42) = 42);

  check "factorise 1" (Surd.Prime_factors.factorise (Surd.Positive.of_int_exn 1) = []);
  check "factorise 12" (Surd.Prime_factors.factorise (Surd.Positive.of_int_exn 12) = [(2, 2); (3, 1)]);
  check "factorise 360" (Surd.Prime_factors.factorise (Surd.Positive.of_int_exn 360) = [(2, 3); (3, 2); (5, 1)]);
  check "factorise 7" (Surd.Prime_factors.factorise (Surd.Positive.of_int_exn 7) = [(7, 1)]);
  check "is_prime 2" (Surd.Prime_factors.is_prime 2);
  check "is_prime 7" (Surd.Prime_factors.is_prime 7);
  check "not is_prime 4" (not (Surd.Prime_factors.is_prime 4));
  check "not is_prime 1" (not (Surd.Prime_factors.is_prime 1));
  ()

(* ------------------------------------------------------------------ *)
(* RadExpr                                                            *)
(* ------------------------------------------------------------------ *)

let test_rad_expr () =
  section "RadExpr";
  let e1 = E.add (E.lit (R.of_int 1)) (E.lit (R.of_int 2)) in
  let e2 = E.add (E.lit (R.of_int 1)) (E.lit (R.of_int 2)) in
  check "structural equal" (E.equal R.equal e1 e2);

  let e3 = E.mul (E.lit (R.of_int 3)) (E.root 2 (E.lit (R.of_int 5))) in
  check "size" (E.size e3 = 4);  (* Mul, Lit 3, Root, Lit 5 *)
  check "depth" (E.depth e3 = 2);

  let e4 = E.add (E.root 2 (E.lit (R.of_int 2))) (E.root 2 (E.lit (R.of_int 3))) in
  let rads = E.collect_radicals R.equal e4 in
  check "collect_radicals" (List.length rads = 2);

  let mapped = E.map (fun r -> R.mul r (R.of_int 2)) (E.lit (R.of_int 3)) in
  check "map" (E.equal R.equal mapped (E.lit (R.of_int 6)));
  ()

(* ------------------------------------------------------------------ *)
(* Eval                                                               *)
(* ------------------------------------------------------------------ *)

let test_eval () =
  section "Eval";
  (* sqrt(2) ~ 1.41421 *)
  let sqrt2 = E.root 2 (E.lit (R.of_int 2)) in
  check_float "eval_float sqrt(2)" 1.41421356 (Surd.Eval.eval_float sqrt2) 1e-5;

  (* 3 + 2*sqrt(5) *)
  let e = E.add (E.lit (R.of_int 3))
              (E.mul (E.lit (R.of_int 2)) (E.root 2 (E.lit (R.of_int 5)))) in
  let v = Surd.Eval.eval_float e in
  check_float "eval_float 3+2sqrt(5)" (3.0 +. 2.0 *. Float.sqrt 5.0) v 1e-10;

  (* Complex eval: sqrt(-1) *)
  let i = E.root 2 (E.lit (R.of_int (-1))) in
  let z = Surd.Eval.eval_complex i in
  check_float "eval_complex sqrt(-1) re" 0.0 z.re 1e-10;
  check_float "eval_complex sqrt(-1) im" 1.0 z.im 1e-10;

  (* Inv *)
  let inv3 = E.inv (E.lit (R.of_int 3)) in
  check_float "eval_float 1/3" (1.0 /. 3.0) (Surd.Eval.eval_float inv3) 1e-10;

  (* Pow *)
  let sq = E.pow (E.lit (R.of_int 3)) 2 in
  check_float "eval_float 3^2" 9.0 (Surd.Eval.eval_float sq) 1e-10;
  ()

(* ------------------------------------------------------------------ *)
(* Normalize                                                          *)
(* ------------------------------------------------------------------ *)

let test_normalize () =
  section "Normalize";

  (* Double negation: --x = x *)
  let e = E.neg (E.neg (E.lit (R.of_int 5))) in
  let n = Surd.Normalize.normalize e in
  check "double negation" (E.equal R.equal n (E.lit (R.of_int 5)));

  (* 0 + x = x *)
  let e = E.add (E.lit R.zero) (E.lit (R.of_int 3)) in
  let n = Surd.Normalize.normalize e in
  check "add zero" (E.equal R.equal n (E.lit (R.of_int 3)));

  (* 1 * x = x *)
  let e = E.mul (E.lit R.one) (E.root 2 (E.lit (R.of_int 7))) in
  let n = Surd.Normalize.normalize e in
  check "mul one" (E.equal R.equal n (E.root 2 (E.lit (R.of_int 7))));

  (* sqrt(12) = 2*sqrt(3) *)
  let e = E.root 2 (E.lit (R.of_int 12)) in
  let n = Surd.Normalize.normalize e in
  let expected = E.mul (E.lit (R.of_int 2)) (E.root 2 (E.lit (R.of_int 3))) in
  check "sqrt(12) = 2*sqrt(3)" (E.equal R.equal n expected);

  (* sqrt(2)^2 = 2 *)
  let e = E.pow (E.root 2 (E.lit (R.of_int 2))) 2 in
  let n = Surd.Normalize.normalize e in
  check "sqrt(2)^2 = 2" (E.equal R.equal n (E.lit (R.of_int 2)));

  (* 3*sqrt(5) + 2*sqrt(5) = 5*sqrt(5) *)
  let s5 = E.root 2 (E.lit (R.of_int 5)) in
  let e = E.add (E.mul (E.lit (R.of_int 3)) s5) (E.mul (E.lit (R.of_int 2)) s5) in
  let n = Surd.Normalize.normalize e in
  let expected = E.mul (E.lit (R.of_int 5)) s5 in
  check "collect like terms" (E.equal R.equal n expected);

  (* 0 * anything = 0 *)
  let e = E.mul (E.lit R.zero) (E.root 2 (E.lit (R.of_int 17))) in
  let n = Surd.Normalize.normalize e in
  check "0 * x = 0" (E.equal R.equal n (E.lit R.zero));

  (* sqrt(sqrt(x)) = 4th-root(x) *)
  let e = E.root 2 (E.root 2 (E.lit (R.of_int 5))) in
  let n = Surd.Normalize.normalize e in
  let expected = E.root 4 (E.lit (R.of_int 5)) in
  check "sqrt(sqrt(5)) = 4th-root(5)" (E.equal R.equal n expected);
  ()

(* ------------------------------------------------------------------ *)
(* Polynomial                                                         *)
(* ------------------------------------------------------------------ *)

let test_poly () =
  section "Polynomial";

  (* x + 1 *)
  let p1 = P.of_coeffs [R.one; R.one] in
  check "degree of x+1" (P.degree p1 = 1);

  (* x^2 - 1 = (x+1)(x-1) *)
  let p2 = P.of_coeffs [R.minus_one; R.zero; R.one] in
  check "degree of x^2-1" (P.degree p2 = 2);

  (* (x+1) * (x-1) = x^2 - 1 *)
  let p3 = P.of_coeffs [R.neg R.one; R.one] in
  let product = P.mul p1 p3 in
  check "mul (x+1)(x-1) = x^2-1" (P.equal product p2);

  (* Division *)
  let q, r = P.div_mod p2 p1 in
  check "div_mod quotient" (P.equal q p3);
  check "div_mod remainder" (P.equal r P.zero);

  (* GCD *)
  let g = P.gcd p2 p1 in
  check "gcd has right degree" (P.degree g = 1);

  (* Eval *)
  let v = P.eval p2 (R.of_int 3) in
  check "eval x^2-1 at 3 = 8" (R.equal v (R.of_int 8));

  (* Derivative *)
  let d = P.diff p2 in
  let expected_d = P.of_coeffs [R.zero; R.of_int 2] in
  check "diff x^2-1 = 2x" (P.equal d expected_d);

  (* Composition *)
  let p_x_plus_1 = P.of_coeffs [R.one; R.one] in
  let composed = P.compose p2 p_x_plus_1 in
  (* (x+1)^2 - 1 = x^2 + 2x *)
  let expected_c = P.of_coeffs [R.zero; R.of_int 2; R.one] in
  check "compose (x^2-1)(x+1)" (P.equal composed expected_c);
  ()

(* ------------------------------------------------------------------ *)
(* Extension field                                                    *)
(* ------------------------------------------------------------------ *)

let test_extension () =
  section "Extension field";
  let module Ext = Surd.Extension.Make(R.Field)(struct
    type scalar = R.t
    (* x^2 + 1 = 0, so alpha = sqrt(-1) *)
    let coeffs = [R.one; R.zero; R.one]
  end) in
  (* alpha^2 = -1 *)
  let a = Ext.generator in
  let a2 = Ext.mul a a in
  let neg_one = Ext.of_base (R.neg R.one) in
  check "alpha^2 = -1" (Ext.equal a2 neg_one);

  (* (1 + alpha)(1 - alpha) = 1 - alpha^2 = 2 *)
  let one_plus_a = Ext.add (Ext.of_base R.one) a in
  let one_minus_a = Ext.sub (Ext.of_base R.one) a in
  let product = Ext.mul one_plus_a one_minus_a in
  let two = Ext.of_base (R.of_int 2) in
  check "(1+i)(1-i) = 2" (Ext.equal product two);

  (* Inverse: 1/alpha = -alpha *)
  let inv_a = Ext.inv a in
  let neg_a = Ext.neg a in
  check "1/i = -i" (Ext.equal inv_a neg_a);
  ()

(* ------------------------------------------------------------------ *)
(* Cyclotomic                                                         *)
(* ------------------------------------------------------------------ *)

let test_cyclotomic () =
  section "Cyclotomic";

  (* Phi_1(x) = x - 1 *)
  let phi1 = Surd.Cyclotomic.cyclotomic 1 in
  let expected = P.of_coeffs [R.neg R.one; R.one] in
  check "Phi_1 = x-1" (P.equal phi1 expected);

  (* Phi_2(x) = x + 1 *)
  let phi2 = Surd.Cyclotomic.cyclotomic 2 in
  let expected = P.of_coeffs [R.one; R.one] in
  check "Phi_2 = x+1" (P.equal phi2 expected);

  (* Phi_3(x) = x^2 + x + 1 *)
  let phi3 = Surd.Cyclotomic.cyclotomic 3 in
  check "Phi_3 degree" (P.degree phi3 = 2);

  (* Phi_4(x) = x^2 + 1 *)
  let phi4 = Surd.Cyclotomic.cyclotomic 4 in
  let expected = P.of_coeffs [R.one; R.zero; R.one] in
  check "Phi_4 = x^2+1" (P.equal phi4 expected);

  (* Euler totient *)
  check "euler_totient 1" (Surd.Cyclotomic.euler_totient 1 = 1);
  check "euler_totient 6" (Surd.Cyclotomic.euler_totient 6 = 2);
  check "euler_totient 12" (Surd.Cyclotomic.euler_totient 12 = 4);
  ()

(* ------------------------------------------------------------------ *)
(* Pretty-printing                                                    *)
(* ------------------------------------------------------------------ *)

let test_pretty () =
  section "Pretty-printing";

  check "pretty lit 3" (Surd.Pretty.pretty (E.lit (R.of_int 3)) = "3");
  check "pretty lit -3" (Surd.Pretty.pretty (E.lit (R.of_int (-3))) = "-3");
  check "pretty frac" (Surd.Pretty.pretty (E.lit (R.of_ints 1 2)) = "(1/2)");

  let s2 = E.root 2 (E.lit (R.of_int 2)) in
  let p = Surd.Pretty.pretty s2 in
  check "pretty sqrt(2)" (p = "\xe2\x88\x9a2");

  let e = E.add (E.lit (R.of_int 1)) s2 in
  let p = Surd.Pretty.pretty e in
  check "pretty 1+sqrt(2)" (p = "1 + \xe2\x88\x9a2");
  ()

(* ------------------------------------------------------------------ *)
(* LaTeX                                                              *)
(* ------------------------------------------------------------------ *)

let test_latex () =
  section "LaTeX";

  check "latex lit 3" (Surd.Latex.latex (E.lit (R.of_int 3)) = "3");

  let s2 = E.root 2 (E.lit (R.of_int 2)) in
  check "latex sqrt(2)" (Surd.Latex.latex s2 = "\\sqrt{2}");

  let s3 = E.root 3 (E.lit (R.of_int 5)) in
  check "latex cbrt(5)" (Surd.Latex.latex s3 = "\\sqrt[3]{5}");

  let i = E.root 2 (E.lit (R.of_int (-1))) in
  check "latex i" (Surd.Latex.latex i = "\\mathrm{i}");
  ()

(* ------------------------------------------------------------------ *)
(* Trig                                                               *)
(* ------------------------------------------------------------------ *)

let test_trig () =
  section "Trig";

  (* cos(0) = 1 *)
  let c0 = Surd.Trig.cos_exact 0 1 in
  check_float "cos(0) = 1" 1.0 (Surd.Eval.eval_float c0) 1e-10;

  (* cos(pi/2) = 0 *)
  let c90 = Surd.Trig.cos_exact 1 2 in
  check_float "cos(pi/2) = 0" 0.0 (Surd.Eval.eval_float c90) 1e-10;

  (* cos(pi/3) = 0.5 *)
  let c60 = Surd.Trig.cos_exact 1 3 in
  check_float "cos(pi/3) = 0.5" 0.5 (Surd.Eval.eval_float c60) 1e-10;

  (* cos(pi/4) = sqrt(2)/2 *)
  let c45 = Surd.Trig.cos_exact 1 4 in
  check_float "cos(pi/4)" (Float.sqrt 2.0 /. 2.0) (Surd.Eval.eval_float c45) 1e-10;

  (* cos(pi/6) = sqrt(3)/2 *)
  let c30 = Surd.Trig.cos_exact 1 6 in
  check_float "cos(pi/6)" (Float.sqrt 3.0 /. 2.0) (Surd.Eval.eval_float c30) 1e-10;

  (* sin(pi/6) = 0.5 *)
  let s30 = Surd.Trig.sin_exact 1 6 in
  check_float "sin(pi/6) = 0.5" 0.5 (Surd.Eval.eval_float s30) 1e-10;
  ()

(* ------------------------------------------------------------------ *)
(* Root isolation                                                     *)
(* ------------------------------------------------------------------ *)

let test_root_isolation () =
  section "Root isolation";

  (* x^2 - 2 has roots at +/- sqrt(2) *)
  let f = P.of_coeffs [R.of_int (-2); R.zero; R.one] in
  let roots = Surd.Root_isolation.isolate_roots f in
  check "x^2-2 has 2 roots" (List.length roots = 2);

  (* x - 3 has one root at 3 *)
  let g = P.of_coeffs [R.of_int (-3); R.one] in
  let roots = Surd.Root_isolation.isolate_roots g in
  check "x-3 has 1 root" (List.length roots = 1);
  ()

(* ------------------------------------------------------------------ *)
(* Normal form                                                        *)
(* ------------------------------------------------------------------ *)

let test_normal_form () =
  section "Normal form";

  (* NF of a rational *)
  let e = E.lit (R.of_int 5) in
  let nf = Surd.Normal_form.simplify_via_nf e in
  check_float "NF of 5" 5.0 (Surd.Eval.eval_float nf) 1e-10;

  (* NF of sqrt(2) + sqrt(2) = 2*sqrt(2) *)
  let s2 = E.root 2 (E.lit (R.of_int 2)) in
  let e = E.add s2 s2 in
  let nf = Surd.Normal_form.simplify_via_nf e in
  check_float "NF of sqrt(2)+sqrt(2)" (2.0 *. Float.sqrt 2.0) (Surd.Eval.eval_float nf) 1e-10;
  ()

(* ------------------------------------------------------------------ *)
(* Denesting                                                          *)
(* ------------------------------------------------------------------ *)

let test_denest () =
  section "Denesting";

  (* sqrt(3 + 2*sqrt(2)) = 1 + sqrt(2) *)
  let radicand = E.add (E.lit (R.of_int 3))
                       (E.mul (E.lit (R.of_int 2)) (E.root 2 (E.lit (R.of_int 2)))) in
  let e = E.root 2 radicand in
  let d = Surd.Denest.denest e in
  check_float "denest sqrt(3+2sqrt(2))"
    (1.0 +. Float.sqrt 2.0) (Surd.Eval.eval_float d) 1e-10;
  ()

(* ------------------------------------------------------------------ *)
(* Main                                                               *)
(* ------------------------------------------------------------------ *)

let () =
  Printf.printf "Surd OCaml Test Suite\n";
  Printf.printf "=====================\n";
  test_rational ();
  test_positive_and_primes ();
  test_rad_expr ();
  test_eval ();
  test_normalize ();
  test_poly ();
  test_extension ();
  test_cyclotomic ();
  test_pretty ();
  test_latex ();
  test_trig ();
  test_root_isolation ();
  test_normal_form ();
  test_denest ();
  Printf.printf "\n=====================\n";
  Printf.printf "Results: %d passed, %d failed\n" !tests_passed !tests_failed;
  if !tests_failed > 0 then begin
    Printf.printf "Failed tests:\n";
    List.iter (fun name -> Printf.printf "  - %s\n" name) (List.rev !test_errors);
    exit 1
  end else
    Printf.printf "All tests passed!\n"
