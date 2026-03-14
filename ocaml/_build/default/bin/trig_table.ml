(** Demo: print a table of exact trig values. *)

let () =
  Printf.printf "Exact Trigonometric Values\n";
  Printf.printf "=========================\n\n";
  let angles = [
    (0, 1, "0");
    (1, 6, "pi/6");
    (1, 4, "pi/4");
    (1, 3, "pi/3");
    (1, 2, "pi/2");
    (2, 3, "2pi/3");
    (3, 4, "3pi/4");
    (5, 6, "5pi/6");
    (1, 1, "pi");
  ] in
  Printf.printf "%-10s  %-30s  %-30s  %-40s\n" "angle" "cos (text)" "sin (text)" "cos (LaTeX)";
  Printf.printf "%s\n" (String.make 115 '-');
  List.iter (fun (p, q, label) ->
    let cos_e = Surd.Trig.cos_exact p q in
    let sin_e = Surd.Trig.sin_exact p q in
    let cos_text = Surd.Pretty.pretty cos_e in
    let sin_text = Surd.Pretty.pretty sin_e in
    let cos_latex = Surd.Latex.latex cos_e in
    Printf.printf "%-10s  %-30s  %-30s  %-40s\n" label cos_text sin_text cos_latex)
    angles;
  Printf.printf "\nNumerical verification:\n";
  List.iter (fun (p, q, label) ->
    let cos_e = Surd.Trig.cos_exact p q in
    let sin_e = Surd.Trig.sin_exact p q in
    let cv = Surd.Eval.eval_float cos_e in
    let sv = Surd.Eval.eval_float sin_e in
    let angle = Float.pi *. float_of_int p /. float_of_int q in
    let ce = Float.cos angle in
    let se = Float.sin angle in
    Printf.printf "  %s: cos err=%.2e, sin err=%.2e\n" label
      (Float.abs (cv -. ce)) (Float.abs (sv -. se)))
    angles
