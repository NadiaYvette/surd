(** Normalization passes for radical expressions.

    These are explicit transformations the user applies as needed.
    They do NOT denest -- see {!Denest} for that. *)

open Rad_expr

(** Module alias for concise rational comparisons. *)
module R = Rational

(** {2 Individual passes} *)

(** Flatten nested Add/Mul (associativity) and cancel double negations. *)
let rec flatten_arith : 'k t -> 'k t = function
  | Neg (Neg a) -> flatten_arith a
  | Neg a -> Neg (flatten_arith a)
  | Add (a, b) -> Add (flatten_arith a, flatten_arith b)
  | Mul (a, b) -> Mul (flatten_arith a, flatten_arith b)
  | Inv (Inv a) -> flatten_arith a
  | Inv a -> Inv (flatten_arith a)
  | Root (n, a) -> Root (n, flatten_arith a)
  | Pow (a, n) -> Pow (flatten_arith a, n)
  | e -> e

(** Fold constant subexpressions: evaluate pure-literal subtrees. *)
let rec fold_constants : R.t t -> R.t t = function
  | Lit r -> Lit r
  | Neg a -> begin match fold_constants a with
    | Lit r -> Lit (R.neg r)
    | Neg a' -> a'  (* double negation *)
    | a' -> Neg a'
    end
  | Add (a, b) -> begin match fold_constants a, fold_constants b with
    | Lit r, Lit s -> Lit (R.add r s)
    | Lit r, b' when R.is_zero r -> b'
    | a', Lit s when R.is_zero s -> a'
    | a', Neg b' when Rad_expr.equal R.equal a' b' -> Lit R.zero
    | a', b' -> Add (a', b')
    end
  | Mul (a, b) -> begin match fold_constants a, fold_constants b with
    | Lit r, Lit s -> Lit (R.mul r s)
    | Lit r, _ when R.is_zero r -> Lit R.zero
    | _, Lit s when R.is_zero s -> Lit R.zero
    | Lit r, b' when R.is_one r -> b'
    | a', Lit s when R.is_one s -> a'
    | Lit r, b' when R.equal r R.minus_one -> Neg b'
    | a', Lit s when R.equal s R.minus_one -> Neg a'
    | a', b' -> Mul (a', b')
    end
  | Inv a -> begin match fold_constants a with
    | Lit r when not (R.is_zero r) -> Lit (R.inv r)
    | Inv a' -> a'  (* double inverse *)
    | a' -> Inv a'
    end
  | Root (n, a) -> begin match fold_constants a with
    | Lit r when R.is_zero r -> Lit R.zero
    | Lit r when R.is_one r -> Lit R.one
    | a' -> Root (n, a')
    end
  | Pow (a, n) -> begin match fold_constants a with
    | Lit r -> Lit (R.pow_z r n)
    | a' -> match n with
      | 0 -> Lit R.one
      | 1 -> a'
      | _ -> Pow (a', n)
    end

(** Simplify power expressions:
    - [Pow (Pow a m) n  -->  Pow a (m*n)]
    - [Pow (Root n a) n -->  a]
    - [Root n (Pow a n) -->  a]
    - [Root m (Root n a) --> Root (m*n) a]
    - [Mul (Root 2 a) (Root 2 a) --> a] *)
let rec simplify_powers : R.t t -> R.t t = function
  | Mul (Root (2, a), Root (2, b)) when Rad_expr.equal R.equal a b ->
    simplify_powers a
  | Pow (Pow (a, m), n) -> simplify_powers (Pow (a, m * n))
  | Pow (Root (n, a), m) when m = n -> simplify_powers a
  | Root (n, Pow (a, m)) when m = n -> simplify_powers a
  | Root (m, Root (n, a)) -> Root (m * n, simplify_powers a)
  | Neg a -> Neg (simplify_powers a)
  | Add (a, b) -> Add (simplify_powers a, simplify_powers b)
  | Mul (a, b) -> Mul (simplify_powers a, simplify_powers b)
  | Inv a -> Inv (simplify_powers a)
  | Root (n, a) -> Root (n, simplify_powers a)
  | Pow (a, n) -> Pow (simplify_powers a, n)
  | e -> e

(** Extract the largest n-th power factor from a positive integer.
    Returns [(extracted, remainder)] where [m = extracted^n * remainder]. *)
let extract_nth_power (n : int) (m : int) : R.t * R.t =
  let m = abs m in
  let pos = Positive.of_int_exn m in
  let fs = Prime_factors.factorise pos in
  let extracted = List.fold_left
    (fun acc (p, e) ->
      let q = e / n in
      let rec ipow base exp = if exp = 0 then 1 else base * ipow base (exp - 1) in
      acc * ipow p q)
    1 fs
  in
  let remainder = List.fold_left
    (fun acc (p, e) ->
      let r = e mod n in
      let rec ipow base exp = if exp = 0 then 1 else base * ipow base (exp - 1) in
      acc * ipow p r)
    1 fs
  in
  (R.of_int extracted, R.of_int remainder)

(** Extract perfect n-th powers from under radicals.
    E.g., [Root (2, Lit 12)] --> [Mul (Lit 2, Root (2, Lit 3))]. *)
let rec extract_perfect_powers : R.t t -> R.t t = function
  | Root (n, Lit r) when R.is_pos r ->
    let num_z = Z.to_int (R.num r) in
    let den_z = Z.to_int (R.den r) in
    let (num_out, num_in) = extract_nth_power n num_z in
    let (den_out, den_in) = extract_nth_power n den_z in
    let outer_coeff, inner_rat =
      if R.is_one den_in then
        (R.div num_out den_out, num_in)
      else
        let rec ipow base exp = if exp = 0 then R.one else R.mul base (ipow base (exp - 1)) in
        let new_inner = R.mul num_in (ipow den_in (n - 1)) in
        let new_outer = R.div num_out (R.mul den_out den_in) in
        (* Re-extract from new_inner *)
        let ni_int = Z.to_int (R.num new_inner) in
        let (num_out2, num_in2) = extract_nth_power n ni_int in
        (R.mul new_outer num_out2, num_in2)
    in
    begin match R.is_one outer_coeff, R.is_one inner_rat with
    | true, true -> Lit R.one
    | true, false -> Root (n, Lit inner_rat)
    | _, true -> Lit outer_coeff
    | _ -> Mul (Lit outer_coeff, Root (n, Lit inner_rat))
    end
  | Root (n, Lit r) when R.is_neg r && n mod 2 = 1 ->
    Neg (extract_perfect_powers (Root (n, Lit (R.neg r))))
  | Root (n, a) -> Root (n, extract_perfect_powers a)
  | Neg a -> Neg (extract_perfect_powers a)
  | Add (a, b) -> Add (extract_perfect_powers a, extract_perfect_powers b)
  | Mul (a, b) -> Mul (extract_perfect_powers a, extract_perfect_powers b)
  | Inv a -> Inv (extract_perfect_powers a)
  | Pow (a, n) -> Pow (extract_perfect_powers a, n)
  | e -> e

let rec flatten_add = function
  | Add (a, b) -> flatten_add a @ flatten_add b
  | e -> [e]

let rec flatten_mul = function
  | Mul (a, b) -> flatten_mul a @ flatten_mul b
  | e -> [e]

let build_add = function
  | [] -> Lit R.zero
  | [x] -> x
  | x :: xs -> List.fold_left (fun acc e -> Add (acc, e)) x xs

let build_mul = function
  | [] -> Lit R.one
  | [x] -> x
  | x :: xs -> List.fold_left (fun acc e -> Mul (acc, e)) x xs

(** Sort children of commutative operators into canonical order.
    Ensures [a + b] and [b + a] normalise to the same expression. *)
let sort_commutative : R.t t -> R.t t =
  let cmp = Rad_expr.compare R.compare in
  let rec go = function
    | Add _ as e ->
      let terms = flatten_add e in
      let sorted = List.sort cmp (List.map go terms) in
      build_add sorted
    | Mul _ as e ->
      let factors = flatten_mul e in
      let sorted = List.sort cmp (List.map go factors) in
      build_mul sorted
    | Neg a -> Neg (go a)
    | Inv a -> Inv (go a)
    | Root (n, a) -> Root (n, go a)
    | Pow (a, n) -> Pow (go a, n)
    | e -> e
  in
  go

(** Collect rational coefficients in products.
    Flattens a product tree, multiplies all [Lit] factors together,
    and produces a single [Mul (Lit coeff, body)]. *)
let collect_coefficients : R.t t -> R.t t =
  let partition_lits exprs =
    List.fold_right (fun e (lits, rest) ->
      match e with
      | Lit r -> (r :: lits, rest)
      | Inv (Lit r) when not (R.is_zero r) -> (R.inv r :: lits, rest)
      | _ -> (lits, e :: rest))
      exprs ([], [])
  in
  let apply_coeff c body =
    if R.is_zero c then Lit R.zero
    else if R.is_one c then body
    else if R.equal c R.minus_one then Neg body
    else match body with
      | Lit r -> Lit (R.mul c r)
      | _ -> Mul (Lit c, body)
  in
  let rec go = function
    | Mul _ as e ->
      let factors = flatten_mul e in
      let (lits, rest) = partition_lits (List.map go factors) in
      let coeff = List.fold_left R.mul R.one lits in
      let body = build_mul rest in
      apply_coeff coeff body
    | Neg a -> begin match go a with
      | Lit r -> Lit (R.neg r)
      | a' -> Neg a'
      end
    | Add (a, b) -> Add (go a, go b)
    | Inv a -> Inv (go a)
    | Root (n, a) -> Root (n, go a)
    | Pow (a, n) -> Pow (go a, n)
    | e -> e
  in
  go

(** Collect like terms in sums.
    Flattens a sum tree, groups summands by their non-coefficient "base" part,
    adds up coefficients within each group, and rebuilds.
    E.g., [3*sqrt(5) + 2*sqrt(5)] becomes [5*sqrt(5)]. *)
let collect_terms : R.t t -> R.t t =
  let module ExprMap = Map.Make(struct
    type t = R.t Rad_expr.t
    let compare = Rad_expr.compare R.compare
  end) in
  let split_coeff = function
    | Mul (Lit c, body) -> (c, body)
    | Neg e ->
      let rec inner = function
        | Mul (Lit c, body) -> (R.neg c, body)
        | Neg e -> let (c, b) = inner e in (R.neg c, b)
        | Lit r -> (R.neg r, Lit R.one)
        | e -> (R.minus_one, e)
      in inner e
    | Lit r -> (r, Lit R.one)
    | e -> (R.one, e)
  in
  let group_terms terms =
    List.fold_left (fun m term ->
      let (c, base) = split_coeff term in
      let cur = match ExprMap.find_opt base m with
        | Some v -> v
        | None -> R.zero
      in
      ExprMap.add base (R.add cur c) m)
      ExprMap.empty terms
  in
  let apply_coeff c base =
    if R.is_one c && Rad_expr.equal R.equal base (Lit R.one) then Lit R.one
    else if R.is_one c then base
    else if R.equal c R.minus_one then Neg base
    else match base with
      | Lit r when R.is_one r -> Lit c
      | _ -> Mul (Lit c, base)
  in
  let rec go = function
    | Add _ as e ->
      let terms = flatten_add e in
      let processed = List.map go terms in
      let grouped = group_terms processed in
      let sorted = List.sort (fun (a, _) (b, _) ->
        Rad_expr.compare R.compare a b)
        (ExprMap.bindings grouped) in
      let rebuilt = List.filter_map (fun (base, c) ->
        if R.is_zero c then None
        else Some (apply_coeff c base))
        sorted
      in
      build_add rebuilt
    | Neg a -> Neg (go a)
    | Mul (a, b) -> Mul (go a, go b)
    | Inv a -> Inv (go a)
    | Root (n, a) -> Root (n, go a)
    | Pow (a, n) -> Pow (go a, n)
    | e -> e
  in
  go

(** Distribute scalar multiplication over addition and simplify roots.
    [Lit c * (a + b)] becomes [Lit c * a + Lit c * b]. *)
let distribute : R.t t -> R.t t =
  let is_sum = function Add _ -> true | Neg e -> (match e with Add _ -> true | _ -> false) | _ -> false in
  let rec flatten_s = function
    | Add (a, b) -> flatten_s a @ flatten_s b
    | Neg a -> List.map (fun e -> Neg e) (flatten_s a)
    | e -> [e]
  in
  let is_tiny_sum e = is_sum e && List.length (flatten_s e) <= 2 in
  let rebuild_add = function
    | [] -> Lit R.zero
    | x :: xs -> List.fold_left (fun acc e -> Add (acc, e)) x xs
  in
  let rec go = function
    (* Lit * (a + b + ...) -> Lit*a + Lit*b + ... *)
    | Mul (Lit c, r) when is_sum r ->
      let terms = flatten_s r in
      rebuild_add (List.map (fun t -> Mul (Lit c, t)) terms)
    | Mul (l, Lit c) when is_sum l ->
      let terms = flatten_s l in
      rebuild_add (List.map (fun t -> Mul (t, Lit c)) terms)
    (* (a + b) * x -> a*x + b*x when sum is tiny *)
    | Mul (l, r) when is_tiny_sum l && not (is_sum r) ->
      let terms = flatten_s l in
      rebuild_add (List.map (fun t -> go (Mul (t, r))) terms)
    | Mul (l, r) when not (is_sum l) && is_tiny_sum r ->
      let terms = flatten_s r in
      rebuild_add (List.map (fun t -> go (Mul (l, t))) terms)
    (* c * (-a) -> -(c*a) *)
    | Mul (Lit c, Neg a) -> Neg (go (Mul (Lit c, a)))
    | Mul (Neg a, Lit c) -> Neg (go (Mul (a, Lit c)))
    (* Recurse *)
    | Neg a -> Neg (go a)
    | Add (a, b) -> Add (go a, go b)
    | Mul (a, b) -> Mul (go a, go b)
    | Inv a -> Inv (go a)
    | Root (n, a) -> Root (n, go a)
    | Pow (a, n) -> Pow (go a, n)
    | e -> e
  in
  go

(** {2 Composite normalisation} *)

(** A single normalisation pass (all sub-passes composed once). *)
let normalize_once (e : R.t t) : R.t t =
  e
  |> flatten_arith
  |> fold_constants
  |> simplify_powers
  |> extract_perfect_powers
  |> sort_commutative
  |> distribute
  |> collect_coefficients
  |> collect_terms

(** Iterate [f] until fixed point or fuel runs out. *)
let fix_n n f x =
  let rec go fuel x =
    if fuel = 0 then x
    else
      let x' = f x in
      if Rad_expr.equal R.equal x x' then x
      else go (fuel - 1) x'
  in
  go n x

(** Apply all normalisation passes, iterated to a fixed point (max 10 rounds). *)
let normalize (e : R.t t) : R.t t =
  fix_n 10 normalize_once e
