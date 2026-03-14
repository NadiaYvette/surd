(** Galois group identification for irreducible polynomials over Q.

    Uses generalized Stauduhar descent:
    1. Compute discriminant; if perfect square, group <= A_n.
    2. Use Frobenius/Chebotarev factorisation patterns mod small primes
       to determine if group is inside AGL(1,p) and to find the exact
       stabiliser order.

    For prime degree p, all solvable transitive groups are affine:
    Z/p ⋊ H with H cyclic of order d | (p-1). The factorisation
    patterns of f mod primes determine d. *)

module P = Poly.RatPoly

(** Result of Galois group identification. *)
type galois_result = {
  group : Transitive_group.t;
  roots : Complex.t list;
}

(** Small odd primes for Frobenius/Chebotarev test. *)
let small_primes =
  [3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47; 53;
   59; 61; 67; 71; 73; 79; 83; 89; 97; 101; 103; 107; 109; 113]

(** Test whether an integer is prime. *)
let is_prime_deg n =
  if n < 2 then false
  else if n < 4 then true
  else if n mod 2 = 0 then false
  else
    let rec check d =
      if d * d > n then true
      else if n mod d = 0 then false
      else check (d + 2)
    in
    check 3

(** {2 Frobenius test for cyclic groups} *)

(** Test whether the factorisation pattern modulo p is inconsistent
    with a cyclic Galois group (i.e., is not {n} or {1,1,...,1}). *)
let has_non_cyclic_pattern int_cs p n =
  let p_int = p in
  let cs = List.map (fun c -> ((c mod p_int) + p_int) mod p_int) int_cs in
  let pat = Resolvent.factor_pattern cs p_int in
  pat <> [n] && pat <> List.init n (fun _ -> 1)

(** {2 Sextic resolvent for degree 5} *)

(** All permutations of a list. *)
let rec perms = function
  | [] -> [[]]
  | xs ->
    let picks lst =
      List.mapi (fun i x ->
        let rest = List.filteri (fun j _ -> j <> i) lst in
        (x, rest)
      ) lst
    in
    List.concat_map (fun (x, ys) ->
      List.map (fun rest -> x :: rest) (perms ys)
    ) (picks xs)

(** Cluster complex values by proximity. *)
let cluster_by_distance vals tol =
  let clusters = ref [] in
  List.iter (fun v ->
    let found = ref false in
    clusters := List.map (fun cl ->
      if !found then cl
      else
        let min_dist = List.fold_left (fun acc c ->
          Float.min acc (Complex.norm (Complex.sub v c))
        ) Float.infinity cl in
        if min_dist < tol then begin
          found := true;
          v :: cl
        end else cl
    ) !clusters;
    if not !found then clusters := [v] :: !clusters
  ) vals;
  !clusters

let cluster_center cs =
  let n = Float.of_int (List.length cs) in
  let sum = List.fold_left Complex.add Complex.zero cs in
  Complex.div sum { Complex.re = n; im = 0.0 }

(** Construct the sextic resolvent from 5 numerical roots. *)
let sextic_resolvent_5 roots =
  let theta xs =
    let get i = List.nth xs (i mod 5) in
    let sum = ref Complex.zero in
    for i = 0 to 4 do
      let xi = get i in
      let t1 = Complex.mul (get (i + 1)) (get (i + 4)) in
      let t2 = Complex.mul (get (i + 2)) (get (i + 3)) in
      sum := Complex.add !sum
        (Complex.mul (Complex.mul xi xi) (Complex.add t1 t2))
    done;
    !sum
  in
  let all_perms = perms [0; 1; 2; 3; 4] in
  let vals = List.map (fun p ->
    theta (List.map (fun j -> List.nth roots j) p)
  ) all_perms in
  (* Try a range of tolerances to find one that gives exactly 6 clusters *)
  let try_tol tol =
    let clusters = cluster_by_distance vals tol in
    if List.length clusters = 6 then begin
      let centers = List.map cluster_center clusters in
      Resolvent.round_to_rat_poly centers
    end else
      None
  in
  (* Try the default first, then wider tolerances *)
  let tols = [1e-4; 1e-3; 1e-2; 5e-2] in
  let rec try_all = function
    | [] -> None
    | t :: rest ->
      match try_tol t with
      | Some p -> Some p
      | None -> try_all rest
  in
  try_all tols

(** {2 Degree-5 identification} *)

(** Frobenius/Chebotarev test to distinguish C_5 from D_5.

    For C_5, only patterns {5} and {1,1,1,1,1} are possible.
    D_5 additionally admits {1,2,2}. *)
let is_cyclic_by_frobenius f =
  let cs = P.to_coeffs f in
  try
    let lcm_den = List.fold_left (fun acc c ->
      let d = Z.to_int (Rational.den c) in
      let rec gcd a b = if b = 0 then a else gcd b (a mod b) in
      acc / (gcd acc d) * d
    ) 1 cs in
    let int_cs = List.map (fun c ->
      Z.to_int (Rational.num (Rational.mul c (Rational.of_int lcm_den)))
    ) cs in
    let lc = List.nth int_cs (List.length int_cs - 1) in
    let disc = Resolvent.discriminant_of f in
    let disc_n = Z.to_int (Rational.num disc) in
    let disc_d = Z.to_int (Rational.den disc) in
    let good_prime p =
      lc mod p <> 0 && disc_n mod p <> 0 && disc_d mod p <> 0
    in
    let test_ps = List.filteri (fun i _ -> i < 20)
      (List.filter good_prime small_primes) in
    not (List.exists (fun p -> has_non_cyclic_pattern int_cs p 5) test_ps)
  with Z.Overflow -> false

(** Identify the Galois group of a degree-5 polynomial. *)
let identify_galois_group_5 f =
  if P.degree f <> 5 then None
  else
    try
      let disc = Resolvent.discriminant_of f in
      let disc_sq = Resolvent.is_square_rational disc in
      let roots = Resolvent.complex_roots_of f in
      match sextic_resolvent_5 roots with
      | None -> None
      | Some sextic ->
        let has_sextic_root = Resolvent.has_rational_root sextic in
        (* Determine the group.
           For degree 5, the transitive group names are:
           Z5 (order 5), D5 (order 10), Z5:Z4 = AGL(1,5) (order 20), A5 (order 60), S5 (order 120).
           The Frobenius group F20 is AGL(1,5). *)
        let groups = Transitive_group.trans_groups_of_degree 5 in
        let find_by_order ord =
          List.find_opt (fun g -> g.Transitive_group.order = ord) groups
        in
        let group_opt =
          if not has_sextic_root && not disc_sq then
            find_by_order 120  (* S5 *)
          else if not has_sextic_root && disc_sq then
            find_by_order 60   (* A5 *)
          else if has_sextic_root && not disc_sq then
            find_by_order 20   (* F20 = AGL(1,5) *)
          else if is_cyclic_by_frobenius f then
            find_by_order 5    (* C5 = Z5 *)
          else
            find_by_order 10   (* D5 *)
        in
        match group_opt with
        | Some group -> Some { group; roots }
        | None -> None
    with Z.Overflow -> None

(** {2 General prime-degree identification} *)

(** Identify the Galois group of a prime-degree polynomial via
    generalized Stauduhar descent using Frobenius patterns. *)
let identify_galois_group_prime f =
  try
  let n = P.degree f in
  let disc = Resolvent.discriminant_of f in
  let disc_sq = Resolvent.is_square_rational disc in
  let roots = Resolvent.complex_roots_of f in
  let groups = Transitive_group.trans_groups_of_degree n in
  let p = n in

  (* Collect factorisation patterns mod small primes *)
  let cs = P.to_coeffs f in
  let lcm_den = List.fold_left (fun acc c ->
    let d = Z.to_int (Rational.den c) in
    let rec gcd a b = if b = 0 then a else gcd b (a mod b) in
    acc / (gcd acc d) * d
  ) 1 cs in
  let int_cs = List.map (fun c ->
    Z.to_int (Rational.num (Rational.mul c (Rational.of_int lcm_den)))
  ) cs in
  let lc = List.nth int_cs (List.length int_cs - 1) in
  let disc_n = Z.to_int (Rational.num disc) in
  let disc_d = Z.to_int (Rational.den disc) in
  let good_prime pr =
    lc mod pr <> 0 && disc_n mod pr <> 0 && disc_d mod pr <> 0
  in
  let test_primes = List.filteri (fun i _ -> i < 50)
    (List.filter good_prime small_primes) in
  let patterns = List.map (fun pr ->
    let cs_mod = List.map (fun c -> ((c mod pr) + pr) mod pr) int_cs in
    List.sort Int.compare (Resolvent.factor_pattern cs_mod pr)
  ) test_primes in

  (* Check if patterns are consistent with AGL(1,p) *)
  let is_agl_pattern pat =
    pat = [n]                                  (* translation *)
    || pat = List.init n (fun _ -> 1)          (* identity *)
    || (List.length pat >= 2
        && List.fold_left Int.min max_int pat = 1
        && List.length (List.filter (fun x -> x = 1) pat) = 1
        && (let ks = List.filter (fun x -> x <> 1) pat in
            match ks with
            | [] -> true
            | k :: rest -> List.for_all (fun x -> x = k) rest))
  in
  let inside_agl = List.for_all is_agl_pattern patterns in

  (* Find minimum stabiliser order d *)
  let non_triv_cycle_lengths =
    List.concat_map (fun pat ->
      if pat = [n] || pat = List.init n (fun _ -> 1) then []
      else List.filter (fun k -> k <> 1) pat
    ) patterns
  in
  let lcm a b =
    let rec gcd a b = if b = 0 then a else gcd b (a mod b) in
    a / (gcd a b) * b
  in
  let min_d =
    if non_triv_cycle_lengths = [] then 1
    else List.fold_left lcm 1 non_triv_cycle_lengths
  in

  let divs = List.sort Int.compare
    (List.filter_map (fun g ->
      if g.Transitive_group.solvable then Some (g.Transitive_group.order / p)
      else None
    ) groups)
  in

  if not inside_agl then begin
    (* Not in AGL(1,p): either A_p or S_p *)
    let final_group =
      if disc_sq then
        match List.find_opt (fun g ->
          g.Transitive_group.name = Printf.sprintf "A%d" p) groups with
        | Some g -> g
        | None -> List.nth groups (List.length groups - 1)
      else
        List.nth groups (List.length groups - 1)  (* S_p *)
    in
    Some { group = final_group; roots }
  end else begin
    (* Inside AGL(1,p): find smallest d >= min_d *)
    let group_d = match List.find_opt (fun d -> d >= min_d) divs with
      | Some d -> d
      | None -> p - 1  (* fallback to full AGL *)
    in
    let final_group =
      match List.find_opt (fun g -> g.Transitive_group.order = p * group_d) groups with
      | Some g -> g
      | None ->
        match List.find_opt (fun g ->
          g.Transitive_group.name = Printf.sprintf "AGL(1,%d)" p) groups with
        | Some g -> g
        | None -> List.nth groups (List.length groups - 1)
    in
    Some { group = final_group; roots }
  end
  with Z.Overflow -> None

(** {2 Public API} *)

(** Identify the Galois group of an irreducible polynomial over Q.

    For degree 5, uses the optimised sextic resolvent approach.
    For other prime degrees, uses generalized Frobenius/Chebotarev
    patterns. Returns [None] for unsupported degrees. *)
let identify f =
  let d = P.degree f in
  if d = 5 then identify_galois_group_5 f
  else if d >= 3 && is_prime_deg d then identify_galois_group_prime f
  else None

(** Convert a galois_result's group name to string. *)
let group_name gr = gr.group.Transitive_group.name
