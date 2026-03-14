(** Transitive subgroups of S_n for Galois group identification.

    For prime p, solvable transitive subgroups of S_p are exactly the
    affine groups Z/p ⋊ H, one per divisor d of p-1, where |H| = d.
    These are computed at runtime from the AGL(1,p) structure.

    The lattice also includes A_p and S_p (non-solvable for p >= 5). *)

type t = {
  name : string;
  degree : int;
  order : int;
  generators : Permutation.t list;
  solvable : bool;
  maximal_supergroups : int list;
  composition_factors : int list;
}

(** {2 Helpers} *)

let is_prime n =
  if n < 2 then false
  else if n < 4 then true
  else
    let rec check d =
      if d * d > n then true
      else if n mod d = 0 then false
      else check (d + if d = 2 then 1 else 2)
    in
    check 2

let factorial n =
  let rec go acc = function
    | 0 | 1 -> acc
    | k -> go (acc * k) (k - 1)
  in
  go 1 n

(** Modular exponentiation: base^exp mod m. *)
let mod_exp base exp m =
  let rec go result b e =
    if e = 0 then result
    else if e mod 2 = 1 then go ((result * b) mod m) b (e - 1)
    else go result ((b * b) mod m) (e / 2)
  in
  go 1 (base mod m) exp

(** Sorted positive divisors of n. *)
let divisors n =
  let result = ref [] in
  let d = ref 1 in
  while !d * !d <= n do
    if n mod !d = 0 then begin
      result := !d :: !result;
      if !d * !d <> n then result := (n / !d) :: !result
    end;
    incr d
  done;
  List.sort Int.compare !result

(** Primitive root modulo a prime p. *)
let primitive_root p =
  let phi = p - 1 in
  let factors =
    let pos = Positive.of_int_exn phi in
    List.map fst (Prime_factors.factorise pos)
  in
  let is_prim_root g =
    List.for_all (fun q -> mod_exp g (phi / q) p <> 1) factors
  in
  let rec find g =
    if g >= p then failwith "primitive_root: none found"
    else if is_prim_root g then g
    else find (g + 1)
  in
  find 2

(** Prime factorisation as a flat list with multiplicity. *)
let prime_factors_with_mult n =
  if n <= 1 then []
  else
    let pos = Positive.of_int_exn n in
    List.concat_map (fun (p, e) ->
      List.init e (fun _ -> p)
    ) (Prime_factors.factorise pos)

(** {2 Runtime prime group computation} *)

(** All transitive subgroups of S_p for a prime p.

    The solvable subgroups are Z/p ⋊ H for each divisor d of p-1,
    plus A_p and S_p (non-solvable for p >= 5). *)
let trans_groups_of_prime p =
  let g = primitive_root p in
  let ds = divisors (p - 1) in
  let n = p in
  (* Build one solvable group per divisor of p-1 *)
  let mk_affine d =
    let trans = Permutation.from_mapping
      (List.init p (fun i -> (i + 1) mod p)) in
    let scale_factor = mod_exp g ((p - 1) / d) p in
    let scale = Permutation.from_mapping
      (List.init p (fun i -> (scale_factor * i) mod p)) in
    let gens = if d = 1 then [trans] else [trans; scale] in
    let name =
      if d = 1 then Printf.sprintf "Z%d" p
      else if d = 2 then Printf.sprintf "D%d" p
      else if d = p - 1 then Printf.sprintf "AGL(1,%d)" p
      else Printf.sprintf "Z%d:Z%d" p d
    in
    let c_factors = prime_factors_with_mult d @ [p] in
    { name;
      degree = n;
      order = p * d;
      generators = gens;
      solvable = true;
      maximal_supergroups = [];  (* assigned later *)
      composition_factors = c_factors }
  in
  let solvable_groups = List.map mk_affine ds in
  (* A_p *)
  let ap = {
    name = Printf.sprintf "A%d" p;
    degree = n;
    order = factorial p / 2;
    generators = [
      Permutation.from_cycles n [List.init n Fun.id];
      Permutation.from_cycles n [[0; 1; 2]];
    ];
    solvable = p < 5;
    maximal_supergroups = [];
    composition_factors = [];
  } in
  (* S_p *)
  let sp = {
    name = Printf.sprintf "S%d" p;
    degree = n;
    order = factorial p;
    generators = [
      Permutation.from_cycles n [List.init n Fun.id];
      Permutation.from_cycles n [[0; 1]];
    ];
    solvable = p < 4;
    maximal_supergroups = [];
    composition_factors = [];
  } in
  let all_groups =
    List.sort (fun a b -> Int.compare a.order b.order) (solvable_groups @ [ap; sp])
  in
  (* Assign maximal supergroups *)
  let indexed = List.mapi (fun i g -> (i, g)) all_groups in
  List.map (fun (my_idx, tg) ->
    let my_ord = tg.order in
    let cands =
      List.filter_map (fun (i, cg) ->
        if i <> my_idx && cg.order > my_ord && cg.order mod my_ord = 0 then
          Some (i, cg.order)
        else None
      ) indexed
    in
    let is_max (_, super_ord) =
      not (List.exists (fun (_, mid_ord) ->
        mid_ord > my_ord && mid_ord < super_ord &&
        super_ord mod mid_ord = 0 && mid_ord mod my_ord = 0
      ) cands)
    in
    let maximal = List.filter_map (fun (i, o) ->
      if is_max (i, o) then Some i else None
    ) cands in
    { tg with maximal_supergroups = maximal }
  ) indexed

(** {2 Public API} *)

(** All transitive subgroups of S_n (up to conjugacy) for degree n,
    sorted by increasing group order.

    For prime n, uses runtime AGL(1,n) computation.
    Returns the empty list for unsupported composite degrees. *)
let trans_groups_of_degree n =
  if n >= 3 && is_prime n then trans_groups_of_prime n
  else []

(** Find the transitive group(s) of the given degree and order. *)
let trans_group_by_order deg ord =
  List.filter (fun g -> g.order = ord) (trans_groups_of_degree deg)

(** Test whether a transitive group is solvable. *)
let is_solvable tg = tg.solvable

(** {2 Composition series} *)

(** For a solvable affine subgroup of S_p, return the composition series
    as a list of generating sets descending from G to {1}.

    The group has the form Z/p ⋊ H where H is cyclic of order d.
    The series descends through subgroups of H by removing one prime
    factor at a time. *)
let composition_series_prime tg =
  let p = tg.degree in
  let n = p in
  let g = primitive_root p in
  let d = tg.order / p in
  let d_factors = prime_factors_with_mult d in
  (* Chain of divisors: d, d/q1, d/(q1*q2), ..., 1 *)
  let d_chain =
    List.fold_left (fun acc q ->
      let last = List.hd acc in
      (last / q) :: acc
    ) [d] d_factors |> List.rev
  in
  let trans = Permutation.from_cycles n [List.init n Fun.id] in
  let mk_gens d' =
    if d' <= 1 then [trans]
    else
      let sf = mod_exp g ((p - 1) / d') p in
      let scale = Permutation.from_mapping
        (List.init p (fun i -> (sf * i) mod p)) in
      [trans; scale]
  in
  List.map mk_gens d_chain @ [[]]

(** Return the composition series for a solvable transitive group.
    Returns [None] for non-solvable groups. *)
let composition_series tg =
  if not tg.solvable then None
  else if is_prime tg.degree then
    Some (composition_series_prime tg)
  else None

(** Symmetric group S_n. *)
let symmetric n =
  let transposition i j =
    Array.init n (fun k ->
      if k = i then j
      else if k = j then i
      else k)
  in
  let gens = if n <= 1 then []
    else [
      transposition 0 1;
      Array.init n (fun i -> (i + 1) mod n)
    ] in
  { name = Printf.sprintf "S%d" n;
    degree = n;
    generators = gens;
    order = factorial n;
    solvable = n < 4;
    maximal_supergroups = [];
    composition_factors = [] }
