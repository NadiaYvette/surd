/// Transitive permutation groups and their identification.
///
/// For prime degree p, the solvable transitive subgroups of S_p are
/// exactly the affine groups Z/p ⋊ H where H ≤ (Z/pZ)*, one per
/// divisor d of p-1.  These are computed at runtime via AGL(1,p).
///
/// For degree 5, a hard-coded fast path is retained.
module Surd.TransitiveGroup

open FStar.List.Tot
open Surd.Permutation
open Surd.PrimeFactors

/// --------------------------------------------------------------------------
/// Group identity types
/// --------------------------------------------------------------------------

/// Known transitive groups by degree and standard numbering.
noeq type transitive_group_id =
  | TG_Cn   : nat -> transitive_group_id
  | TG_Dn   : nat -> transitive_group_id
  | TG_Sn   : nat -> transitive_group_id
  | TG_An   : nat -> transitive_group_id
  | TG_F20  : transitive_group_id
  | TG_V4   : transitive_group_id
  | TG_Aff  : nat -> nat -> transitive_group_id   // Z/p ⋊ Z/d, order p*d
  | TG_Other : nat -> nat -> transitive_group_id

/// --------------------------------------------------------------------------
/// Rich transitive group record (mirrors Haskell TransitiveGroup)
/// --------------------------------------------------------------------------

/// A transitive group with metadata for Galois identification and radical
/// tower construction.
noeq type transitive_group_info = {
  tgi_name: string;
  tgi_degree: nat;
  tgi_order: int;
  tgi_generators: list perm;
  tgi_solvable: bool;
  tgi_composition_factors: list int;
}

/// --------------------------------------------------------------------------
/// Legacy transitive_group (kept for backward compat)
/// --------------------------------------------------------------------------

/// A transitive group given by generators.
noeq type transitive_group = {
  tg_degree: nat;
  tg_generators: list perm;
}

/// Check if a perm is in a list (structural equality).
let rec perm_mem (p: perm) (xs: list perm) : Tot bool (decreases xs) =
  match xs with
  | [] -> false
  | x :: rest -> x = p || perm_mem p rest

/// Compute the full group from generators by closure.
val generate_group : transitive_group -> Dv (list perm)
let generate_group tg =
  let n = tg.tg_degree in
  let id = id_perm n in
  let rec close (elements: list perm) (fuel: nat) : Dv (list perm) =
    if fuel = 0 then elements
    else
      let rec find_new (gs: list perm) (hs: list perm) : Dv (list perm) =
        match gs with
        | [] -> []
        | g :: grest ->
          let rec try_products (hs': list perm) : Dv (list perm) =
            match hs' with
            | [] -> find_new grest hs
            | h :: hrest ->
              let prod = compose_perm g h in
              if perm_mem prod elements then try_products hrest
              else prod :: try_products hrest
          in
          try_products hs
      in
      let new_elts = find_new tg.tg_generators elements in
      match new_elts with
      | [] -> elements
      | _ -> close (elements @ new_elts) (fuel - 1)
  in
  close (id :: tg.tg_generators) 100

/// Order of the group.
val group_order : transitive_group -> Dv nat
let group_order tg = length (generate_group tg)

/// Identify a transitive group by its degree and order.
val identify_group : transitive_group -> Dv transitive_group_id
let identify_group tg =
  let n = tg.tg_degree in
  let ord = group_order tg in
  if ord = n then TG_Cn n
  else if ord = op_Multiply 2 n then TG_Dn n
  else if n = 3 && ord = 6 then TG_Sn 3
  else if n = 3 && ord = 3 then TG_Cn 3
  else if n = 4 && ord = 4 then TG_V4
  else if n = 4 && ord = 12 then TG_An 4
  else if n = 4 && ord = 24 then TG_Sn 4
  else if n = 5 && ord = 20 then TG_F20
  else if n = 5 && ord = 60 then TG_An 5
  else if n = 5 && ord = 120 then TG_Sn 5
  else TG_Other n ord

/// Check if a transitive group id is solvable.
let is_solvable (tg: transitive_group_id) : bool =
  match tg with
  | TG_Cn _ -> true
  | TG_Dn _ -> true
  | TG_Sn n -> n <= 4
  | TG_An n -> n <= 4
  | TG_F20 -> true
  | TG_V4 -> true
  | TG_Aff _ _ -> true   // all affine subgroups of AGL(1,p) are solvable
  | TG_Other _ ord -> ord <= 24

/// Check if a transitive_group_info is solvable.
let is_solvable_info (tgi: transitive_group_info) : bool =
  tgi.tgi_solvable

/// --------------------------------------------------------------------------
/// Modular arithmetic helpers
/// --------------------------------------------------------------------------

/// Modular exponentiation: base^exp mod m.
val mod_exp : int -> int -> int -> Dv int
let rec mod_exp base exp m =
  if exp = 0 then 1
  else if exp = 1 then base % m
  else
    let half = mod_exp base (exp / 2) m in
    let sq = op_Multiply half half % m in
    if exp % 2 = 0 then sq
    else op_Multiply sq base % m

/// Primitive root modulo a prime p.
/// Finds the smallest g in [2..p-1] such that g has order p-1.
val primitive_root : int -> Dv int
let primitive_root p =
  let phi = p - 1 in
  let factors = prime_factors (if phi > 0 then phi else 1) in
  let is_prim_root (g: int) : Dv bool =
    let rec check_all (fs: list int) : Dv bool =
      match fs with
      | [] -> true
      | q :: rest ->
        if mod_exp g (phi / q) p = 1 then false
        else check_all rest
    in
    check_all factors
  in
  let rec find (g: int) : Dv int =
    if g >= p then 2  // fallback
    else if is_prim_root g then g
    else find (g + 1)
  in
  find 2

/// Sorted positive divisors of n.
val divisors : int -> Dv (list int)
let divisors n =
  let n' = if n >= 0 then n else 0 - n in
  if n' = 0 then [0]
  else
    let rec collect (d: pos) : Dv (list int) =
      if op_Multiply d d > n' then []
      else if n' % d = 0 then
        let q = n' / d in
        if d = q then [d] @ collect (d + 1)
        else [d; q] @ collect (d + 1)
      else collect (d + 1)
    in
    let raw = collect 1 in
    sortWith (fun (a: int) (b: int) -> a - b) raw

/// Prime factorisation as a flat list with multiplicity.
val prime_factors_with_mult : int -> Dv (list int)
let prime_factors_with_mult n =
  if n <= 1 then []
  else
    let facts = factorise (if n > 0 then n else 1) in
    let rec expand (fs: list (int & int)) : list int =
      match fs with
      | [] -> []
      | (p, e) :: rest ->
        let rec rep (k: nat) : list int =
          if k = 0 then []
          else p :: rep (k - 1)
        in
        let e' : nat = if e >= 0 then e else 0 in
        rep e' @ expand rest
    in
    expand facts

/// Factorial.
val factorial : nat -> int
let rec factorial n =
  if n <= 1 then 1
  else op_Multiply n (factorial (n - 1))

/// --------------------------------------------------------------------------
/// Runtime AGL(1,p) group computation for prime p
/// --------------------------------------------------------------------------

/// Build one solvable affine group Z/p ⋊ Z/d for a given divisor d of p-1.
val mk_affine_group : int -> int -> int -> Dv transitive_group_info
let mk_affine_group p g d =
  let n : nat = if p >= 0 then p else 0 in
  let trans : perm =
    let rec build (i: nat) : Tot (list int) (decreases (n - i)) =
      if i >= n then []
      else (i + 1) % n :: build (i + 1)
    in
    build 0
  in
  let scale_factor = mod_exp g ((p - 1) / d) p in
  let scale : perm =
    let rec build (i: nat) : Tot (list int) (decreases (n - i)) =
      if i >= n then []
      else op_Multiply scale_factor i % p :: build (i + 1)
    in
    build 0
  in
  let gens = if d = 1 then [trans] else [trans; scale] in
  let name =
    if d = 1 then "Z" ^ string_of_int p
    else if d = 2 then "D" ^ string_of_int p
    else if d = p - 1 then "AGL(1," ^ string_of_int p ^ ")"
    else "Z" ^ string_of_int p ^ ":Z" ^ string_of_int d
  in
  let comp_factors = prime_factors_with_mult d @ [p] in
  {
    tgi_name = name;
    tgi_degree = n;
    tgi_order = op_Multiply p d;
    tgi_generators = gens;
    tgi_solvable = true;
    tgi_composition_factors = comp_factors;
  }

/// All transitive subgroups of S_p for a prime p, computed at runtime.
///
/// The solvable subgroups are Z/p ⋊ H for each divisor d of p-1,
/// plus A_p and S_p (non-solvable for p >= 5).
val trans_groups_of_prime : int -> Dv (list transitive_group_info)
let trans_groups_of_prime p =
  let n : nat = if p >= 0 then p else 0 in
  let g = primitive_root p in
  let ds = divisors (p - 1) in
  let rec mk_solvable (ds': list int) : Dv (list transitive_group_info) =
    match ds' with
    | [] -> []
    | d :: rest -> mk_affine_group p g d :: mk_solvable rest
  in
  let solvable_groups = mk_solvable ds in
  let trans_perm : perm =
    let rec build (i: nat) : Tot (list int) (decreases (n - i)) =
      if i >= n then []
      else (i + 1) % n :: build (i + 1)
    in
    build 0
  in
  // A_p: generated by the n-cycle and a 3-cycle (0 1 2)
  let three_cycle : perm =
    let rec build (i: nat) : Tot (list int) (decreases (n - i)) =
      if i >= n then []
      else if i = 0 then 1 :: build (i + 1)
      else if i = 1 then 2 :: build (i + 1)
      else if i = 2 then 0 :: build (i + 1)
      else i :: build (i + 1)
    in
    build 0
  in
  let ap : transitive_group_info = {
    tgi_name = "A" ^ string_of_int p;
    tgi_degree = n;
    tgi_order = factorial n / 2;
    tgi_generators = [trans_perm; three_cycle];
    tgi_solvable = n < 5;
    tgi_composition_factors = [];
  } in
  // S_p: generated by the n-cycle and a transposition (0 1)
  let transposition : perm =
    let rec build (i: nat) : Tot (list int) (decreases (n - i)) =
      if i >= n then []
      else if i = 0 then 1 :: build (i + 1)
      else if i = 1 then 0 :: build (i + 1)
      else i :: build (i + 1)
    in
    build 0
  in
  let sp : transitive_group_info = {
    tgi_name = "S" ^ string_of_int p;
    tgi_degree = n;
    tgi_order = factorial n;
    tgi_generators = [trans_perm; transposition];
    tgi_solvable = n < 4;
    tgi_composition_factors = [];
  } in
  // Sort all groups by order
  let all_groups = solvable_groups @ [ap; sp] in
  sortWith (fun (a: transitive_group_info) (b: transitive_group_info) -> a.tgi_order - b.tgi_order) all_groups

/// Fast-path degree-5 transitive groups.
val degree_5_groups : Dv (list transitive_group_info)
let degree_5_groups =
  trans_groups_of_prime 5

/// All transitive subgroups for a given degree.
/// For prime n, computed at runtime via AGL(1,n).
/// For unsupported composite degrees, returns [].
val trans_groups_of_degree : nat -> Dv (list transitive_group_info)
let trans_groups_of_degree n =
  if n >= 3 && is_prime n then trans_groups_of_prime n
  else []

/// Find transitive groups by degree and order.
val trans_group_by_order : nat -> int -> Dv (list transitive_group_info)
let trans_group_by_order deg ord =
  let gs = trans_groups_of_degree deg in
  filter (fun (g: transitive_group_info) -> g.tgi_order = ord) gs

/// --------------------------------------------------------------------------
/// Composition series for solvable affine groups
/// --------------------------------------------------------------------------

/// For a solvable affine subgroup Z/p ⋊ H with |H| = d, return the
/// composition series as a list of generator sets, descending from G
/// to {1}.
///
/// The series descends through subgroups of H by removing one prime
/// factor at a time, then drops to {1}.
val composition_series_prime : transitive_group_info -> Dv (option (list (list perm)))
let composition_series_prime tgi =
  if not tgi.tgi_solvable then None
  else
    let p = tgi.tgi_degree in
    let p' : int = p in
    let g = primitive_root p' in
    let d = tgi.tgi_order / p' in
    let d_factors = prime_factors_with_mult (if d > 0 then d else 1) in
    // Chain of divisors: d, d/q1, d/(q1*q2), ..., 1
    let rec build_chain (cur: int) (qs: list int) : list int =
      match qs with
      | [] -> [cur]
      | q :: rest -> cur :: build_chain (cur / q) rest
    in
    let d_chain = build_chain d d_factors in
    let n : nat = p in
    let trans_perm : perm =
      let rec build (i: nat) : Tot (list int) (decreases (n - i)) =
        if i >= n then []
        else (i + 1) % n :: build (i + 1)
      in
      build 0
    in
    let mk_gens (d': int) : Dv (list perm) =
      if d' <= 1 then [trans_perm]
      else
        let sf = mod_exp g ((p' - 1) / d') p' in
        let scale : perm =
          let rec build (i: nat) : Tot (list int) (decreases (n - i)) =
            if i >= n then []
            else op_Multiply sf i % p' :: build (i + 1)
          in
          build 0
        in
        [trans_perm; scale]
    in
    let rec map_mk_gens (xs: list int) : Dv (list (list perm)) =
      match xs with
      | [] -> []
      | x :: rest -> mk_gens x :: map_mk_gens rest
    in
    let series = map_mk_gens d_chain @ [[]] in
    Some series
