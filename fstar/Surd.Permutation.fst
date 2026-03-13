/// Permutations on finite sets {0, ..., n-1}.
///
/// Represented as lists of length n where perm[i] = sigma(i).
module Surd.Permutation

open FStar.List.Tot

/// A permutation on {0, ..., n-1}.
type perm = list int

/// The identity permutation of degree n.
let id_perm (n: nat) : perm =
  let rec go (k: nat) : Tot (list int) (decreases (n - k)) =
    if k >= n then []
    else k :: go (k + 1)
  in
  go 0

/// Apply a permutation to an element.
let apply_perm (sigma: perm) (i: int) : int =
  let n = length sigma in
  if i >= 0 && i < n then
    match nth sigma i with
    | Some v -> v
    | None -> i
  else i

/// Compose two permutations: (sigma . tau)(i) = sigma(tau(i)).
let compose_perm (sigma tau: perm) : perm =
  map (fun i -> apply_perm sigma (apply_perm tau i)) (id_perm (length sigma))

/// Inverse of a permutation.
let inverse_perm (sigma: perm) : perm =
  let n = length sigma in
  let rec go (k: nat) : Tot (list int) (decreases (n - k)) =
    if k >= n then []
    else
      let rec find (j: nat) : Tot int (decreases (n - j)) =
        if j >= n then k
        else if apply_perm sigma j = k then j
        else find (j + 1)
      in
      find 0 :: go (k + 1)
  in
  go 0

/// Order of a permutation.
val perm_order : perm -> Dv nat
let perm_order sigma =
  let n = length sigma in
  let id = id_perm n in
  let rec go (current: perm) (k: nat) : Dv nat =
    if k > 0 && current = id then k
    else if k > op_Multiply n n then k
    else go (compose_perm sigma current) (k + 1)
  in
  go sigma 1

/// Cycle type: sorted list of cycle lengths.
val cycle_type : perm -> Dv (list nat)
let cycle_type sigma =
  let n = length sigma in
  let rec find_cycles (visited: list int) (start: nat) : Dv (list nat) =
    if start >= n then []
    else if mem start visited then find_cycles visited (start + 1)
    else
      let rec follow (i: int) (len: nat) (vis: list int) : Dv (nat & list int) =
        let next = apply_perm sigma i in
        if next = start then (len, vis)
        else follow next (len + 1) (next :: vis)
      in
      let (len, vis) = follow start 1 (start :: visited) in
      len :: find_cycles vis (start + 1)
  in
  let cycles = find_cycles [] 0 in
  sortWith (fun (a:nat) (b:nat) -> a - b) cycles

/// Sign of a permutation.
val perm_sign : perm -> Dv int
let perm_sign sigma =
  let ct = cycle_type sigma in
  let n_even_cycles = length (filter (fun (n:nat) -> n % 2 = 0) ct) in
  if n_even_cycles % 2 = 0 then 1 else 0 - 1

/// Check if a permutation is the identity.
let is_identity (sigma: perm) : bool =
  sigma = id_perm (length sigma)
