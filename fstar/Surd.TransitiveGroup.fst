/// Transitive permutation groups and their identification.
module Surd.TransitiveGroup

open FStar.List.Tot
open Surd.Permutation

/// Known transitive groups by degree and standard numbering.
noeq type transitive_group_id =
  | TG_Cn   : nat -> transitive_group_id
  | TG_Dn   : nat -> transitive_group_id
  | TG_Sn   : nat -> transitive_group_id
  | TG_An   : nat -> transitive_group_id
  | TG_F20  : transitive_group_id
  | TG_V4   : transitive_group_id
  | TG_Other : nat -> nat -> transitive_group_id

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

/// Check if a transitive group is solvable.
let is_solvable (tg: transitive_group_id) : bool =
  match tg with
  | TG_Cn _ -> true
  | TG_Dn _ -> true
  | TG_Sn n -> n <= 4
  | TG_An n -> n <= 4
  | TG_F20 -> true
  | TG_V4 -> true
  | TG_Other _ ord -> ord <= 24
