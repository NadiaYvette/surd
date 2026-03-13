/// Explicit DAG (directed acyclic graph) representation for radical expressions.
///
/// Eliminates exponential tree traversal by sharing common subexpressions.
/// Nodes are stored in an association-list-based map keyed by integer IDs.
module Surd.DAG

open FStar.List.Tot
open Surd.Ring
open Surd.Rational
open Surd.Types

/// A DAG node references children by integer ID.
noeq type dag_node =
  | DLit   : rational -> dag_node
  | DNeg   : int -> dag_node
  | DAdd   : int -> int -> dag_node
  | DMul   : int -> int -> dag_node
  | DInv   : int -> dag_node
  | DRoot  : n:int{n >= 2} -> int -> dag_node
  | DPow   : int -> int -> dag_node

/// A DAG: list of (id, node) pairs with a designated root ID.
noeq type dag = {
  dag_nodes: list (int & dag_node);
  dag_root: int;
  dag_next_id: int;
}

/// Empty DAG.
let empty_dag : dag = { dag_nodes = []; dag_root = 0; dag_next_id = 0 }

/// Look up a node by ID.
let rec dag_lookup (nid: int) (nodes: list (int & dag_node)) : option dag_node =
  match nodes with
  | [] -> None
  | (k, v) :: rest -> if k = nid then Some v else dag_lookup nid rest

/// Add a node to the DAG, returning the new ID and updated DAG.
let dag_add_node (d: dag) (node: dag_node) : (int & dag) =
  let nid = d.dag_next_id in
  (nid, { dag_nodes = d.dag_nodes @ [(nid, node)];
           dag_root = nid;
           dag_next_id = nid + 1 })

/// Check if a node already exists in the DAG (structural dedup).
let rec dag_find_node (node: dag_node) (nodes: list (int & dag_node)) : option int =
  match nodes with
  | [] -> None
  | (k, DLit r) :: rest ->
    (match node with
     | DLit r' -> if rat_eq r r' then Some k else dag_find_node node rest
     | _ -> dag_find_node node rest)
  | (k, DNeg a) :: rest ->
    (match node with
     | DNeg a' -> if a = a' then Some k else dag_find_node node rest
     | _ -> dag_find_node node rest)
  | (k, DAdd a b) :: rest ->
    (match node with
     | DAdd a' b' -> if a = a' && b = b' then Some k else dag_find_node node rest
     | _ -> dag_find_node node rest)
  | (k, DMul a b) :: rest ->
    (match node with
     | DMul a' b' -> if a = a' && b = b' then Some k else dag_find_node node rest
     | _ -> dag_find_node node rest)
  | (k, DInv a) :: rest ->
    (match node with
     | DInv a' -> if a = a' then Some k else dag_find_node node rest
     | _ -> dag_find_node node rest)
  | (k, DRoot n a) :: rest ->
    (match node with
     | DRoot n' a' -> if n = n' && a = a' then Some k else dag_find_node node rest
     | _ -> dag_find_node node rest)
  | (k, DPow a n) :: rest ->
    (match node with
     | DPow a' n' -> if a = a' && n = n' then Some k else dag_find_node node rest
     | _ -> dag_find_node node rest)

/// Insert a node with deduplication.
let dag_insert (d: dag) (node: dag_node) : (int & dag) =
  match dag_find_node node d.dag_nodes with
  | Some existing_id -> (existing_id, { d with dag_root = existing_id })
  | None -> dag_add_node d node

/// Convert a rad_expr tree into a DAG with structural sharing.
val expr_to_dag : rad_expr rational -> Dv dag
let expr_to_dag e =
  let rec go (d: dag) (e: rad_expr rational) : Dv (int & dag) =
    match e with
    | Lit r -> dag_insert d (DLit r)
    | Neg a ->
      let (aid, d1) = go d a in
      dag_insert d1 (DNeg aid)
    | Add a b ->
      let (aid, d1) = go d a in
      let (bid, d2) = go d1 b in
      dag_insert d2 (DAdd aid bid)
    | Mul a b ->
      let (aid, d1) = go d a in
      let (bid, d2) = go d1 b in
      dag_insert d2 (DMul aid bid)
    | Inv a ->
      let (aid, d1) = go d a in
      dag_insert d1 (DInv aid)
    | Root n a ->
      let (aid, d1) = go d a in
      dag_insert d1 (DRoot n aid)
    | Pow a n ->
      let (aid, d1) = go d a in
      dag_insert d1 (DPow aid n)
  in
  let (root_id, d) = go empty_dag e in
  { d with dag_root = root_id }

/// Convert a DAG back to a rad_expr tree (unsharing).
val dag_to_expr : dag -> Dv (rad_expr rational)
let dag_to_expr d =
  let rec go (nid: int) (fuel: nat) : Dv (rad_expr rational) =
    if fuel = 0 then Lit rat_zero
    else
      match dag_lookup nid d.dag_nodes with
      | None -> Lit rat_zero
      | Some (DLit r) -> Lit r
      | Some (DNeg a) -> Neg (go a (fuel - 1))
      | Some (DAdd a b) -> Add (go a (fuel - 1)) (go b (fuel - 1))
      | Some (DMul a b) -> Mul (go a (fuel - 1)) (go b (fuel - 1))
      | Some (DInv a) -> Inv (go a (fuel - 1))
      | Some (DRoot n a) -> Root n (go a (fuel - 1))
      | Some (DPow a n) -> Pow (go a (fuel - 1)) n
  in
  go d.dag_root (length d.dag_nodes + 1)

/// Number of unique nodes in the DAG.
let dag_size (d: dag) : nat = length d.dag_nodes

/// Depth of the DAG from the root.
val dag_depth : dag -> Dv nat
let dag_depth d =
  let rec go (nid: int) (fuel: nat) : Dv nat =
    if fuel = 0 then 0
    else
      match dag_lookup nid d.dag_nodes with
      | None -> 0
      | Some (DLit _) -> 0
      | Some (DNeg a) -> 1 + go a (fuel - 1)
      | Some (DAdd a b) ->
        let da = go a (fuel - 1) in
        let db = go b (fuel - 1) in
        1 + (if da > db then da else db)
      | Some (DMul a b) ->
        let da = go a (fuel - 1) in
        let db = go b (fuel - 1) in
        1 + (if da > db then da else db)
      | Some (DInv a) -> 1 + go a (fuel - 1)
      | Some (DRoot _ a) -> 1 + go a (fuel - 1)
      | Some (DPow a _) -> 1 + go a (fuel - 1)
  in
  go d.dag_root (length d.dag_nodes + 1)
