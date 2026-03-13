/-
  Surd.Polynomial.MinimalPolyTower — Fast minimal polynomial computation via
  extension tower.

  Instead of computing resultants bottom-up at every AST node (which
  causes exponential blowup when radicals are shared), this module:

  1. Collects distinct radicals in the expression
  2. Builds an extension tower Q(α₁)(α₂)...
  3. Evaluates the expression in the tower
  4. Computes the annihilating polynomial via iterated norms
     (one resultant per tower level)
  5. Factors over Q to get the minimal polynomial

  TODO: Full implementation requires ExtField/ExtElem infrastructure
  to be ported. Currently provides collectRadicals and the tower-based
  entry point (which falls back to annihilatingPoly for now).
-/
import Surd.Poly.Univariate
import Surd.Poly.Resultant
import Surd.Radical.Expr
import Surd.Radical.Eval
import Surd.Radical.Normalize
import Surd.Polynomial.MinimalPoly
import Surd.Rat
import Std.Internal.Rat

open Std.Internal
open Surd.Poly

namespace Surd

-- ---------------------------------------------------------------------------
-- Collect radicals
-- ---------------------------------------------------------------------------

/-- Collect all distinct radical subexpressions, ordered leaves-first.
    Each entry is (root degree, radicand expression).
    Uses structural equality to identify shared radicals.
    Radicands are normalized to improve sharing. -/
partial def collectRadicals : RadExpr Rat → List (Int × RadExpr Rat)
  | .lit _ => []
  | .neg a => collectRadicals a
  | .add a b => dedup (collectRadicals a ++ collectRadicals b)
  | .mul a b => dedup (collectRadicals a ++ collectRadicals b)
  | .inv a => collectRadicals a
  | .pow a _ => collectRadicals a
  | .root n a =>
    let inner := collectRadicals a
    dedup (inner ++ [(n, a)])
where
  dedup : List (Int × RadExpr Rat) → List (Int × RadExpr Rat)
    | [] => []
    | x :: xs =>
      if xs.any (fun y => y.1 == x.1 && y.2 == x.2) then dedup xs
      else x :: dedup xs

/-- Check if all Root subexpressions in e match one of the resolved radicals. -/
private def allRootsResolved (resolved : List (Int × RadExpr Rat))
    : RadExpr Rat → Bool
  | .lit _ => true
  | .neg a => allRootsResolved resolved a
  | .add a b => allRootsResolved resolved a && allRootsResolved resolved b
  | .mul a b => allRootsResolved resolved a && allRootsResolved resolved b
  | .inv a => allRootsResolved resolved a
  | .pow a _ => allRootsResolved resolved a
  | .root n a =>
    resolved.any (fun (n', r) => n == n' && r == a) && allRootsResolved resolved a

/-- Try to evaluate a RadExpr as a pure rational (no radicals). -/
private def evalRational : RadExpr Rat → Option Rat
  | .lit r => some r
  | .neg a => do let v ← evalRational a; some (-v)
  | .add a b => do let va ← evalRational a; let vb ← evalRational b; some (va + vb)
  | .mul a b => do let va ← evalRational a; let vb ← evalRational b; some (va * vb)
  | .inv a => do let v ← evalRational a; if v != 0 then some (1 / v) else none
  | .pow a n => do
    let v ← evalRational a
    if n ≥ 0 then some (ratPow v n.toNat)
    else if v != 0 then some (1 / ratPow v (-n).toNat)
    else none
  | .root _ _ => none
where
  ratPow (r : Rat) : Nat → Rat
    | 0 => 1
    | 1 => r
    | n + 1 => r * ratPow r n

-- ---------------------------------------------------------------------------
-- Topological sort for dependency ordering
-- ---------------------------------------------------------------------------

/-- Sort radicals so dependencies come first (leaves-first ordering). -/
private partial def topoSortGo (resolved remaining : List (Int × RadExpr Rat))
    : List (Int × RadExpr Rat) :=
  match remaining with
  | [] => resolved
  | _ =>
    let (ready, notReady) := remaining.partition fun (_, radicand) =>
      allRootsResolved resolved radicand
    match ready with
    | [] => resolved ++ remaining
    | _ => topoSortGo (resolved ++ ready) notReady

private def topoSortRadicals (radicals : List (Int × RadExpr Rat))
    : List (Int × RadExpr Rat) :=
  topoSortGo [] radicals

/-- Build dependency chain: order radicals so that each radical's
    radicand only uses radicals earlier in the chain.
    Drops unresolvable radicals. -/
private def buildChain (radicals : List (Int × RadExpr Rat))
    : List (Int × RadExpr Rat) :=
  dropUnresolved [] (topoSortRadicals radicals)
where
  dropUnresolved (resolved : List (Int × RadExpr Rat))
      : List (Int × RadExpr Rat) → List (Int × RadExpr Rat)
    | [] => resolved
    | r :: rs =>
      if allRootsResolved resolved r.2 then
        dropUnresolved (resolved ++ [r]) rs
      else resolved

-- ---------------------------------------------------------------------------
-- Simple (non-tower) annihilating polynomial
-- ---------------------------------------------------------------------------

/-- Simple annihilating polynomial (non-tower, for fallback).
    Same as annihilatingPoly but defined locally to avoid import issues. -/
private partial def simpleAnnihilating : RadExpr Rat → Poly Rat
  | .lit r => Poly.mkPoly #[-r, 1]
  | .neg e => Poly.negateVar (simpleAnnihilating e)
  | .add a b => Poly.composedSum (simpleAnnihilating a) (simpleAnnihilating b)
  | .mul a b => Poly.composedProduct (simpleAnnihilating a) (simpleAnnihilating b)
  | .inv e => Poly.reciprocalPoly (simpleAnnihilating e)
  | .root n (.lit r) =>
    if n ≤ 0 then Poly.mkPoly #[-r, 1]
    else
      let nNat := n.toNat
      Poly.mkPoly (#[-r] ++ Array.mkArray (nNat - 1) (0 : Rat) ++ #[1])
  | .root n e =>
    if n ≤ 0 then simpleAnnihilating e
    else Poly.substituteXN n.toNat (simpleAnnihilating e)
  | .pow e n =>
    if n ≥ 0 then annihPow (simpleAnnihilating e) n.toNat
    else annihPow (Poly.reciprocalPoly (simpleAnnihilating e)) (-n).toNat

where
  /-- Annihilating polynomial for e^n via resultant interpolation. -/
  annihPow (p : Poly Rat) : Nat → Poly Rat
    | 0 => Poly.mkPoly #[-1, 1]
    | 1 => p
    | n =>
      let dp := match p.degree with | some d => d | none => 0
      let points := (List.range (dp + 1)).map fun i =>
        let x0 : Rat := (Int.ofNat i : Int)
        let ynMinusX0 : Poly Rat :=
          Poly.mkPoly (#[-x0] ++ Array.mkArray (n - 1) (0 : Rat) ++ #[1])
        (x0, Poly.resultant p ynMinusX0)
      Poly.lagrangeInterpolate points

-- ---------------------------------------------------------------------------
-- Tower-based computation (stub)
-- ---------------------------------------------------------------------------

/-- Compute annihilating polynomial via extension tower.
    TODO: requires ExtField/ExtElem infrastructure to be ported.
    Currently falls back to simpleAnnihilating. -/
private partial def computeViaTower (expr : RadExpr Rat)
    (_radicals : List (Int × RadExpr Rat)) : Poly Rat :=
  -- TODO: when ExtField/ExtElem are ported, this will build the
  -- tower Q(α₁)(α₂)..., evaluate the expression, and compute
  -- iterated norms via resultant at each level.
  simpleAnnihilating expr

-- ---------------------------------------------------------------------------
-- Stub for square-free factorization
-- ---------------------------------------------------------------------------

private def factorSquareFreeTower (p : Poly Rat) : List (Poly Rat) :=
  let sf := Poly.monic (Poly.squareFree p)
  if sf.isZero then [] else [sf]

-- ---------------------------------------------------------------------------
-- Pick closest factor
-- ---------------------------------------------------------------------------

private def pickClosestTower (target : Float) (factors : List (Poly Rat))
    : Poly Rat :=
  match factors with
  | [] => Poly.zero
  | [f] => f
  | f :: fs =>
    let approxRoot (p : Poly Rat) : Float :=
      let eval (x : Float) : Float :=
        p.coeffs.foldr (fun c acc => ratToFloat c + x * acc) 0.0
      let evalDeriv (x : Float) : Float :=
        let dp := Poly.diff p
        dp.coeffs.foldr (fun c acc => ratToFloat c + x * acc) 0.0
      let rec go (x : Float) : Nat → Float
        | 0 => x
        | n + 1 =>
          let fx := eval x
          let dfx := evalDeriv x
          if Float.abs dfx < 1.0e-15 then x
          else go (x - fx / dfx) n
      go target 20
    let dist (p : Poly Rat) : Float :=
      Float.abs (approxRoot p - target)
    fs.foldl (fun best p => if dist p < dist best then p else best) f

-- ---------------------------------------------------------------------------
-- Public interface
-- ---------------------------------------------------------------------------

/-- Compute an annihilating polynomial (not necessarily minimal) via
    the extension tower approach.
    Falls back to simple (per-node) approach when tower is not available. -/
partial def annihilatingPolyTower (expr : RadExpr Rat) : Poly Rat :=
  let expr' := normalize expr
  let radicals := collectRadicals expr'
  match radicals with
  | [] =>
    match evalRational expr' with
    | some r => Poly.mkPoly #[-r, 1]
    | none => simpleAnnihilating expr'
  | _ => computeViaTower expr' radicals

/-- Compute the minimal polynomial of a radical expression over Q,
    using the extension tower approach.
    TODO: implement tower-based computation when ExtField is ported.
    Currently falls back to per-node annihilating polynomial. -/
partial def minimalPolyTower (expr : RadExpr Rat) : Poly Rat :=
  let ann := annihilatingPolyTower expr
  let v := evalFloat expr
  let factors := factorSquareFreeTower (Poly.monic ann)
  match factors with
  | [] => ann
  | _ => Poly.monic (pickClosestTower v factors)

end Surd
