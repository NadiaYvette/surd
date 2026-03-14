/-
  Surd.Galois.TransitiveGroup — Database and runtime computation of
  transitive subgroups of Sₙ.

  Transitive subgroups classify possible Galois groups of irreducible
  degree-n polynomials over Q. Degree 5 uses a hard-coded database;
  other primes p use runtime AGL(1,p) computation.

  For prime p, solvable transitive subgroups of Sₚ are Z/p ⋊ H
  where H ≤ (Z/pZ)*, one per divisor d of p−1.

  Numbering follows the Butler–McKay convention (1983).
-/
import Surd.Galois.Permutation
import Surd.PrimeFactors

namespace Surd

/-- A transitive subgroup of Sₙ, with metadata for Galois group
    identification and radical tower construction. -/
structure TransitiveGroup where
  tgName : String
  tgDegree : Nat
  tgOrder : Nat
  tgGenerators : List Perm
  tgSolvable : Bool
  tgMaximalSupergroups : List Nat
  tgCompositionFactors : List Nat
  deriving Inhabited

/-- Test whether a transitive group is solvable. -/
def TransitiveGroup.isSolvable (tg : TransitiveGroup) : Bool := tg.tgSolvable

-- ---------------------------------------------------------------------------
-- Modular arithmetic helpers
-- ---------------------------------------------------------------------------

/-- Modular exponentiation: base^exp mod m. -/
private partial def modExpNat (base exp m : Nat) : Nat :=
  go 1 (base % m) exp
where
  go (res b e : Nat) : Nat :=
    if e == 0 then res
    else
      let res' := if e % 2 == 1 then (res * b) % m else res
      let b' := (b * b) % m
      go res' b' (e / 2)

/-- Find a primitive root modulo a prime p. -/
private def primitiveRootP (p : Nat) : Nat :=
  let phi := p - 1
  let factors := primeFactors phi
  let isPrimRoot (g : Nat) : Bool :=
    factors.all fun q => modExpNat g (phi / q) p != 1
  match (List.range (p - 2)).map (· + 2) |>.find? isPrimRoot with
  | some g => g
  | none => 2  -- fallback (should not happen for primes)

/-- Sorted positive divisors of n. -/
private def divisorsOf (n : Nat) : List Nat :=
  if n == 0 then [0]
  else
    let isqrt := Float.sqrt n.toFloat |>.toUInt64.toNat
    let small := (List.range isqrt).filterMap fun d' =>
      let d := d' + 1
      if n % d == 0 then some d else none
    let all := small.flatMap fun d =>
      if d * d == n then [d] else [d, n / d]
    all.mergeSort (· ≤ ·) |>.eraseDups

/-- Prime factorisation as a flat list with multiplicity. -/
private def primeFactorsWithMult (n : Nat) : List Nat :=
  if n ≤ 1 then []
  else (factoriseNat n).flatMap fun (q, e) => List.replicate e q

/-- Factorial of n. -/
private def factorial (n : Nat) : Nat :=
  (List.range n).foldl (fun acc k => acc * (k + 1)) 1

/-- Left scan (like Haskell's scanl). -/
private def listScanl {α β : Type} (f : β → α → β) (init : β) : List α → List β
  | [] => [init]
  | x :: xs => init :: listScanl f (f init x) xs

-- ---------------------------------------------------------------------------
-- Degree 5 transitive groups (hard-coded fast path)
-- ---------------------------------------------------------------------------

private def degree5Groups : List TransitiveGroup :=
  [ -- T1: C5 = ⟨(0 1 2 3 4)⟩
    { tgName := "C5"
      tgDegree := 5
      tgOrder := 5
      tgGenerators := [fromCycles 5 [[0, 1, 2, 3, 4]]]
      tgSolvable := true
      tgMaximalSupergroups := [1]
      tgCompositionFactors := [5]
    }
  , -- T2: D5 = ⟨(0 1 2 3 4), (1 4)(2 3)⟩
    { tgName := "D5"
      tgDegree := 5
      tgOrder := 10
      tgGenerators :=
        [ fromCycles 5 [[0, 1, 2, 3, 4]]
        , fromCycles 5 [[1, 4], [2, 3]]
        ]
      tgSolvable := true
      tgMaximalSupergroups := [2]
      tgCompositionFactors := [5, 2]
    }
  , -- T3: F20 = GA(1,5) = ⟨(0 1 2 3 4), (1 2 4 3)⟩
    { tgName := "F20"
      tgDegree := 5
      tgOrder := 20
      tgGenerators :=
        [ fromCycles 5 [[0, 1, 2, 3, 4]]
        , fromCycles 5 [[1, 2, 4, 3]]
        ]
      tgSolvable := true
      tgMaximalSupergroups := [3, 4]
      tgCompositionFactors := [5, 2, 2]
    }
  , -- T4: A5 = ⟨(0 1 2 3 4), (0 1 2)⟩
    { tgName := "A5"
      tgDegree := 5
      tgOrder := 60
      tgGenerators :=
        [ fromCycles 5 [[0, 1, 2, 3, 4]]
        , fromCycles 5 [[0, 1, 2]]
        ]
      tgSolvable := false
      tgMaximalSupergroups := [4]
      tgCompositionFactors := []
    }
  , -- T5: S5 = ⟨(0 1 2 3 4), (0 1)⟩
    { tgName := "S5"
      tgDegree := 5
      tgOrder := 120
      tgGenerators :=
        [ fromCycles 5 [[0, 1, 2, 3, 4]]
        , fromCycles 5 [[0, 1]]
        ]
      tgSolvable := false
      tgMaximalSupergroups := []
      tgCompositionFactors := []
    }
  ]

-- ---------------------------------------------------------------------------
-- Runtime prime group computation
-- ---------------------------------------------------------------------------

/-- All transitive subgroups of Sₚ for a prime p, computed at runtime
    from the structure of AGL(1,p).

    The solvable subgroups are Z/p ⋊ H for each divisor d of p−1,
    plus Aₚ and Sₚ (non-solvable for p ≥ 5). -/
private def transGroupsOfPrimeRT (p : Nat) : List TransitiveGroup :=
  let g := primitiveRootP p
  let ds := divisorsOf (p - 1)
  -- Build one solvable group per divisor of p-1
  let mkAffine (d : Nat) : TransitiveGroup :=
    let trans := fromMapping ((List.range p).map fun i => (i + 1) % p)
    let scaleFactor := modExpNat g ((p - 1) / d) p
    let scale := fromMapping ((List.range p).map fun i => (scaleFactor * i) % p)
    let gens := if d == 1 then [trans] else [trans, scale]
    let gName :=
      if d == 1 then "Z" ++ toString p
      else if d == 2 then "D" ++ toString p
      else if d == p - 1 then "AGL(1," ++ toString p ++ ")"
      else "Z" ++ toString p ++ ":Z" ++ toString d
    let cFactors := primeFactorsWithMult d ++ [p]
    { tgName := gName
      tgDegree := p
      tgOrder := p * d
      tgGenerators := gens
      tgSolvable := true
      tgMaximalSupergroups := []  -- assigned below
      tgCompositionFactors := cFactors
    }
  let solvableGroups := ds.map mkAffine
  -- Aₚ
  let ap : TransitiveGroup :=
    { tgName := "A" ++ toString p
      tgDegree := p
      tgOrder := factorial p / 2
      tgGenerators :=
        [ fromCycles p [List.range p]
        , fromCycles p [[0, 1, 2]]
        ]
      tgSolvable := p < 5
      tgMaximalSupergroups := []
      tgCompositionFactors := []
    }
  -- Sₚ
  let sp : TransitiveGroup :=
    { tgName := "S" ++ toString p
      tgDegree := p
      tgOrder := factorial p
      tgGenerators :=
        [ fromCycles p [List.range p]
        , fromCycles p [[0, 1]]
        ]
      tgSolvable := p < 4
      tgMaximalSupergroups := []
      tgCompositionFactors := []
    }
  let allGroups := (solvableGroups ++ [ap, sp]).mergeSort (fun a b => a.tgOrder ≤ b.tgOrder)
  -- Assign maximal supergroups by divisibility
  let indexed := allGroups.enum
  indexed.map fun (myIdx, tg) =>
    let myOrd := tg.tgOrder
    let cands := indexed.filter fun (i, cg) =>
      i != myIdx && cg.tgOrder > myOrd && cg.tgOrder % myOrd == 0
    let isMaximal (_superIdx : Nat) (superOrd : Nat) : Bool :=
      !cands.any fun (_, midG) =>
        midG.tgOrder > myOrd && midG.tgOrder < superOrd
          && superOrd % midG.tgOrder == 0 && midG.tgOrder % myOrd == 0
    let maxSupers := cands.filterMap fun (i, cg) =>
      if isMaximal i cg.tgOrder then some i else none
    { tg with tgMaximalSupergroups := maxSupers }

-- ---------------------------------------------------------------------------
-- Composition series (private helper defined first for forward reference)
-- ---------------------------------------------------------------------------

/-- Composition series for a solvable affine subgroup of Sₚ.

    The group has the form Z/p ⋊ H where H is cyclic of order d.
    The series descends through subgroups of H by removing one prime
    factor at a time, then drops to {1}. -/
private def compositionSeriesPrime (tg : TransitiveGroup) : Option (List (List Perm)) :=
  let p := tg.tgDegree
  let g := primitiveRootP p
  let d := tg.tgOrder / p
  let dFactors := primeFactorsWithMult d
  -- Chain of divisors: d, d/q1, d/(q1*q2), ..., 1
  let dChain := listScanl (fun acc q => acc / q) d dFactors
  let trans := fromCycles p [List.range p]
  let mkGens (d' : Nat) : List Perm :=
    if d' <= 1 then [trans]
    else
      let sf := modExpNat g ((p - 1) / d') p
      let scale := fromMapping ((List.range p).map fun i => (sf * i) % p)
      [trans, scale]
  some (dChain.map mkGens ++ [[]])

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

/-- All transitive subgroups of Sₙ (up to conjugacy) for the given
    degree n, sorted by increasing group order.

    For degree 5, uses the hard-coded database. For other primes,
    computes groups at runtime from AGL(1,p). Returns [] for
    unsupported composite degrees. -/
def transGroupsOfDegree (n : Nat) : List TransitiveGroup :=
  if n == 5 then degree5Groups
  else if n >= 3 && isPrime n then transGroupsOfPrimeRT n
  else []

/-- Find transitive group(s) of the given degree and order. -/
def transGroupByOrder (deg : Nat) (ord : Nat) : List TransitiveGroup :=
  (transGroupsOfDegree deg).filter (·.tgOrder == ord)

-- ---------------------------------------------------------------------------
-- Composition series (public)
-- ---------------------------------------------------------------------------

/-- For a solvable transitive group, return the composition series as a
    list of generating sets, descending from G to {1}.

    For degree-5, uses hard-coded series. For prime degree, computes
    from the affine structure. -/
def compositionSeries (tg : TransitiveGroup) : Option (List (List Perm)) :=
  if !tg.tgSolvable then none
  else match tg.tgName with
    -- Fast path: degree-5 hard-coded
    | "C5" => some [tg.tgGenerators, []]
    | "D5" => some
        [ tg.tgGenerators
        , [fromCycles 5 [[0, 1, 2, 3, 4]]]
        , []
        ]
    | "F20" => some
        [ tg.tgGenerators
        , [ fromCycles 5 [[0, 1, 2, 3, 4]]
          , fromCycles 5 [[1, 4], [2, 3]]
          ]
        , [fromCycles 5 [[0, 1, 2, 3, 4]]]
        , []
        ]
    -- General prime-degree affine groups
    | _ =>
        if isPrime tg.tgDegree then compositionSeriesPrime tg
        else none

end Surd
