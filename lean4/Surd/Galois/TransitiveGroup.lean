/-
  Surd.Galois.TransitiveGroup — Database of transitive subgroups of Sₙ
  for small n.

  Transitive subgroups classify possible Galois groups of irreducible
  degree-n polynomials over Q. Currently covers degree 5
  (C₅, D₅, F₂₀, A₅, S₅).

  Numbering follows the Butler–McKay convention (1983).
-/
import Surd.Galois.Permutation

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
def isSolvable (tg : TransitiveGroup) : Bool := tg.tgSolvable

/-- All transitive subgroups of Sₙ (up to conjugacy), sorted by order.
    Currently degree 5 only. -/
def transGroupsOfDegree : Nat → List TransitiveGroup
  | 5 => degree5Groups
  | _ => []
where
  degree5Groups : List TransitiveGroup :=
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

/-- Find transitive group(s) of the given degree and order. -/
def transGroupByOrder (deg : Nat) (ord : Nat) : List TransitiveGroup :=
  (transGroupsOfDegree deg).filter (·.tgOrder == ord)

/-- For a solvable transitive group, return the composition series as a
    list of generating sets, descending from G to {1}. -/
def compositionSeries (tg : TransitiveGroup) : Option (List (List Perm)) :=
  if !tg.tgSolvable then none
  else match tg.tgName with
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
    | _ => none

end Surd
