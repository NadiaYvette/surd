module DemoSolvableQuintic

// Demo: solvable quintic solver.
// Clean port of SolvableQuintic.hs.

import StdEnv
import RadExpr
import Rational
import Poly
import Identify
import TransitiveGroup
import RadicalTower
import Resolvent
import Pretty
import Eval
import DAG
import Data.Integer

:: Example = { exName :: !{#Char}, exPoly :: !(Poly Rational), exNotes :: !{#Char} }

examples :: [Example]
examples =
    [ // C5 quintic: minimal polynomial of 2cos(2pi/11)
      { exName = "x^5 + x^4 - 4x^3 - 3x^2 + 3x + 1"
      , exPoly = mkPoly [one, ratFromInt 3, ratFromInt (~3), ratFromInt (~4), one, one]
      , exNotes = "C5 -- minimal polynomial of 2cos(2pi/11)"
      }
    , // F20 (Frobenius) quintic: x^5 - 2
      { exName = "x^5 - 2"
      , exPoly = mkPoly [ratFromInt (~2), zero, zero, zero, zero, one]
      , exNotes = "F20 -- Gal(Q(5th-root 2, zeta5)/Q) = Z/5 x| Z/4"
      }
    , // D5 (dihedral) quintic
      { exName = "x^5 + 20x + 32"
      , exPoly = mkPoly [ratFromInt 32, ratFromInt 20, zero, zero, zero, one]
      , exNotes = "D5 -- dihedral group of order 10"
      }
    , // S5 quintic (not solvable)
      { exName = "x^5 - 4x + 2"
      , exPoly = mkPoly [ratFromInt 2, ratFromInt (~4), zero, zero, zero, one]
      , exNotes = "S5 -- irreducible by Eisenstein (not solvable)"
      }
    , // S5 quintic (not solvable)
      { exName = "x^5 - x - 1"
      , exPoly = mkPoly [ratFromInt (~1), ratFromInt (~1), zero, zero, zero, one]
      , exNotes = "S5 (not solvable)"
      }
    ]

Start :: *World -> *World
Start world
    # (console, world) = stdio world
    # console = fwrites "=== Solvable Quintic Solver ===\n\n" console
    # console = runExamples console examples
    # (ok, world) = fclose console world
    = world

runExamples :: *File ![Example] -> *File
runExamples f [] = f
runExamples f [ex:exs]
    # f = fwrites ("--- " +++ ex.exName +++ " ---\n") f
    # f = fwrites ("  (" +++ ex.exNotes +++ ")\n") f
    # disc = discriminantOf ex.exPoly
    # discSq = isSquareRational disc
    # f = fwrites ("  disc(f) = " +++ toString disc +++ "\n") f
    # f = fwrites ("  disc is square: " +++ toString discSq +++ "\n") f
    # grResult = identifyGaloisGroup5 ex.exPoly
    # f = printGaloisResult f ex grResult
    = runExamples f exs

printGaloisResult :: *File !Example !(?(GaloisResult)) -> *File
printGaloisResult f _ ?None
    = fwrites "  (Galois group identification failed)\n\n" f
printGaloisResult f ex (?Just gr)
    # tg = gr.grGroup
    # f = fwrites ("  Galois group: " +++ tg.tgName +++ " (order " +++ toString tg.tgOrder +++ ")\n") f
    # f = fwrites ("  Solvable: " +++ toString tg.tgSolvable +++ "\n") f
    # realRoots = [fst r \\ r <- gr.grRoots | abs (snd r) < 0.000001]
    # f = fwrites ("  Real roots: " +++ toString (length realRoots) +++ "\n") f
    # f = printReals f realRoots
    | tg.tgSolvable
        # towerResult = solveViaTower gr ex.exPoly
        = printTowerResult f towerResult
    = fwrites "  (not solvable -- no radical expression exists)\n\n" f

printTowerResult :: *File !(?([RadExpr Rational])) -> *File
printTowerResult f ?None
    = fwrites "  (radical tower construction failed)\n\n" f
printTowerResult f (?Just radExprs)
    # f = fwrites "  Radical expressions:\n" f
    # realExprs = [e \\ e <- radExprs | let (_, im) = evalComplex e in abs im < 0.000001]
    # showExprs = if (isEmpty realExprs) (take 1 radExprs) realExprs
    # f = printRadExprs f showExprs
    = fwrites "\n" f

printReals :: *File ![Real] -> *File
printReals f [] = f
printReals f [r:rs]
    # f = fwrites ("    " +++ toString r +++ "\n") f
    = printReals f rs

printRadExprs :: *File ![RadExpr Rational] -> *File
printRadExprs f [] = f
printRadExprs f [e:es]
    # (re, _) = evalComplex e
    # f = fwrites ("    " +++ pretty e +++ "\n") f
    # f = fwrites ("    = " +++ toString re +++ "\n") f
    = printRadExprs f es
