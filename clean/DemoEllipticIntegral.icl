module DemoEllipticIntegral

// Demo: elliptic integral reduction to Legendre normal forms.
// Clean port of EllipticIntegral.hs.

import StdEnv
import RadExpr
import Rational
import Poly
import Elliptic
import Data.Integer

:: Example = { exName :: !{#Char}, exIntgd :: !EllipticIntegrand, exNotes :: !{#Char} }

examples :: [Example]
examples =
    [ { exName = "integral dx / sqrt(x^3 - x)"
      , exIntgd = { eiNum = constPoly one, eiDen = constPoly one
                   , eiRadicand = mkPoly [zero, ~ one, zero, one] }
      , exNotes = "roots 1, 0, -1; k^2 = 1/2"
      }
    , { exName = "integral dx / sqrt(x^3 - 1)"
      , exIntgd = { eiNum = constPoly one, eiDen = constPoly one
                   , eiRadicand = mkPoly [~ one, zero, zero, one] }
      , exNotes = "one rational root at 1, two complex"
      }
    , { exName = "integral dx / sqrt((1-x^2)(1-x^2/2))"
      , exIntgd = { eiNum = constPoly one, eiDen = constPoly one
                   , eiRadicand = mkPoly [one, zero, mkRational (toInteger (~3)) (toInteger 2),
                                          zero, mkRational (toInteger 1) (toInteger 2)] }
      , exNotes = "roots +/-1, +/-sqrt(2); k^2 = 1/2"
      }
    , { exName = "integral dx / sqrt(4x^3 - 4x)"
      , exIntgd = { eiNum = constPoly one, eiDen = constPoly one
                   , eiRadicand = mkPoly [zero, ratFromInt (~4), zero, ratFromInt 4] }
      , exNotes = "same roots as #1, leading coeff 4; k^2 = 1/2"
      }
    , { exName = "integral dx / sqrt((x^2-1)(x^2-4))"
      , exIntgd = { eiNum = constPoly one, eiDen = constPoly one
                   , eiRadicand = mkPoly [ratFromInt 4, zero, ratFromInt (~5), zero, one] }
      , exNotes = "roots 2, 1, -1, -2; quartic"
      }
    , { exName = "integral dx / sqrt(x^3 - 7x + 6)"
      , exIntgd = { eiNum = constPoly one, eiDen = constPoly one
                   , eiRadicand = mkPoly [ratFromInt 6, ratFromInt (~7), zero, one] }
      , exNotes = "roots 2, 1, -3; factors (x-1)(x-2)(x+3)"
      }
    ]

Start :: *World -> *World
Start world
    # (console, world) = stdio world
    # console = fwrites "=== Legendre Form ===\n\n" console
    # console = printExamples console False examples
    # (ok, world) = fclose console world
    = world

printExamples :: *File !Bool ![Example] -> *File
printExamples f _ [] = f
printExamples f jacobi [ex:exs]
    # f = fwrites ("--- " +++ ex.exName +++ " ---\n") f
    # f = fwrites ("  (" +++ ex.exNotes +++ ")\n") f
    # result = reduceElliptic jacobi ex.exIntgd
    # f = printEllResult f result
    = printExamples f jacobi exs

printEllResult :: *File !(?(EllipticResult)) -> *File
printEllResult f ?None = fwrites "  (cannot reduce: not all roots real, or degree unsupported)\n\n" f
printEllResult f (?Just result)
    # f = fwrites (prettyEllipticResult result) f
    = fwrites "\n" f
