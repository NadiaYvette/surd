module DemoEulerIntegral

// Demo: Euler substitution integrals.
// Clean port of EulerIntegral.hs.

import StdEnv
import RadExpr
import Rational
import Poly
import Euler
import Data.Integer

:: Example = { exName :: !{#Char}, exIntgd :: !EulerIntegrand, exKnown :: !{#Char} }

examples :: [Example]
examples =
    [ { exName = "integral dx / sqrt(x^2+1)"
      , exIntgd = { eiP = constPoly one, eiQ = constPoly one, eiSqrtPow = ~1
                   , eiA = one, eiB = zero, eiC = one }
      , exKnown = "ln|x + sqrt(x^2+1)|"
      }
    , { exName = "integral dx / sqrt(x^2-1)"
      , exIntgd = { eiP = constPoly one, eiQ = constPoly one, eiSqrtPow = ~1
                   , eiA = one, eiB = zero, eiC = ~ one }
      , exKnown = "ln|x + sqrt(x^2-1)|"
      }
    , { exName = "integral dx / sqrt(x^2+2x+2)"
      , exIntgd = { eiP = constPoly one, eiQ = constPoly one, eiSqrtPow = ~1
                   , eiA = one, eiB = ratFromInt 2, eiC = ratFromInt 2 }
      , exKnown = "ln|x + 1 + sqrt(x^2+2x+2)|"
      }
    , { exName = "integral x dx / sqrt(x^2+1)"
      , exIntgd = { eiP = monoX, eiQ = constPoly one, eiSqrtPow = ~1
                   , eiA = one, eiB = zero, eiC = one }
      , exKnown = "sqrt(x^2+1)"
      }
    , { exName = "integral dx / sqrt(4x^2+1)"
      , exIntgd = { eiP = constPoly one, eiQ = constPoly one, eiSqrtPow = ~1
                   , eiA = ratFromInt 4, eiB = zero, eiC = one }
      , exKnown = "(1/2)*ln|2x + sqrt(4x^2+1)|"
      }
    , { exName = "integral sqrt(x^2+1) dx"
      , exIntgd = { eiP = constPoly one, eiQ = constPoly one, eiSqrtPow = 1
                   , eiA = one, eiB = zero, eiC = one }
      , exKnown = "(x*sqrt(x^2+1) + ln|x + sqrt(x^2+1)|) / 2"
      }
    , { exName = "integral dx / sqrt(1-x^2)  [Euler 2]"
      , exIntgd = { eiP = constPoly one, eiQ = constPoly one, eiSqrtPow = ~1
                   , eiA = ~ one, eiB = zero, eiC = one }
      , exKnown = "arcsin(x)"
      }
    , { exName = "integral dx / sqrt(x^2-3x+2)  [Euler 3]"
      , exIntgd = { eiP = constPoly one, eiQ = constPoly one, eiSqrtPow = ~1
                   , eiA = one, eiB = ratFromInt (~3), eiC = ratFromInt 2 }
      , exKnown = "ln|x - 3/2 + sqrt(x^2-3x+2)|"
      }
    ]

Start :: *World -> *World
Start world
    # (console, world) = stdio world
    # console = fwrites "=== Euler Substitution Integrals ===\n\n" console
    # console = printExamples console examples
    # (ok, world) = fclose console world
    = world

printExamples :: *File ![Example] -> *File
printExamples f [] = f
printExamples f [ex:exs]
    # f = fwrites ("--- " +++ ex.exName +++ " ---\n") f
    # result = eulerIntegrate ex.exIntgd
    # f = printResult f ex result
    = printExamples f exs

printResult :: *File !Example !(?(IntegralResult)) -> *File
printResult f _ ?None
    = fwrites "  (no rational Euler substitution found)\n\n" f
printResult f ex (?Just result)
    # f = fwrites ("  = " +++ prettySymExpr result.irExpr +++ "\n") f
    = fwrites ("  Known: " +++ ex.exKnown +++ "\n\n") f
