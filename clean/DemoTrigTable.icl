module DemoTrigTable

// Demo: generate a table of exact trigonometric values.
// Clean port of surd-trig-table.

import StdEnv
import RadExpr
import Rational
import Trig
import Pretty
import LaTeX
import Normalize
import Data.Integer

Start :: *World -> *World
Start world
    # (console, world) = stdio world
    # console = fwrites "=== Exact Trigonometric Values ===\n\n" console
    // Table header
    # console = fwrites (padRight 12 "angle") console
    # console = fwrites (padRight 60 "cos") console
    # console = fwrites "sin\n" console
    # console = fwrites (repeatChar 132 '-') console
    # console = fwrites "\n" console
    // Compute for denominators 1 through 12
    # console = printDenoms console [1..12]
    # (ok, world) = fclose console world
    = world

printDenoms :: *File ![Int] -> *File
printDenoms f [] = f
printDenoms f [q:qs]
    # f = printAngles f q [0..q]
    # f = fwrites "\n" f
    = printDenoms f qs

printAngles :: *File !Int ![Int] -> *File
printAngles f _ [] = f
printAngles f q [p:ps]
    # angleStr = if (p == 0) "0"
                 (if (q == 1) (showCoeff p +++ "pi")
                 (if (p == 1) ("pi/" +++ toString q)
                 (toString p +++ "pi/" +++ toString q)))
    # cosResult = cosExact p q
    # sinResult = sinExact p q
    # cosStr = renderResult cosResult
    # sinStr = renderResult sinResult
    # f = fwrites (padRight 12 angleStr) f
    # f = fwrites (padRight 60 cosStr) f
    # f = fwrites sinStr f
    # f = fwrites "\n" f
    = printAngles f q ps

showCoeff :: !Int -> {#Char}
showCoeff 1 = ""
showCoeff n
    | n == ~1 = "-"
    = toString n

renderResult :: !TrigResult -> {#Char}
renderResult (Radical e) = pretty (normalize e)
renderResult (MinPoly _) = "minpoly (no radical form)"

padRight :: !Int !{#Char} -> {#Char}
padRight n s
    # slen = size s
    | slen >= n = s
    = s +++ {' ' \\ _ <- [1 .. n - slen]}

repeatChar :: !Int !Char -> {#Char}
repeatChar n c = {c \\ _ <- [1..n]}
