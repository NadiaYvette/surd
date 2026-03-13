implementation module RootIsolation

import StdEnv
import Poly
import Rational
import Interval
import RootBound

// Build Sturm sequence
sturmSequence :: !(Poly Rational) -> [Poly Rational]
sturmSequence f
    # f` = diffPoly f
    = sturmLoop f f`

sturmLoop :: !(Poly Rational) !(Poly Rational) -> [Poly Rational]
sturmLoop f g
    | degree g < 0 = [f]
    # (_, r) = divModPoly f g
    = [f : sturmLoop g (negatePoly r)]

// Count sign changes in a list
signChanges :: ![Int] -> Int
signChanges xs
    # nonzero = filter (\x -> x <> 0) xs
    = countChanges nonzero

countChanges :: ![Int] -> Int
countChanges [] = 0
countChanges [_] = 0
countChanges [a, b : rest]
    | (a > 0 && b < 0) || (a < 0 && b > 0) = 1 + countChanges [b : rest]
    = countChanges [b : rest]

signOf :: !Rational -> Int
signOf r
    | r > zero = 1
    | r < zero = ~1
    = 0

// Sturm count: number of distinct real roots in (a, b]
sturmCount :: !(Poly Rational) !Rational !Rational -> Int
sturmCount f a b
    # seq = sturmSequence f
    # signsA = [signOf (evalPoly p a) \\ p <- seq]
    # signsB = [signOf (evalPoly p b) \\ p <- seq]
    = signChanges signsA - signChanges signsB

// Isolate all real roots
isolateRealRoots :: !(Poly Rational) -> [Interval]
isolateRealRoots f
    | degree f <= 0 = []
    # bound = cauchyBound f
    | bound == zero = []
    # b = bound + one
    # nRoots = sturmCount f (~ b) b
    | nRoots == 0 = []
    = bisectIsolate f (~ b) b nRoots

bisectIsolate :: !(Poly Rational) !Rational !Rational !Int -> [Interval]
bisectIsolate _ _ _ 0 = []
bisectIsolate f lo hi 1 = [mkInterval lo hi]
bisectIsolate f lo hi n
    # mid = (lo + hi) / ratFromInt 2
    # nLeft = sturmCount f lo mid
    # nRight = n - nLeft
    = bisectIsolate f lo mid nLeft ++ bisectIsolate f mid hi nRight
