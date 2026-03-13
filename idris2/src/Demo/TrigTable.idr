module Demo.TrigTable

import Surd.Rational
import Surd.Types
import Surd.Trig
import Surd.Pretty
import Surd.LaTeX
import Surd.Eval

import Data.List

%default covering

------------------------------------------------------------------------
-- Trig table generation
------------------------------------------------------------------------

||| Format a fraction p/q as a string.
frac : Integer -> Integer -> String
frac p q =
  let g = gcdI (abs p) (abs q)
      p' = div p g
      q' = div q g
  in if q' == 1 then show p'
     else show p' ++ "/" ++ show q'
  where
    gcdI : Integer -> Integer -> Integer
    gcdI a 0 = a
    gcdI a b = assert_total $ gcdI b (mod a b)

||| Generate one row of the trig table.
trigRow : Integer -> Integer -> String
trigRow p q =
  let angle = frac p q ++ "pi"
      cosR = cosExact p q
      sinR = sinExact p q
      cosStr = case cosR of
                 Radical e => pretty e
                 MinPoly _ => "(minpoly)"
      sinStr = case sinR of
                 Radical e => pretty e
                 MinPoly _ => "(minpoly)"
      cosApprox = case cosR of
                    Radical e => show (eval e)
                    MinPoly _ => "?"
      sinApprox = case sinR of
                    Radical e => show (eval e)
                    MinPoly _ => "?"
  in angle ++ "  |  cos = " ++ cosStr
     ++ "  (~" ++ cosApprox ++ ")"
     ++ "  |  sin = " ++ sinStr
     ++ "  (~" ++ sinApprox ++ ")"

||| Generate a LaTeX row.
trigRowLaTeX : Integer -> Integer -> String
trigRowLaTeX p q =
  let angle = "\\frac{" ++ show p ++ "}{" ++ show q ++ "}\\pi"
      cosR = cosExact p q
      sinR = sinExact p q
      cosStr = case cosR of
                 Radical e => latex e
                 MinPoly _ => "\\text{(minpoly)}"
      sinStr = case sinR of
                 Radical e => latex e
                 MinPoly _ => "\\text{(minpoly)}"
  in "  " ++ angle ++ " & $" ++ cosStr ++ "$ & $" ++ sinStr ++ "$ \\\\"

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

||| Standard angles: multiples of pi/12 from 0 to pi/2.
standardAngles : List (Integer, Integer)
standardAngles =
  [ (0, 1), (1, 12), (1, 6), (1, 4), (1, 3), (5, 12), (1, 2) ]

||| Additional interesting angles.
interestingAngles : List (Integer, Integer)
interestingAngles =
  [ (1, 5), (2, 5), (1, 8), (3, 8), (1, 10), (3, 10) ]

export
main : IO ()
main = do
  putStrLn "=== Exact Trigonometric Values ==="
  putStrLn ""
  putStrLn "--- Standard Angles ---"
  traverse_ (\pq => putStrLn (trigRow (fst pq) (snd pq))) standardAngles
  putStrLn ""
  putStrLn "--- Additional Angles ---"
  traverse_ (\pq => putStrLn (trigRow (fst pq) (snd pq))) interestingAngles
  putStrLn ""
  putStrLn "--- LaTeX Table ---"
  putStrLn "\\begin{tabular}{lll}"
  putStrLn "  \\hline"
  putStrLn "  Angle & $\\cos$ & $\\sin$ \\\\"
  putStrLn "  \\hline"
  traverse_ (\pq => putStrLn (trigRowLaTeX (fst pq) (snd pq)))
            (standardAngles ++ interestingAngles)
  putStrLn "  \\hline"
  putStrLn "\\end{tabular}"
