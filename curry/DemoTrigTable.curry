--- Demo: trig table showing exact cos and sin values at
--- rational multiples of pi.
module DemoTrigTable where

import Rational
import RadExpr
import Pretty (pretty)
import Trig (cosExact, sinExact, TrigResult(..))
import Normalize (normalize)

--- Show a trig result as a string.
showTrigResult :: TrigResult -> String
showTrigResult tr = case tr of
  Radical e -> pretty (normalize e)
  MinPoly p -> "MinPoly: " ++ show p

--- Print cos(p*pi/q) and sin(p*pi/q).
printTrigRow :: Int -> Int -> IO ()
printTrigRow p q = do
  let angle = show p ++ "*pi/" ++ show q
      cosR = cosExact p q
      sinR = sinExact p q
  putStrLn ("  " ++ padRight 12 angle
            ++ padRight 40 (showTrigResult cosR)
            ++ showTrigResult sinR)

--- Pad a string to the right.
padRight :: Int -> String -> String
padRight n s = s ++ replicate (max 0 (n - length s)) ' '

--- Main: print a table of trig values.
main :: IO ()
main = do
  putStrLn "===== Exact Trigonometric Values ====="
  putStrLn ""
  putStrLn ("  " ++ padRight 12 "angle"
            ++ padRight 40 "cos" ++ "sin")
  putStrLn (replicate 92 '-')
  -- Standard angles
  printTrigRow 0 1
  printTrigRow 1 6
  printTrigRow 1 4
  printTrigRow 1 3
  printTrigRow 1 2
  printTrigRow 2 3
  printTrigRow 3 4
  printTrigRow 5 6
  printTrigRow 1 1
  putStrLn ""
  putStrLn "--- Non-constructible angles ---"
  putStrLn ""
  -- Angles requiring cube roots or higher
  printTrigRow 1 5
  printTrigRow 1 8
  printTrigRow 1 10
  printTrigRow 1 12
  putStrLn ""
  putStrLn "Done."
