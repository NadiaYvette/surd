--- Demo: solving solvable polynomials of prime degree.
---
--- Demonstrates the Galois group identification pipeline for
--- prime-degree polynomials, showing which are solvable by radicals.
--- Handles degree 5, 7, 11, 13, and other primes.
module DemoSolvableQuintic where

import Rational
import Poly
import RadExpr
import Pretty (pretty)
import Identify (identifyGaloisGroup, showGaloisResult, GaloisResult(..))
import GaloisSolve (solvePoly)
import Resolvent (discriminant)
import TransitiveGroup (tgSolvable)
import Normalize (normalize)

--- Local alias.
ri :: Int -> Rational
ri = Rational.fromInt

--- Show a polynomial in a readable format.
showPolyPretty :: Poly -> String
showPolyPretty (Poly cs) = case cs of
  [] -> "0"
  _  -> joinWith " + " (reverse (zipWith showTerm [0..] cs))
  where
    showTerm :: Int -> Rational -> String
    showTerm k c
      | k == 0    = showRat c
      | k == 1    = if c == ri 1 then "x"
                    else if c == ri (negate 1) then "-x"
                    else showRat c ++ "*x"
      | otherwise = if c == ri 1 then "x^" ++ show k
                    else if c == ri (negate 1) then "-x^" ++ show k
                    else showRat c ++ "*x^" ++ show k

--- Join strings with separator.
joinWith :: String -> [String] -> String
joinWith sep xs = case xs of
  []     -> ""
  [s]    -> s
  (s:ss) -> s ++ sep ++ joinWith sep ss

--- Analyze a polynomial of any prime degree.
analyzePoly :: String -> Poly -> IO ()
analyzePoly name p = do
  putStrLn ("--- " ++ name ++ " ---")
  putStrLn ("  f(x) = " ++ showPolyPretty p)
  putStrLn ("  degree = " ++ show (degree p))
  let disc = discriminant p
  putStrLn ("  discriminant = " ++ showRat disc)
  let galois = identifyGaloisGroup p
  putStrLn ("  Galois group: " ++ showGaloisResult galois)
  case galois of
    Identified g ->
      if tgSolvable g
      then do
        putStrLn "  => Solvable by radicals!"
        case solvePoly p of
          Just roots -> do
            putStrLn ("  Found " ++ show (length roots) ++ " root expression(s):")
            mapM_ (\r -> putStrLn ("    " ++ pretty (normalize r))) roots
          Nothing ->
            putStrLn "  (radical tower construction failed)"
      else
        putStrLn "  => NOT solvable by radicals (Abel-Ruffini theorem)"
    NotSupported s ->
      putStrLn ("  => " ++ s)
  putStrLn ""

--- Main: demonstrate polynomial solving at various prime degrees.
main :: IO ()
main = do
  putStrLn "===== Solvable Polynomial Demo (all prime degrees) ====="
  putStrLn ""

  -- Degree 5 examples
  putStrLn "=== Degree 5 ==="
  putStrLn ""

  -- Example 1: x^5 - 1 (cyclotomic, Galois group C5)
  let p1 = mkPoly [ri (negate 1), ri 0, ri 0, ri 0, ri 0, ri 1]
  analyzePoly "x^5 - 1 (cyclotomic)" p1

  -- Example 2: x^5 - 2 (pure radical, Galois group F20)
  let p2 = mkPoly [ri (negate 2), ri 0, ri 0, ri 0, ri 0, ri 1]
  analyzePoly "x^5 - 2 (pure radical)" p2

  -- Example 3: x^5 - x - 1 (Galois group S5, not solvable)
  let p3 = mkPoly [ri (negate 1), ri (negate 1), ri 0, ri 0, ri 0, ri 1]
  analyzePoly "x^5 - x - 1 (Bring radical)" p3

  -- Example 4: x^5 + x^4 - 4x^3 - 3x^2 + 3x + 1 (Galois group C5)
  -- This is the minimal polynomial of 2*cos(2*pi/11)
  let p4 = mkPoly [ri 1, ri 3, ri (negate 3), ri (negate 4), ri 1, ri 1]
  analyzePoly "minpoly of 2*cos(2*pi/11)" p4

  -- Degree 7 examples
  putStrLn "=== Degree 7 ==="
  putStrLn ""

  -- x^7 - 1 (cyclotomic, Galois group Z7)
  let p5 = mkPoly [ri (negate 1), ri 0, ri 0, ri 0, ri 0, ri 0, ri 0, ri 1]
  analyzePoly "x^7 - 1 (cyclotomic)" p5

  -- x^7 - 2 (pure radical, Galois group AGL(1,7))
  let p6 = mkPoly [ri (negate 2), ri 0, ri 0, ri 0, ri 0, ri 0, ri 0, ri 1]
  analyzePoly "x^7 - 2 (pure radical)" p6

  -- x^7 - x - 1 (likely S7, not solvable)
  let p7 = mkPoly [ri (negate 1), ri (negate 1), ri 0, ri 0, ri 0, ri 0, ri 0, ri 1]
  analyzePoly "x^7 - x - 1" p7

  -- Degree 3 example
  putStrLn "=== Degree 3 ==="
  putStrLn ""

  -- x^3 - 2 (Galois group S3)
  let p8 = mkPoly [ri (negate 2), ri 0, ri 0, ri 1]
  analyzePoly "x^3 - 2" p8

  putStrLn "Done."
