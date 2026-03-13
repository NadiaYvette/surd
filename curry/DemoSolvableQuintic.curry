--- Demo: solving solvable quintic polynomials.
---
--- Demonstrates the Galois group identification pipeline for
--- degree-5 polynomials, showing which are solvable by radicals.
module DemoSolvableQuintic where

import Rational
import Poly
import RadExpr
import Pretty (pretty)
import Identify (identifyGaloisGroup5, showGaloisResult, GaloisResult(..))
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

--- Analyze a quintic polynomial.
analyzeQuintic :: String -> Poly -> IO ()
analyzeQuintic name p = do
  putStrLn ("--- " ++ name ++ " ---")
  putStrLn ("  f(x) = " ++ showPolyPretty p)
  putStrLn ("  degree = " ++ show (degree p))
  let disc = discriminant p
  putStrLn ("  discriminant = " ++ showRat disc)
  let galois = identifyGaloisGroup5 p
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
            putStrLn "  (radical tower construction pending)"
      else
        putStrLn "  => NOT solvable by radicals (Abel-Ruffini theorem)"
    NotSupported s ->
      putStrLn ("  => " ++ s)
  putStrLn ""

--- Main: demonstrate quintic solving.
main :: IO ()
main = do
  putStrLn "===== Solvable Quintic Demo ====="
  putStrLn ""

  -- Example 1: x^5 - 1 (cyclotomic, Galois group C5)
  let p1 = mkPoly [ri (negate 1), ri 0, ri 0, ri 0, ri 0, ri 1]
  analyzeQuintic "x^5 - 1 (cyclotomic)" p1

  -- Example 2: x^5 - 2 (pure radical, Galois group F20)
  let p2 = mkPoly [ri (negate 2), ri 0, ri 0, ri 0, ri 0, ri 1]
  analyzeQuintic "x^5 - 2 (pure radical)" p2

  -- Example 3: x^5 - x - 1 (Galois group S5, not solvable)
  let p3 = mkPoly [ri (negate 1), ri (negate 1), ri 0, ri 0, ri 0, ri 1]
  analyzeQuintic "x^5 - x - 1 (Bring radical)" p3

  -- Example 4: x^5 + x^4 - 4x^3 - 3x^2 + 3x + 1 (Galois group C5)
  -- This is the minimal polynomial of 2*cos(2*pi/11)
  let p4 = mkPoly [ri 1, ri 3, ri (negate 3), ri (negate 4), ri 1, ri 1]
  analyzeQuintic "minpoly of 2*cos(2*pi/11)" p4

  putStrLn "Done."
