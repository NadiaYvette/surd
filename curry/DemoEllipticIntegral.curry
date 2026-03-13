--- Demo: reduction of elliptic integrals to Legendre normal forms.
module DemoEllipticIntegral where

import Rational
import Poly
import EllipticIntegration

--- Local alias.
ri :: Int -> Rational
ri = Rational.fromInt

--- Main: demonstrate elliptic integral reduction.
main :: IO ()
main = do
  putStrLn "===== Elliptic Integral Reduction Demo ====="
  putStrLn ""

  -- Example 1: integral dx / sqrt(x^3 - x)
  -- Cubic radicand: x^3 - x = x(x-1)(x+1), roots at -1, 0, 1
  putStrLn "Example 1: integral dx / sqrt(x^3 - x)"
  let radicand1 = mkPoly [ri 0, ri (negate 1), ri 0, ri 1]  -- x^3 - x
      eg1 = EllipticIntegrand (constPoly (ri 1)) (constPoly (ri 1)) radicand1
  case reduceElliptic eg1 of
    Just result -> do
      putStrLn (prettyEllipticResult result)
      putStrLn ""
      putStrLn "LaTeX:"
      putStrLn (latexEllipticResult result)
    Nothing ->
      putStrLn "  Could not reduce."
  putStrLn ""

  -- Example 2: integral dx / sqrt(x^4 - 1)
  -- Quartic radicand: x^4 - 1
  putStrLn "Example 2: integral dx / sqrt(x^4 - 1)"
  let radicand2 = mkPoly [ri (negate 1), ri 0, ri 0, ri 0, ri 1]  -- x^4 - 1
      eg2 = EllipticIntegrand (constPoly (ri 1)) (constPoly (ri 1)) radicand2
  case reduceElliptic eg2 of
    Just result -> do
      putStrLn (prettyEllipticResult result)
    Nothing ->
      putStrLn "  Could not reduce."
  putStrLn ""

  -- Example 3: integral dx / sqrt(4x^3 - g2*x - g3)
  -- Weierstrass form with g2=1, g3=0
  putStrLn "Example 3: integral dx / sqrt(4x^3 - x)  [Weierstrass form]"
  let radicand3 = mkPoly [ri 0, ri (negate 1), ri 0, ri 4]  -- 4x^3 - x
      eg3 = EllipticIntegrand (constPoly (ri 1)) (constPoly (ri 1)) radicand3
  case reduceElliptic eg3 of
    Just result -> do
      putStrLn (prettyEllipticResult result)
    Nothing ->
      putStrLn "  Could not reduce."
  putStrLn ""

  putStrLn "Done."
