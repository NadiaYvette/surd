--- Demo: Euler substitution for integrals involving square roots
--- of quadratics.
module DemoEulerIntegral where

import Rational
import Poly
import EulerIntegration

--- Local alias.
ri :: Int -> Rational
ri = Rational.fromInt

--- Main: demonstrate Euler substitution integration.
main :: IO ()
main = do
  putStrLn "===== Euler Substitution Integration Demo ====="
  putStrLn ""

  -- Example 1: integral dx / sqrt(x^2 + 1)
  -- Answer: ln|x + sqrt(x^2+1)| = arsinh(x)
  putStrLn "Example 1: integral dx / sqrt(x^2 + 1)"
  let eg1 = EulerIntegrand (constPoly (ri 1)) (constPoly (ri 1))
                            (negate 1) (ri 1) (ri 0) (ri 1)
  case eulerIntegrate eg1 of
    Just (IntegralResult expr a b c) -> do
      putStrLn ("  Result: " ++ prettySymExpr expr)
      putStrLn ("  LaTeX:  " ++ latexSymExpr expr)
    Nothing ->
      putStrLn "  Could not integrate."
  putStrLn ""

  -- Example 2: integral dx / sqrt(1 - x^2)
  -- Answer: arcsin(x)
  putStrLn "Example 2: integral dx / sqrt(1 - x^2)"
  let eg2 = EulerIntegrand (constPoly (ri 1)) (constPoly (ri 1))
                            (negate 1) (ri (negate 1)) (ri 0) (ri 1)
  case eulerIntegrate eg2 of
    Just (IntegralResult expr a b c) -> do
      putStrLn ("  Result: " ++ prettySymExpr expr)
    Nothing ->
      putStrLn "  Could not integrate (disc <= 0, expected for this case)."
  putStrLn ""

  -- Example 3: integral x dx / sqrt(x^2 + 2x + 5)
  putStrLn "Example 3: integral x dx / sqrt(x^2 + 2x + 5)"
  let eg3 = EulerIntegrand monoX (constPoly (ri 1))
                            (negate 1) (ri 1) (ri 2) (ri 5)
  case eulerIntegrate eg3 of
    Just (IntegralResult expr a b c) -> do
      putStrLn ("  Result: " ++ prettySymExpr expr)
      putStrLn ("  LaTeX:  " ++ latexSymExpr expr)
    Nothing ->
      putStrLn "  Could not integrate."
  putStrLn ""

  -- Example 4: Rational function integration
  putStrLn "Example 4: integral (2x+1)/(x+1) dx"
  let num4 = mkPoly [ri 1, ri 2]  -- 2x + 1
      den4 = mkPoly [ri 1, ri 1]  -- x + 1
      result4 = integrateRational num4 den4
  putStrLn ("  Result: " ++ prettySymExpr result4)
  putStrLn ""

  putStrLn "Done."
