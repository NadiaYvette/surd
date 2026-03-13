module Demo.EulerIntegral

import Surd.Rational
import Surd.Poly
import Surd.Cyclotomic  -- Ring/Field instances
import Surd.Types
import Surd.EulerIntegration
import Surd.Pretty

import Data.List

%default covering

------------------------------------------------------------------------
-- Demo: Euler substitution for integrals with square roots
------------------------------------------------------------------------

||| Demo: integrate 1/sqrt(x^2 + 1) dx
|||
||| This is integral dx/sqrt(x^2+1) = arsinh(x) = ln(x + sqrt(x^2+1))
||| Using Euler substitution 1 (a=1 > 0):
|||   sqrt(x^2+1) = t - x, so x = (t^2-1)/(2t), dx = (t^2+1)/(2t^2) dt
example1 : (String, EulerIntegrand)
example1 =
  ( "integral dx / sqrt(x^2 + 1)"
  , MkEulerIntegrand
      (mkPoly [Rational.one])         -- P(x) = 1
      (mkPoly [Rational.one])         -- Q(x) = 1
      (-1)                             -- 1/sqrt(...)
      Rational.one                     -- a = 1
      Rational.zero                    -- b = 0
      Rational.one                     -- c = 1
  )

||| Demo: integrate x / sqrt(4 - x^2) dx
|||
||| = -sqrt(4 - x^2) + C
example2 : (String, EulerIntegrand)
example2 =
  ( "integral x dx / sqrt(4 - x^2)"
  , MkEulerIntegrand
      (mkPoly [Rational.zero, Rational.one])  -- P(x) = x
      (mkPoly [Rational.one])                  -- Q(x) = 1
      (-1)                                      -- 1/sqrt(...)
      (negate Rational.one)                    -- a = -1
      Rational.zero                             -- b = 0
      (Rational.fromInteger 4)                 -- c = 4
  )

||| Demo: integrate 1 / (x * sqrt(x^2 - 1)) dx
|||
||| = arcsec(x) + C
example3 : (String, EulerIntegrand)
example3 =
  ( "integral dx / (x * sqrt(x^2 - 1))"
  , MkEulerIntegrand
      (mkPoly [Rational.one])                  -- P(x) = 1
      (mkPoly [Rational.zero, Rational.one])   -- Q(x) = x
      (-1)                                      -- 1/sqrt(...)
      Rational.one                              -- a = 1
      Rational.zero                             -- b = 0
      (negate Rational.one)                    -- c = -1
  )

export
main : IO ()
main = do
  putStrLn "=== Euler Substitution Integration Demo ==="
  putStrLn ""
  let examples = [example1, example2, example3]
  traverse_ (\ex => do
    let (desc, integrand) = ex
    putStrLn ("Problem: " ++ desc)
    putStrLn ("  a = " ++ show (eiA integrand) ++ ", b = " ++ show (eiB integrand) ++ ", c = " ++ show (eiC integrand))
    case eulerIntegrate integrand of
      Just result => do
        putStrLn ("  Antiderivative: " ++ prettySymExpr (irExpr result))
        putStrLn ("  LaTeX: " ++ latexSymExpr (irExpr result))
      Nothing =>
        putStrLn "  Could not integrate (no valid Euler substitution)"
    putStrLn ""
    ) examples
