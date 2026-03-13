module Demo.EllipticIntegral

import Surd.Rational
import Surd.Poly
import Surd.Cyclotomic  -- Ring/Field instances
import Surd.Types
import Surd.EllipticIntegration
import Surd.Pretty
import Surd.LaTeX

import Data.List

%default covering

------------------------------------------------------------------------
-- Demo: reduction of elliptic integrals to Legendre normal forms
------------------------------------------------------------------------

||| Demo: integral dx / sqrt(x^3 - x)
||| This is a standard elliptic integral of the first kind.
||| P(x) = x(x-1)(x+1) = x^3 - x, roots at -1, 0, 1.
example1 : (String, EllipticIntegrand)
example1 =
  ( "integral dx / sqrt(x^3 - x)"
  , MkEllipticIntegrand
      (mkPoly [Rational.one])                           -- num = 1
      (mkPoly [Rational.one])                           -- den = 1
      (mkPoly [Rational.zero, negate Rational.one, Rational.zero, Rational.one])  -- x^3 - x
  )

||| Demo: integral dx / sqrt((1-x^2)(1-k^2*x^2))
||| The standard Legendre form with k^2 = 1/2.
example2 : (String, EllipticIntegrand)
example2 =
  ( "integral dx / sqrt((1 - x^2)(1 - x^2/2))"
  , MkEllipticIntegrand
      (mkPoly [Rational.one])
      (mkPoly [Rational.one])
      -- (1-x^2)(1-x^2/2) = 1 - 3x^2/2 + x^4/2
      (mkPoly [Rational.one, Rational.zero, negate (mkRat 3 2), Rational.zero, mkRat 1 2])
  )

||| Demo: integral x dx / sqrt(x^4 - 1)
example3 : (String, EllipticIntegrand)
example3 =
  ( "integral x dx / sqrt(x^4 - 1)"
  , MkEllipticIntegrand
      (mkPoly [Rational.zero, Rational.one])            -- num = x
      (mkPoly [Rational.one])                           -- den = 1
      (mkPoly [negate Rational.one, Rational.zero, Rational.zero, Rational.zero, Rational.one])  -- x^4 - 1
  )

export
main : IO ()
main = do
  putStrLn "=== Elliptic Integral Reduction Demo ==="
  putStrLn ""
  let examples = [example1, example2, example3]
  traverse_ (\ex => do
    let (desc, integrand) = ex
    putStrLn ("Problem: " ++ desc)
    putStrLn ("  Radicand: " ++ show (eiRadicand integrand))
    putStrLn ("  Degree: " ++ show (degreeInt (eiRadicand integrand)))
    case reduceElliptic integrand of
      Just result => do
        putStrLn ("  Result: " ++ prettyEllipticResult result)
        putStrLn ("  Modulus k: " ++ pretty (erModulus result))
        putStrLn ("  LaTeX: " ++ latexEllipticResult result)
      Nothing =>
        putStrLn "  Could not reduce to Legendre form"
    putStrLn ""
    ) examples
