-- | Demo: Euler substitution integrals.
--
-- Computes antiderivatives of ∫ P(x)/Q(x) · (√(ax²+bx+c))^n dx
-- using Euler's three substitutions, with radical coefficients
-- simplified by surd's normalization machinery.
{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import Math.Polynomial.Univariate (mkPoly, pattern X)
import Surd.Integration.Euler

-- | An example integral with a description and known closed form.
data Example = Example
  { exName     :: String          -- ^ human-readable description
  , exIntgd    :: EulerIntegrand  -- ^ the integrand
  , exKnown    :: String          -- ^ known closed-form answer
  }

examples :: [Example]
examples =
  [ Example
      { exName  = "∫ dx / √(x²+1)"
      , exIntgd = EulerIntegrand
          { eiP = 1, eiQ = 1, eiSqrtPow = -1, eiA = 1, eiB = 0, eiC = 1 }
      , exKnown = "ln|x + √(x²+1)|"
      }
  , Example
      { exName  = "∫ dx / √(x²-1)"
      , exIntgd = EulerIntegrand
          { eiP = 1, eiQ = 1, eiSqrtPow = -1, eiA = 1, eiB = 0, eiC = -1 }
      , exKnown = "ln|x + √(x²-1)|"
      }
  , Example
      { exName  = "∫ dx / √(x²+2x+2)"
      , exIntgd = EulerIntegrand
          { eiP = 1, eiQ = 1, eiSqrtPow = -1, eiA = 1, eiB = 2, eiC = 2 }
      , exKnown = "ln|x + 1 + √(x²+2x+2)|"
      }
  , Example
      { exName  = "∫ x dx / √(x²+1)"
      , exIntgd = EulerIntegrand
          { eiP = X, eiQ = 1, eiSqrtPow = -1, eiA = 1, eiB = 0, eiC = 1 }
      , exKnown = "√(x²+1)"
      }
  , Example
      { exName  = "∫ dx / √(4x²+1)"
      , exIntgd = EulerIntegrand
          { eiP = 1, eiQ = 1, eiSqrtPow = -1, eiA = 4, eiB = 0, eiC = 1 }
      , exKnown = "(1/2)·ln|2x + √(4x²+1)|"
      }
  , Example
      { exName  = "∫ dx / ((x+1)·√(x²+1))"
      , exIntgd = EulerIntegrand
          { eiP = 1, eiQ = mkPoly [1, 1], eiSqrtPow = -1
          , eiA = 1, eiB = 0, eiC = 1 }
      , exKnown = "..."
      }
  , Example
      { exName  = "∫ √(x²+1) dx"
      , exIntgd = EulerIntegrand
          { eiP = 1, eiQ = 1, eiSqrtPow = 1, eiA = 1, eiB = 0, eiC = 1 }
      , exKnown = "(x·√(x²+1) + ln|x + √(x²+1)|) / 2"
      }
  , Example
      { exName  = "∫ dx / √(1-x²)  [Euler 2]"
      , exIntgd = EulerIntegrand
          { eiP = 1, eiQ = 1, eiSqrtPow = -1, eiA = -1, eiB = 0, eiC = 1 }
      , exKnown = "arcsin(x)"
      }
  , Example
      { exName  = "∫ dx / √(x²-3x+2)  [Euler 3]"
      , exIntgd = EulerIntegrand
          { eiP = 1, eiQ = 1, eiSqrtPow = -1
          , eiA = 1, eiB = -3, eiC = 2 }
      , exKnown = "ln|x - 3/2 + √(x²-3x+2)|"
      }
  ]

main :: IO ()
main = do
  let fmt = "text"  -- could parse from args
  mapM_ (renderExample fmt) examples

renderExample :: String -> Example -> IO ()
renderExample fmt ex = do
  putStrLn $ "─── " ++ exName ex ++ " ───"
  case eulerIntegrate (exIntgd ex) of
    Nothing -> putStrLn "  (no rational Euler substitution found)"
    Just result ->
      case fmt of
        "latex" -> do
          putStrLn $ "  = " ++ latexSymExpr (irExpr result)
          putStrLn $ "  Known: " ++ exKnown ex
        _ -> do
          putStrLn $ "  = " ++ prettySymExpr (irExpr result)
          putStrLn $ "  Known: " ++ exKnown ex
  putStrLn ""
