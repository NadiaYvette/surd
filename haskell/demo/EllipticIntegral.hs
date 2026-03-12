{-# LANGUAGE PatternSynonyms #-}

-- | Demo: elliptic integral reduction to Legendre normal forms.
--
-- Shows how ∫ R(x) dx / √P(x) with P degree 3 or 4 reduces to
-- F(φ,k), E(φ,k), and Π(φ,n,k) with exact radical modulus k.
module Main (main) where

import Data.Ratio ((%))
import Math.Polynomial.Univariate (mkPoly)
import Surd.Integration.Elliptic

-- | An example integral with a description.
data Example = Example
  { exName  :: String
  , exIntgd :: EllipticIntegrand
  , exNotes :: String
  }

examples :: [Example]
examples =
  [ Example
      { exName  = "∫ dx / √(x³ - x)"
      , exIntgd = EllipticIntegrand
          { eiNum = 1, eiDen = 1
          , eiRadicand = mkPoly [0, -1, 0, 1] }  -- x³ - x = x(x-1)(x+1)
      , exNotes = "roots 1, 0, -1; k² = 1/2"
      }
  , Example
      { exName  = "∫ dx / √(x³ - 1)"
      , exIntgd = EllipticIntegrand
          { eiNum = 1, eiDen = 1
          , eiRadicand = mkPoly [-1, 0, 0, 1] }  -- x³ - 1
      , exNotes = "one rational root at 1, two complex — should fail (not all real)"
      }
  , Example
      { exName  = "∫ dx / √((1-x²)(1-½x²))"
      , exIntgd = EllipticIntegrand
          { eiNum = 1, eiDen = 1
          , eiRadicand = mkPoly [1, 0, -3%2, 0, 1%2] }
          -- (1-x²)(1-x²/2) = 1 - 3x²/2 + x⁴/2
      , exNotes = "already Legendre-like; roots ±1, ±√2; k² = 1/2"
      }
  , Example
      { exName  = "∫ dx / √(4x³ - 4x)"
      , exIntgd = EllipticIntegrand
          { eiNum = 1, eiDen = 1
          , eiRadicand = mkPoly [0, -4, 0, 4] }  -- 4x³ - 4x = 4x(x-1)(x+1)
      , exNotes = "same roots as #1, leading coeff 4; k² = 1/2"
      }
  , Example
      { exName  = "∫ dx / √((x²-1)(x²-4))"
      , exIntgd = EllipticIntegrand
          { eiNum = 1, eiDen = 1
          , eiRadicand = mkPoly [4, 0, -5, 0, 1] }  -- (x²-1)(x²-4) = x⁴-5x²+4
      , exNotes = "roots 2, 1, -1, -2; quartic"
      }
  , Example
      { exName  = "∫ dx / ((x-3)·√(x³ - x))"
      , exIntgd = EllipticIntegrand
          { eiNum = 1, eiDen = mkPoly [-3, 1]  -- (x - 3)
          , eiRadicand = mkPoly [0, -1, 0, 1] }
      , exNotes = "Π form: pole at x=3"
      }
  , Example
      { exName  = "∫ dx / √(x³ - 7x + 6)"
      , exIntgd = EllipticIntegrand
          { eiNum = 1, eiDen = 1
          , eiRadicand = mkPoly [6, -7, 0, 1] }  -- x³ - 7x + 6 = (x-1)(x-2)(x+3)
      , exNotes = "roots 2, 1, -3; factors (x-1)(x-2)(x+3)"
      }
  ]

main :: IO ()
main = do
  let fmt = "text"  -- could parse from args
      jacobi = False
  mapM_ (renderExample fmt jacobi) examples

renderExample :: String -> Bool -> Example -> IO ()
renderExample fmt jacobi ex = do
  putStrLn $ "─── " ++ exName ex ++ " ───"
  putStrLn $ "  (" ++ exNotes ex ++ ")"
  case reduceElliptic jacobi (exIntgd ex) of
    Nothing -> putStrLn "  (cannot reduce: not all roots are real, or degree unsupported)\n"
    Just result ->
      case fmt of
        "latex" -> putStrLn $ latexEllipticResult result
        _       -> putStrLn $ prettyEllipticResult result
