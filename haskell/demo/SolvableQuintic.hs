{- | Demo: solving solvable quintic polynomials via Galois theory.

Tests the Galois group identification and radical tower construction
for degree 5 polynomials.
-}
module Main (main) where

import Data.Complex (imagPart, realPart)
import Math.Polynomial.Univariate (Poly, mkPoly)
import Surd.Galois.Identify
import Surd.Galois.RadicalTower (solveViaTower)
import Surd.Galois.Resolvent (discriminantOf, isSquareRational)
import Surd.Galois.TransitiveGroup
import Surd.Radical.DAG (dagEvalComplex, toDAG)
import Surd.Radical.Pretty (pretty)

data Example = Example
    { exName :: String
    , exPoly :: Poly Rational
    , exNotes :: String
    }

examples :: [Example]
examples =
    [ -- C₅ quintic: min poly of 2cos(2π/11)
      Example
        { exName = "x⁵ + x⁴ - 4x³ - 3x² + 3x + 1"
        , exPoly = mkPoly [1, 3, -3, -4, 1, 1]
        , exNotes = "C₅ — minimal polynomial of 2cos(2π/11)"
        }
    , -- F₂₀ quintic: x⁵ - 2 (splitting field = Q(⁵√2, ζ₅))
      Example
        { exName = "x⁵ - 2"
        , exPoly = mkPoly [-2, 0, 0, 0, 0, 1]
        , exNotes = "F₂₀ — Gal(Q(⁵√2,ζ₅)/Q) = Z/5 ⋊ Z/4"
        }
    , -- D₅ quintic
      Example
        { exName = "x⁵ + 20x + 32"
        , exPoly = mkPoly [32, 20, 0, 0, 0, 1]
        , exNotes = "D₅ — dihedral group of order 10"
        }
    , -- S₅ quintic (not solvable — Eisenstein)
      Example
        { exName = "x⁵ - 4x + 2"
        , exPoly = mkPoly [2, -4, 0, 0, 0, 1]
        , exNotes = "S₅ — irreducible by Eisenstein (not solvable)"
        }
    , -- x⁵ - x - 1 (not solvable)
      Example
        { exName = "x⁵ - x - 1"
        , exPoly = mkPoly [-1, -1, 0, 0, 0, 1]
        , exNotes = "S₅ (not solvable)"
        }
    ]

main :: IO ()
main = do
    putStrLn "══════ Solvable Quintic Solver ══════\n"
    mapM_ runExample examples

runExample :: Example -> IO ()
runExample ex = do
    putStrLn $ "─── " ++ exName ex ++ " ───"
    putStrLn $ "  (" ++ exNotes ex ++ ")"

    let f = exPoly ex
        disc = discriminantOf f
        discSq = isSquareRational disc

    putStrLn $ "  disc(f) = " ++ show disc
    putStrLn $ "  disc is square: " ++ show discSq

    case identifyGaloisGroup5 f of
        Nothing -> putStrLn "  (Galois group identification failed)\n"
        Just gr -> do
            let tg = grGroup gr
            putStrLn $ "  Galois group: " ++ tgName tg ++ " (order " ++ show (tgOrder tg) ++ ")"
            putStrLn $ "  Solvable: " ++ show (tgSolvable tg)

            -- Show numerical roots
            let roots = grRoots gr
                realRoots = filter (\r -> abs (imagPart r) < 1e-6) roots
            putStrLn $ "  Real roots: " ++ show (length realRoots)
            mapM_ (\r -> putStrLn $ "    " ++ show (realPart r)) realRoots

            if tgSolvable tg
                then case solveViaTower gr f of
                    Nothing -> putStrLn "  (radical tower construction failed)\n"
                    Just radExprs -> do
                        putStrLn "  Radical expressions:"
                        -- Show expressions whose numerical values are approximately real
                        let realExprs = filter (\e -> abs (imagPart (dagEvalComplex (toDAG e))) < 1e-6) radExprs
                            showExprs = if null realExprs then take 1 radExprs else realExprs
                        mapM_
                            ( \e -> do
                                let v = dagEvalComplex (toDAG e)
                                putStrLn $ "    " ++ pretty e
                                putStrLn $ "    ≈ " ++ show (realPart v)
                            )
                            showExprs
                        putStrLn ""
                else putStrLn "  (not solvable — no radical expression exists)\n"
