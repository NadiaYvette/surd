{- | Solvable quintic solver demo.

Demonstrates Galois-theoretic identification and radical tower construction
for degree-5 polynomials over Q. The programme classifies each polynomial's
Galois group among the five transitive subgroups of S5, then — when the group
is solvable — constructs explicit radical expressions for the roots.

== Mathematical background

A degree-5 polynomial over Q has Galois group isomorphic to one of the five
transitive subgroups of S5 (up to conjugacy):

  1. __C5__ (cyclic, order 5) — always solvable.
  2. __D5__ (dihedral, order 10) — solvable.
  3. __F20__ (Frobenius, order 20) — solvable.  This is the normaliser of
     C5 in S5, isomorphic to Z\/5 ⋊ Z\/4.
  4. __A5__ (alternating, order 60) — __not__ solvable.
  5. __S5__ (symmetric, order 120) — __not__ solvable.

The first three are solvable, so their roots can be expressed in radicals.
The last two are not solvable by the Abel–Ruffini theorem.

Classification uses the sextic resolvent and discriminant, following the
algorithm of Dummit (1991), "Solving Solvable Quintics", Table 1.

== Test cases

The demo runs five representative polynomials, one from each major class:

  * @x^5 + x^4 - 4x^3 - 3x^2 + 3x + 1@: Galois group C5.
    Minimal polynomial of 2 cos(2 pi\/11); all five roots are real.

  * @x^5 - 2@: Galois group F20.
    The classic Frobenius example with splitting field Q(5th-root 2, zeta5).

  * @x^5 + 20x + 32@: Galois group D5 (dihedral of order 10).

  * @x^5 - 4x + 2@: Galois group S5.
    Irreducible by Eisenstein's criterion at p = 2; not solvable in radicals.

  * @x^5 - x - 1@: Galois group S5.
    Related to the Bring–Jerrard normal form; not solvable in radicals.

== Expected output

For each polynomial the programme prints the discriminant, whether it is a
perfect square in Q, the identified Galois group, and (when solvable) the
radical expressions for the real roots together with their numerical values.

== References

  * Dummit, D. S. (1991). /Solving Solvable Quintics/.
    Mathematics of Computation, 57(195), 387–401.
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

-- | A named test case pairing a polynomial with human-readable metadata.
data Example = Example
    { exName :: String
    -- ^ Display name (Unicode rendering of the polynomial).
    , exPoly :: Poly Rational
    -- ^ The polynomial, given in ascending-coefficient order via 'mkPoly'.
    , exNotes :: String
    -- ^ One-line annotation (Galois group, mathematical context).
    }

{- | The five representative test polynomials, spanning all major Galois group
classes for irreducible quintics over Q.  Three are solvable (C5, F20, D5)
and two are not (S5).
-}
examples :: [Example]
examples =
    [ -- C5 quintic: minimal polynomial of 2cos(2pi/11).
      -- All five roots are real; the Galois group is cyclic of order 5.
      Example
        { exName = "x⁵ + x⁴ - 4x³ - 3x² + 3x + 1"
        , exPoly = mkPoly [1, 3, -3, -4, 1, 1]
        , exNotes = "C₅ — minimal polynomial of 2cos(2π/11)"
        }
    , -- F20 (Frobenius) quintic: x^5 - 2.
      -- Splitting field Q(5th-root 2, zeta5) has degree 20 over Q.
      -- Gal = Z/5 ⋊ Z/4, the unique Frobenius group of order 20.
      Example
        { exName = "x⁵ - 2"
        , exPoly = mkPoly [-2, 0, 0, 0, 0, 1]
        , exNotes = "F₂₀ — Gal(Q(⁵√2,ζ₅)/Q) = Z/5 ⋊ Z/4"
        }
    , -- D5 (dihedral) quintic of order 10.
      Example
        { exName = "x⁵ + 20x + 32"
        , exPoly = mkPoly [32, 20, 0, 0, 0, 1]
        , exNotes = "D₅ — dihedral group of order 10"
        }
    , -- S5 quintic (not solvable in radicals).
      -- Irreducible by Eisenstein's criterion at p = 2.
      Example
        { exName = "x⁵ - 4x + 2"
        , exPoly = mkPoly [2, -4, 0, 0, 0, 1]
        , exNotes = "S₅ — irreducible by Eisenstein (not solvable)"
        }
    , -- S5 quintic (not solvable).  Related to the Bring-Jerrard normal form.
      Example
        { exName = "x⁵ - x - 1"
        , exPoly = mkPoly [-1, -1, 0, 0, 0, 1]
        , exNotes = "S₅ (not solvable)"
        }
    ]

-- | Run all five test cases, printing results to stdout.
main :: IO ()
main = do
    putStrLn "══════ Solvable Quintic Solver ══════\n"
    mapM_ runExample examples

{- | Process a single example: identify the Galois group via the sextic
resolvent and discriminant, then (if solvable) construct and display
radical expressions for the roots.
-}
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
