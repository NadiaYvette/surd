-- | LaTeX rendering of radical expressions.
module Surd.Radical.LaTeX
  ( latex
  , latexPrec
  ) where

import Data.Ratio (numerator, denominator)
import Surd.Types

-- | Render a radical expression as a LaTeX math-mode string.
latex :: RadExpr Rational -> String
latex = latexPrec 0

precAdd, precMul, precNeg, precPow :: Int
precAdd = 1
precMul = 2
precNeg = 3
precPow = 4

latexPrec :: Int -> RadExpr Rational -> String
latexPrec _ (Lit r) = latexRat r
latexPrec p (Neg e) = case e of
  -- Neg of a sum: distribute the sign into terms
  Add _ _ ->
    parensIf (p > precAdd) $ renderTerms (map negTerm (flattenAdd e))
  -- Neg of a product with literal coefficient: absorb sign
  Mul (Lit c) rest ->
    latexPrec p (Mul (Lit (negate c)) rest)
  -- Neg of a literal: just negate
  Lit r ->
    latexPrec p (Lit (negate r))
  -- Otherwise: prefix minus
  _ ->
    parensIf (p > precNeg) $ "-" ++ latexPrec precNeg e
  where
    negTerm (s, t) = (not s, t)
latexPrec p e@(Add _ _) =
  parensIf (p > precAdd) $ renderTerms (flattenAdd e)
-- a / b
latexPrec p (Mul a (Inv b)) =
  parensIf (p > precMul) $ "\\frac{" ++ latexPrec 0 a ++ "}{" ++ latexPrec 0 b ++ "}"
-- (1/a) * b  →  b/a
latexPrec p (Mul (Inv a) b) =
  parensIf (p > precMul) $ "\\frac{" ++ latexPrec 0 b ++ "}{" ++ latexPrec 0 a ++ "}"
latexPrec p (Inv e) =
  parensIf (p > precMul) $ "\\frac{1}{" ++ latexPrec 0 e ++ "}"
latexPrec _ (Root 2 (Lit (-1))) = "\\mathrm{i}"
latexPrec _ (Root 2 e) = "\\sqrt{" ++ latexRadicand e ++ "}"
latexPrec _ (Root n e) = "\\sqrt[" ++ show n ++ "]{" ++ latexRadicand e ++ "}"
latexPrec p (Pow e n) =
  parensIf (p > precPow) $ latexBase e ++ "^{" ++ show n ++ "}"
latexPrec p e@(Mul _ _) =
  parensIf (p > precMul) $ renderFactors (flattenMul e)

-- | Render the base of a power expression.
-- Roots and other compound expressions need grouping for the exponent.
latexBase :: RadExpr Rational -> String
latexBase e@(Root _ _) = "\\left(" ++ latexPrec 0 e ++ "\\right)"
latexBase e@(Add _ _)  = "\\left(" ++ latexPrec 0 e ++ "\\right)"
latexBase e@(Mul _ _)  = "\\left(" ++ latexPrec 0 e ++ "\\right)"
latexBase e@(Neg _)    = "\\left(" ++ latexPrec 0 e ++ "\\right)"
latexBase e@(Inv _)    = "\\left(" ++ latexPrec 0 e ++ "\\right)"
latexBase e            = latexPrec precPow e

-- | Render a radicand (inside \\sqrt{...}).
-- No outer parens needed since braces provide grouping.
latexRadicand :: RadExpr Rational -> String
latexRadicand (Lit r)
  | d == 1    = show n'  -- no parens needed inside braces, even for negatives
  | n' < 0   = "-\\frac{" ++ show (abs n') ++ "}{" ++ show d ++ "}"
  | otherwise = "\\frac{" ++ show n' ++ "}{" ++ show d ++ "}"
  where n' = numerator r; d = denominator r
latexRadicand e = latexPrec 0 e

-- --------------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------------

parensIf :: Bool -> String -> String
parensIf True  s = "\\left(" ++ s ++ "\\right)"
parensIf False s = s

latexRat :: Rational -> String
latexRat r
  | d == 1    = show n'
  | n' < 0   = "-\\frac{" ++ show (abs n') ++ "}{" ++ show d ++ "}"
  | otherwise = "\\frac{" ++ show n' ++ "}{" ++ show d ++ "}"
  where
    n' = numerator r
    d  = denominator r

flattenAdd :: RadExpr Rational -> [(Bool, RadExpr Rational)]
flattenAdd (Add a b) = flattenAdd a ++ flattenAdd b
flattenAdd (Neg e)   = map (\(s, t) -> (not s, t)) (flattenAdd e)
flattenAdd (Lit r) | r < 0 = [(False, Lit (negate r))]
flattenAdd (Mul (Lit r) b) | r < 0 = [(False, Mul (Lit (negate r)) b)]
flattenAdd e = [(True, e)]

renderTerms :: [(Bool, RadExpr Rational)] -> String
renderTerms [] = "0"
renderTerms ((s, t):rest) =
  let first = if s then latexPrec precAdd t else "-" ++ latexMulOrAtom t
  in first ++ concatMap rr rest
  where
    rr (True,  e) = " + " ++ latexPrec precAdd e
    rr (False, e) = " - " ++ latexMulOrAtom e
    -- Render at multiplication level without adding parens for products/atoms
    latexMulOrAtom = latexPrec precMul

flattenMul :: RadExpr Rational -> [RadExpr Rational]
flattenMul (Mul a b) = flattenMul a ++ flattenMul b
flattenMul e         = [e]

renderFactors :: [RadExpr Rational] -> String
renderFactors [] = "1"
renderFactors [x] = latexPrec precMul x
renderFactors (Lit c : rest)
  | c == 1    = joinMul (map (latexPrec precPow) rest)
  | c == -1   = "-" ++ joinMul (map (latexPrec precPow) rest)
  | otherwise = latexRat c ++ " \\cdot " ++ joinMul (map (latexPrec precPow) rest)
renderFactors fs = joinMul (map (latexPrec precPow) fs)

joinMul :: [String] -> String
joinMul []     = ""
joinMul [x]    = x
joinMul (x:xs) = x ++ concatMap (" \\cdot " ++) xs
