--- LaTeX rendering of radical expressions.
---
--- Produces math-mode LaTeX strings using \\frac, \\sqrt, etc.
module LaTeX
  ( latex
  , latexPrec
  ) where

import Rational
import RadExpr

--- Precedence levels.
precAdd :: Int
precAdd = 1

precMul :: Int
precMul = 2

precNeg :: Int
precNeg = 3

precPow :: Int
precPow = 4

--- Render a radical expression as a LaTeX math-mode string.
latex :: RadExpr Rational -> String
latex = latexPrec 0

--- Render with precedence context.
latexPrec :: Int -> RadExpr Rational -> String
latexPrec p expr = case expr of
  Lit r       -> latexRat r
  Neg e       -> latexNeg p e
  Add _ _     -> parensIf (p > precAdd) (renderTerms (flattenAdd expr))
  Mul a (Inv b) ->
    parensIf (p > precMul)
      ("\\frac{" ++ latexPrec 0 a ++ "}{" ++ latexPrec 0 b ++ "}")
  Mul (Inv a) b ->
    parensIf (p > precMul)
      ("\\frac{" ++ latexPrec 0 b ++ "}{" ++ latexPrec 0 a ++ "}")
  Inv e       ->
    parensIf (p > precMul)
      ("\\frac{1}{" ++ latexPrec 0 e ++ "}")
  Root 2 (Lit r) ->
    if ratEq r (Rational.fromInt (negate 1))
    then "\\mathrm{i}"
    else "\\sqrt{" ++ latexRadicand (Lit r) ++ "}"
  Root 2 e    -> "\\sqrt{" ++ latexRadicand e ++ "}"
  Root n e    -> "\\sqrt[" ++ show n ++ "]{" ++ latexRadicand e ++ "}"
  Pow _ 0     -> "1"
  Pow e n     ->
    if n < 0
    then latexPrec p (Inv (Pow e (negate n)))
    else if n == 1
         then latexPrec p e
         else parensIf (p > precPow)
                (latexBase e ++ "^{" ++ show n ++ "}")
  Mul _ _     -> parensIf (p > precMul) (renderFactors (flattenMul expr))

--- LaTeX negation with special handling.
latexNeg :: Int -> RadExpr Rational -> String
latexNeg p e = case e of
  Add _ _     -> parensIf (p > precAdd)
                   (renderTerms (map negTerm (flattenAdd e)))
  Mul (Lit c) rest -> latexPrec p (Mul (Lit (ratNeg c)) rest)
  Lit r       -> latexPrec p (Lit (ratNeg r))
  _           -> parensIf (p > precNeg) ("-" ++ latexPrec precNeg e)

--- Negate the sign of a term.
negTerm :: (Bool, RadExpr Rational) -> (Bool, RadExpr Rational)
negTerm (s, t) = (not s, t)

--- Render the base of a power expression.
--- Compound expressions need grouping for the exponent.
latexBase :: RadExpr Rational -> String
latexBase expr = case expr of
  Root _ _ -> "\\left(" ++ latexPrec 0 expr ++ "\\right)"
  Add _ _  -> "\\left(" ++ latexPrec 0 expr ++ "\\right)"
  Mul _ _  -> "\\left(" ++ latexPrec 0 expr ++ "\\right)"
  Neg _    -> "\\left(" ++ latexPrec 0 expr ++ "\\right)"
  Inv _    -> "\\left(" ++ latexPrec 0 expr ++ "\\right)"
  _        -> latexPrec precPow expr

--- Render a radicand (inside \\sqrt{...}).
--- No outer parens needed since braces provide grouping.
latexRadicand :: RadExpr Rational -> String
latexRadicand expr = case expr of
  Lit r ->
    let n = numerator r
        d = denominator r
    in if d == 1
       then show n
       else if n < 0
            then "-\\frac{" ++ show (absInt n) ++ "}{" ++ show d ++ "}"
            else "\\frac{" ++ show n ++ "}{" ++ show d ++ "}"
  _ -> latexPrec 0 expr

--- Flatten an Add tree into a list of (positive?, term) pairs.
flattenAdd :: RadExpr Rational -> [(Bool, RadExpr Rational)]
flattenAdd expr = case expr of
  Add a b     -> flattenAdd a ++ flattenAdd b
  Neg e       -> map negTerm (flattenAdd e)
  Lit r       -> if ratLt r (Rational.fromInt 0)
                 then [(False, Lit (ratNeg r))]
                 else [(True, Lit r)]
  Mul (Neg a) b -> [(False, Mul a b)]
  Mul _ _     -> case flattenMul expr of
                   (Lit r : rest) ->
                     if ratLt r (Rational.fromInt 0)
                     then [(False, rebuildMul (Lit (ratNeg r) : rest))]
                     else [(True, expr)]
                   _ -> [(True, expr)]
  _           -> [(True, expr)]

--- Flatten a Mul tree.
flattenMul :: RadExpr Rational -> [RadExpr Rational]
flattenMul expr = case expr of
  Mul a b -> flattenMul a ++ flattenMul b
  _       -> [expr]

--- Rebuild a Mul from a list.
rebuildMul :: [RadExpr Rational] -> RadExpr Rational
rebuildMul xs = case xs of
  []       -> Lit (Rational.fromInt 1)
  [x]      -> x
  (x:rest) -> foldl Mul x rest

--- Render signed terms for Add.
renderTerms :: [(Bool, RadExpr Rational)] -> String
renderTerms ts = case ts of
  [] -> "0"
  ((s,t):rest) ->
    let hd = if s then latexPrec precAdd t
             else "-" ++ latexPrec precMul t
    in hd ++ concatMap renderRest rest

--- Render a subsequent term with sign.
renderRest :: (Bool, RadExpr Rational) -> String
renderRest (s, e) = if s
                    then " + " ++ latexPrec precAdd e
                    else " - " ++ latexPrec precMul e

--- Render factors for Mul.
renderFactors :: [RadExpr Rational] -> String
renderFactors fs = case fs of
  []    -> "1"
  [x]   -> latexPrec precMul x
  (Lit c : rest) ->
    if ratEq c (Rational.fromInt 1)
    then joinMul (map (latexPrec precPow) rest)
    else if ratEq c (Rational.fromInt (negate 1))
    then "-" ++ joinMul (map (latexPrec precPow) rest)
    else latexRat c ++ " \\cdot " ++ joinMul (map (latexPrec precPow) rest)
  _ -> joinMul (map (latexPrec precPow) fs)

--- Join with LaTeX multiplication dot.
joinMul :: [String] -> String
joinMul xs = case xs of
  []    -> ""
  [x]   -> x
  (x:rest) -> x ++ concatMap (" \\cdot " ++) rest

--- Render a Rational as LaTeX.
latexRat :: Rational -> String
latexRat r =
  let n = numerator r
      d = denominator r
  in if d == 1
     then show n
     else if n < 0
          then "-\\frac{" ++ show (absInt n) ++ "}{" ++ show d ++ "}"
          else "\\frac{" ++ show n ++ "}{" ++ show d ++ "}"

--- Absolute value of an integer.
absInt :: Int -> Int
absInt n = if n < 0 then negate n else n

--- Conditionally wrap in LaTeX parens.
parensIf :: Bool -> String -> String
parensIf cond s = if cond
                  then "\\left(" ++ s ++ "\\right)"
                  else s
