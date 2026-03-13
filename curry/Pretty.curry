--- Pretty-printing radical expressions in human-readable mathematical notation.
---
--- Uses precedence-based parenthesization for minimal brackets.
module Pretty
  ( pretty
  , prettyPrec
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

--- Render a radical expression as a human-readable string.
pretty :: RadExpr Rational -> String
pretty = prettyPrec 0

--- Render with precedence context.
prettyPrec :: Int -> RadExpr Rational -> String
prettyPrec p expr = case expr of
  Lit r     -> prettyRat r
  Neg e     -> prettyNeg p e
  Add _ _   -> parensIf (p > precAdd) (renderTermsBasic (flattenAddBasic expr))
  Mul a (Inv b) ->
    parensIf (p > precMul) (prettyPrec precMul a ++ "/" ++ prettyPrec precPow b)
  Mul _ _   -> parensIf (p > precMul) (renderFactorsBasic (flattenMulBasic expr))
  Inv e     -> parensIf (p > precMul) ("1/" ++ prettyPrec precPow e)
  Root 2 (Lit r) -> if ratEq r (Rational.fromInt (negate 1))
                     then "i"
                     else "sqrt" ++ prettyRadicand (Lit r)
  Root 2 e  -> "sqrt" ++ prettyRadicand e
  Root 3 e  -> "cbrt" ++ prettyRadicand e
  Root n e  -> show n ++ "rt" ++ prettyRadicand e
  Pow _ 0   -> "1"
  Pow e n   -> if n < 0
               then prettyPrec p (Inv (Pow e (negate n)))
               else if n == 1
                    then prettyPrec p e
                    else parensIf (p > precPow) (prettyPrec precPow e ++ "^" ++ show n)

--- Pretty-print negation with special handling.
prettyNeg :: Int -> RadExpr Rational -> String
prettyNeg p e = case e of
  Add _ _     -> parensIf (p > precAdd)
                   (renderTermsBasic (map negTerm (flattenAddBasic e)))
  Mul (Lit c) rest -> prettyPrec p (Mul (Lit (ratNeg c)) rest)
  Lit r       -> prettyPrec p (Lit (ratNeg r))
  _           -> parensIf (p > precNeg) ("-" ++ prettyPrec precNeg e)

--- Negate the sign of a term.
negTerm :: (Bool, RadExpr Rational) -> (Bool, RadExpr Rational)
negTerm (s, t) = (not s, t)

--- Render a radicand (the expression under a root sign).
prettyRadicand :: RadExpr Rational -> String
prettyRadicand expr = case expr of
  Lit r -> if ratGe r (Rational.fromInt 0) && denominator r == 1
           then show (numerator r)
           else "(" ++ pretty expr ++ ")"
  Root _ _ -> prettyPrec precPow expr
  _        -> "(" ++ pretty expr ++ ")"

--- Flatten an Add tree into a list of (positive?, term) pairs.
flattenAddBasic :: RadExpr Rational -> [(Bool, RadExpr Rational)]
flattenAddBasic expr = case expr of
  Add a b   -> flattenAddBasic a ++ flattenAddBasic b
  Neg e     -> map negTerm (flattenAddBasic e)
  Lit r     -> if ratLt r (Rational.fromInt 0)
               then [(False, Lit (ratNeg r))]
               else [(True, Lit r)]
  Mul (Neg a) b -> [(False, Mul a b)]
  Mul _ _   -> case flattenMulBasic expr of
                 (Lit r : rest) -> if ratLt r (Rational.fromInt 0)
                                   then [(False, rebuildMul (Lit (ratNeg r) : rest))]
                                   else [(True, expr)]
                 _ -> [(True, expr)]
  _         -> [(True, expr)]

--- Flatten a Mul tree into a flat list.
flattenMulBasic :: RadExpr Rational -> [RadExpr Rational]
flattenMulBasic expr = case expr of
  Mul a b -> flattenMulBasic a ++ flattenMulBasic b
  _       -> [expr]

--- Rebuild a Mul from a list.
rebuildMul :: [RadExpr Rational] -> RadExpr Rational
rebuildMul xs = case xs of
  []    -> Lit (Rational.fromInt 1)
  [x]   -> x
  (x:rest) -> foldl Mul x rest

--- Render a list of signed terms.
renderTermsBasic :: [(Bool, RadExpr Rational)] -> String
renderTermsBasic ts = case ts of
  [] -> "0"
  ((s,t):rest) ->
    let hd = if s then prettyPrec precAdd t
             else "-" ++ prettyPrec precMul t
    in hd ++ concatMap renderRest rest

--- Render a subsequent term with sign.
renderRest :: (Bool, RadExpr Rational) -> String
renderRest (s, e) = if s
                    then " + " ++ prettyPrec precAdd e
                    else " - " ++ prettyPrec precMul e

--- Render a list of factors.
renderFactorsBasic :: [RadExpr Rational] -> String
renderFactorsBasic fs = case fs of
  []    -> "1"
  [x]   -> prettyPrec precMul x
  (Lit c : rest) ->
    if ratEq c (Rational.fromInt 1)
    then joinWith "*" (map (prettyPrec precPow) rest)
    else if ratEq c (Rational.fromInt (negate 1))
    then "-" ++ joinWith "*" (map (prettyPrec precPow) rest)
    else prettyRat c ++ "*" ++ joinWith "*" (map (prettyPrec precPow) rest)
  _ -> joinWith "*" (map (prettyPrec precPow) fs)

--- Pretty-print a rational number.
prettyRat :: Rational -> String
prettyRat r =
  let n = numerator r
      d = denominator r
  in if d == 1
     then show n
     else if n < 0
          then "(-" ++ show (absInt n) ++ "/" ++ show d ++ ")"
          else "(" ++ show n ++ "/" ++ show d ++ ")"

--- Absolute value of an integer.
absInt :: Int -> Int
absInt n = if n < 0 then negate n else n

--- Conditionally wrap in parentheses.
parensIf :: Bool -> String -> String
parensIf cond s = if cond then "(" ++ s ++ ")" else s

--- Join strings with a separator.
joinWith :: String -> [String] -> String
joinWith sep xs = case xs of
  []     -> ""
  [x]    -> x
  (x:rest) -> x ++ concatMap (sep ++) rest
