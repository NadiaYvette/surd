module Surd.LaTeX

import Surd.Rational
import Surd.Types

import Data.Nat
import Data.List

%default covering

------------------------------------------------------------------------
-- Precedence levels
------------------------------------------------------------------------

precAdd : Int
precAdd = 1

precMul : Int
precMul = 2

precNeg : Int
precNeg = 3

precPow : Int
precPow = 4

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

parensIf : Bool -> String -> String
parensIf True  s = "\\left(" ++ s ++ "\\right)"
parensIf False s = s

latexRat : Rational -> String
latexRat r =
  let n = numer r
      d = denom r
  in if d == 1 then show n
     else if n < 0 then "-\\frac{" ++ show (abs n) ++ "}{" ++ show d ++ "}"
     else "\\frac{" ++ show n ++ "}{" ++ show d ++ "}"

joinMul : List String -> String
joinMul [] = ""
joinMul [x] = x
joinMul (x :: xs) = x ++ concatMap (" \\cdot " ++) xs

------------------------------------------------------------------------
-- Flattening helpers
------------------------------------------------------------------------

flattenMul : RadExpr Rational -> List (RadExpr Rational)
flattenMul (Mul a b) = flattenMul a ++ flattenMul b
flattenMul e = [e]

rebuildMul : List (RadExpr Rational) -> RadExpr Rational
rebuildMul [] = Lit Rational.one
rebuildMul [x] = x
rebuildMul (x :: xs) = foldl Mul x xs

mutual
  flattenAdd : RadExpr Rational -> List (Bool, RadExpr Rational)
  flattenAdd (Add a b) = flattenAdd a ++ flattenAdd b
  flattenAdd (Neg e) = map (\(s, t) => (not s, t)) (flattenAdd e)
  flattenAdd (Lit r) =
    if r < Rational.zero then [(False, Lit (negate r))]
    else [(True, Lit r)]
  flattenAdd (Mul (Neg a) b) = [(False, Mul a b)]
  flattenAdd e@(Mul _ _) = case flattenMul e of
    (Lit r :: rest) =>
      if r < Rational.zero
        then [(False, rebuildMul (Lit (negate r) :: rest))]
        else [(True, e)]
    _ => [(True, e)]
  flattenAdd e = [(True, e)]

  renderTerms : List (Bool, RadExpr Rational) -> String
  renderTerms [] = "0"
  renderTerms ((s, t) :: rest) =
    let hd = if s then latexPrec precAdd t
             else "-" ++ latexPrec precMul t
    in hd ++ concatMap rr rest
    where
      rr : (Bool, RadExpr Rational) -> String
      rr (True, e) = " + " ++ latexPrec precAdd e
      rr (False, e) = " - " ++ latexPrec precMul e

  renderFactors : List (RadExpr Rational) -> String
  renderFactors [] = "1"
  renderFactors [x] = latexPrec precMul x
  renderFactors (Lit c :: rest) =
    if c == Rational.one then joinMul (map (latexPrec precPow) rest)
    else if c == negate Rational.one then "-" ++ joinMul (map (latexPrec precPow) rest)
    else latexRat c ++ " \\cdot " ++ joinMul (map (latexPrec precPow) rest)
  renderFactors fs = joinMul (map (latexPrec precPow) fs)

  ||| Render a radicand (inside \\sqrt{...}).
  ||| No outer parens needed since braces provide grouping.
  latexRadicand : RadExpr Rational -> String
  latexRadicand (Lit r) =
    let n = numer r
        d = denom r
    in if d == 1 then show n
       else if n < 0 then "-\\frac{" ++ show (abs n) ++ "}{" ++ show d ++ "}"
       else "\\frac{" ++ show n ++ "}{" ++ show d ++ "}"
  latexRadicand e = latexPrec 0 e

  ||| Render the base of a power expression.
  ||| Roots and other compound expressions need grouping for the exponent.
  latexBase : RadExpr Rational -> String
  latexBase e@(Root _ _) = "\\left(" ++ latexPrec 0 e ++ "\\right)"
  latexBase e@(Add _ _)  = "\\left(" ++ latexPrec 0 e ++ "\\right)"
  latexBase e@(Mul _ _)  = "\\left(" ++ latexPrec 0 e ++ "\\right)"
  latexBase e@(Neg _)    = "\\left(" ++ latexPrec 0 e ++ "\\right)"
  latexBase e@(Inv _)    = "\\left(" ++ latexPrec 0 e ++ "\\right)"
  latexBase e = latexPrec precPow e

  ||| Render a radical expression as a LaTeX math-mode string
  ||| with a given precedence context.
  export
  latexPrec : Int -> RadExpr Rational -> String
  latexPrec _ (Lit r) = latexRat r
  latexPrec p (Neg e) = case e of
    Add _ _ =>
      parensIf (p > precAdd) $ renderTerms (map (\(s, t) => (not s, t)) (flattenAdd e))
    Mul (Lit c) rest =>
      latexPrec p (Mul (Lit (negate c)) rest)
    Lit r =>
      latexPrec p (Lit (negate r))
    _ =>
      parensIf (p > precNeg) $ "-" ++ latexPrec precNeg e
  latexPrec p e@(Add _ _) =
    parensIf (p > precAdd) $ renderTerms (flattenAdd e)
  -- a / b
  latexPrec p (Mul a (Inv b)) =
    parensIf (p > precMul) $ "\\frac{" ++ latexPrec 0 a ++ "}{" ++ latexPrec 0 b ++ "}"
  -- (1/a) * b -> b/a
  latexPrec p (Mul (Inv a) b) =
    parensIf (p > precMul) $ "\\frac{" ++ latexPrec 0 b ++ "}{" ++ latexPrec 0 a ++ "}"
  latexPrec p (Inv e) =
    parensIf (p > precMul) $ "\\frac{1}{" ++ latexPrec 0 e ++ "}"
  latexPrec _ (Root 2 (Lit r)) =
    if r == negate Rational.one then "\\mathrm{i}"
    else "\\sqrt{" ++ latexRadicand (Lit r) ++ "}"
  latexPrec _ (Root 2 e) = "\\sqrt{" ++ latexRadicand e ++ "}"
  latexPrec _ (Root n e) = "\\sqrt[" ++ show n ++ "]{" ++ latexRadicand e ++ "}"
  latexPrec _ (Pow _ 0) = "1"
  latexPrec p (Pow e n) =
    if n < 0 then latexPrec p (Inv (Pow e (negate n)))
    else if n == 1 then latexPrec p e
    else parensIf (p > precPow) $ latexBase e ++ "^{" ++ show n ++ "}"
  latexPrec p e@(Mul _ _) =
    parensIf (p > precMul) $ renderFactors (flattenMul e)

||| Render a radical expression as a LaTeX math-mode string.
export
latex : RadExpr Rational -> String
latex = latexPrec 0
