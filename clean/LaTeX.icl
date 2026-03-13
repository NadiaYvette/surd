implementation module LaTeX

import StdEnv
import RadExpr
import Rational
import Data.Integer

// ---------------------------------------------------------------------------
// Precedence levels
// ---------------------------------------------------------------------------

precAdd :== 1
precMul :== 2
precNeg :== 3
precPow :== 4

// ---------------------------------------------------------------------------
// Main entry point
// ---------------------------------------------------------------------------

latex :: !(RadExpr Rational) -> {#Char}
latex e = latexPrec 0 e

latexPrec :: !Int !(RadExpr Rational) -> {#Char}
latexPrec _ (Lit r) = latexRat r
latexPrec p (Neg e) = case e of
    (Add _ _)
        -> parensIf (p > precAdd) (renderTerms (map negTerm (flattenAdd e)))
    (Mul (Lit c) rest)
        -> latexPrec p (Mul (Lit (~ c)) rest)
    (Lit r)
        -> latexPrec p (Lit (~ r))
    _
        -> parensIf (p > precNeg) ("-" +++ latexPrec precNeg e)
latexPrec p e=:(Add _ _) =
    parensIf (p > precAdd) (renderTerms (flattenAdd e))
// a / b
latexPrec p (Mul a (Inv b)) =
    parensIf (p > precMul) ("\\frac{" +++ latexPrec 0 a +++ "}{" +++ latexPrec 0 b +++ "}")
// (1/a) * b  ->  b/a
latexPrec p (Mul (Inv a) b) =
    parensIf (p > precMul) ("\\frac{" +++ latexPrec 0 b +++ "}{" +++ latexPrec 0 a +++ "}")
latexPrec p (Inv e) =
    parensIf (p > precMul) ("\\frac{1}{" +++ latexPrec 0 e +++ "}")
latexPrec _ (Root 2 (Lit r))
    | r == ~ one = "\\mathrm{i}"
latexPrec _ (Root 2 e) = "\\sqrt{" +++ latexRadicand e +++ "}"
latexPrec _ (Root n e) = "\\sqrt[" +++ toString n +++ "]{" +++ latexRadicand e +++ "}"
latexPrec _ (Pow _ 0) = "1"
latexPrec p (Pow e n)
    | n < 0 = latexPrec p (Inv (Pow e (0 - n)))
    | n == 1 = latexPrec p e
    = parensIf (p > precPow) (latexBase e +++ "^{" +++ toString n +++ "}")
latexPrec p e=:(Mul _ _) =
    parensIf (p > precMul) (renderFactors (flattenMul e))

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

negTerm :: !(!Bool, !RadExpr Rational) -> (Bool, RadExpr Rational)
negTerm (s, t) = (not s, t)

parensIf :: !Bool !{#Char} -> {#Char}
parensIf True s = "\\left(" +++ s +++ "\\right)"
parensIf False s = s

latexRat :: !Rational -> {#Char}
latexRat r
    # n = numer r
    # d = denom r
    | d == toInteger 1 = toString n
    | n < toInteger 0 = "-\\frac{" +++ toString (abs n) +++ "}{" +++ toString d +++ "}"
    = "\\frac{" +++ toString n +++ "}{" +++ toString d +++ "}"

// Render the base of a power expression.
// Compound expressions need grouping.
latexBase :: !(RadExpr Rational) -> {#Char}
latexBase e=:(Root _ _) = "\\left(" +++ latexPrec 0 e +++ "\\right)"
latexBase e=:(Add _ _) = "\\left(" +++ latexPrec 0 e +++ "\\right)"
latexBase e=:(Mul _ _) = "\\left(" +++ latexPrec 0 e +++ "\\right)"
latexBase e=:(Neg _) = "\\left(" +++ latexPrec 0 e +++ "\\right)"
latexBase e=:(Inv _) = "\\left(" +++ latexPrec 0 e +++ "\\right)"
latexBase e = latexPrec precPow e

// Render a radicand inside \sqrt{...}.
latexRadicand :: !(RadExpr Rational) -> {#Char}
latexRadicand (Lit r)
    # n = numer r
    # d = denom r
    | d == toInteger 1 = toString n
    | n < toInteger 0 = "-\\frac{" +++ toString (abs n) +++ "}{" +++ toString d +++ "}"
    = "\\frac{" +++ toString n +++ "}{" +++ toString d +++ "}"
latexRadicand e = latexPrec 0 e

// Flatten Add tree with sign tracking.
flattenAdd :: !(RadExpr Rational) -> [(Bool, RadExpr Rational)]
flattenAdd (Add a b) = flattenAdd a ++ flattenAdd b
flattenAdd (Neg e) = map negTerm (flattenAdd e)
flattenAdd (Lit r)
    | r < zero = [(False, Lit (~ r))]
flattenAdd (Mul (Neg a) b) = [(False, Mul a b)]
flattenAdd e=:(Mul _ _) = case flattenMul e of
    [Lit r : rest]
        | r < zero -> [(False, rebuildMul [Lit (~ r) : rest])]
    _ -> [(True, e)]
flattenAdd e = [(True, e)]

rebuildMul :: ![RadExpr Rational] -> RadExpr Rational
rebuildMul [] = Lit one
rebuildMul [x] = x
rebuildMul [x:xs] = foldl Mul x xs

renderTerms :: ![(Bool, RadExpr Rational)] -> {#Char}
renderTerms [] = "0"
renderTerms [(s, t) : rest]
    # hd = if s (latexPrec precAdd t) ("-" +++ latexPrec precMul t)
    = hd +++ concatS (map rr rest)
where
    rr :: !(Bool, RadExpr Rational) -> {#Char}
    rr (True, e) = " + " +++ latexPrec precAdd e
    rr (False, e) = " - " +++ latexPrec precMul e

concatS :: ![{#Char}] -> {#Char}
concatS [] = ""
concatS [x:xs] = x +++ concatS xs

flattenMul :: !(RadExpr Rational) -> [RadExpr Rational]
flattenMul (Mul a b) = flattenMul a ++ flattenMul b
flattenMul e = [e]

renderFactors :: ![RadExpr Rational] -> {#Char}
renderFactors [] = "1"
renderFactors [x] = latexPrec precMul x
renderFactors [Lit c : rest]
    | c == one = joinMul (map (latexPrec precPow) rest)
    | c == ~ one = "-" +++ joinMul (map (latexPrec precPow) rest)
    = latexRat c +++ " \\cdot " +++ joinMul (map (latexPrec precPow) rest)
renderFactors fs = joinMul (map (latexPrec precPow) fs)

joinMul :: ![{#Char}] -> {#Char}
joinMul [] = ""
joinMul [x] = x
joinMul [x:xs] = x +++ concatS (map (\s -> " \\cdot " +++ s) xs)
