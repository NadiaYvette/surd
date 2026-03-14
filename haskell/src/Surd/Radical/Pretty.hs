-- |
-- Module      : Surd.Radical.Pretty
-- Description : Human-readable mathematical notation for radical expressions
-- Stability   : experimental
--
-- Renders 'RadExpr' values as human-readable strings using standard
-- mathematical notation: @sqrt@ symbols, @*@ for multiplication (rendered
-- as a middle dot), fraction notation, and so on.
--
-- Two rendering modes are provided:
--
-- * 'pretty' / 'prettyPrec' -- direct rendering of the expression tree,
--   with precedence-based parenthesization.
-- * 'prettyCSE' -- common subexpression elimination: repeated
--   subexpressions are named (as @let@ bindings) and referenced by name,
--   improving readability for large expressions with shared structure.
--
-- === Precedence levels
--
-- The renderer uses five precedence levels (from lowest to highest):
--
-- @
-- precAdd  = 1  -- addition, subtraction
-- precMul  = 2  -- multiplication, division
-- precNeg  = 3  -- unary negation
-- precPow  = 4  -- exponentiation
-- precAtom = 5  -- atoms (literals, named subexpressions)
-- @
module Surd.Radical.Pretty
  ( -- * Rendering
    pretty,
    prettyPrec,
    prettyCSE,
  )
where

import Data.Bifunctor qualified as Bifunctor
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ratio (denominator, numerator)
import Surd.Types

-- | Render a radical expression as a human-readable string.
--
-- Uses standard mathematical notation: @sqrt(n)@ for square roots (with
-- Unicode sqrt symbol), middle dot for multiplication, and minimal
-- parentheses based on operator precedence.
--
-- >>> pretty (Add (Lit 3) (Mul (Lit 2) (Root 2 (Lit 5))))
-- "3 + 2*sqrt(5)"
pretty :: RadExpr Rational -> String
pretty = prettyPrec 0

-- --------------------------------------------------------------------------
-- CSE pretty-printing
-- --------------------------------------------------------------------------

-- | Render with common subexpression elimination.
--
-- Subexpressions that appear 2 or more times in the tree are assigned
-- names (@a@, @b@, ..., @z@, @a1@, @b1@, ...) and shown as @let@
-- bindings before the main expression. Trivial subexpressions (literals,
-- negated literals, simple roots) are not named.
--
-- For example, if @sqrt 5@ appears in multiple places:
--
-- @
-- let
--   a = sqrt 5
-- in 3*a + 2*a
-- @
prettyCSE :: RadExpr Rational -> String
prettyCSE expr =
  let -- Count subexpression occurrences
      counts = countSubs expr
      -- Keep subexpressions that appear 2+ times and are worth naming
      shared =
        Map.keys $
          Map.filter (>= 2) $
            Map.filterWithKey (\k _ -> worthNaming k) counts
      -- Sort by size (smaller first -> defined before larger ones that use them)
      sorted = sortBySize shared
      -- Assign names
      nameList = zipWith (\i _ -> varName i) [0 ..] sorted
      nameMap = Map.fromList (zip sorted nameList)
      -- Build bindings: each binding can reference earlier-defined names
      bindings =
        zipWith
          ( \sub name ->
              let availNames = Map.filterWithKey (\k _ -> k /= sub) nameMap
               in "  " ++ name ++ " = " ++ renderWith availNames sub
          )
          sorted
          nameList
      body = renderWith nameMap expr
   in case bindings of
        [] -> body
        _ -> "let\n" ++ unlines bindings ++ "in " ++ body

-- | Generate variable names: a, b, ..., z, a1, b1, ..., z1, a2, ...
varName :: Int -> String
varName i
  | i < 26 = [toEnum (fromEnum 'a' + i)]
  | otherwise = toEnum (fromEnum 'a' + (i `mod` 26)) : show (i `div` 26)

-- | Count occurrences of every subexpression in the tree.
countSubs :: (Ord k) => RadExpr k -> Map (RadExpr k) Int
countSubs = go Map.empty
  where
    go m e =
      let m' = Map.insertWith (+) e 1 m
       in case e of
            Lit _ -> m'
            Neg a -> go m' a
            Add a b -> go (go m' a) b
            Mul a b -> go (go m' a) b
            Inv a -> go m' a
            Root _ a -> go m' a
            Pow a _ -> go m' a

-- | Is a subexpression complex enough to be worth naming?
worthNaming :: RadExpr k -> Bool
worthNaming (Lit _) = False
worthNaming (Neg (Lit _)) = False
worthNaming (Root _ (Lit _)) = False
worthNaming _ = True

-- | Sort subexpressions by size (number of nodes), smallest first.
sortBySize :: [RadExpr Rational] -> [RadExpr Rational]
sortBySize = map snd . sortPairs . map (\e -> (exprSize e, e))
  where
    sortPairs [] = []
    sortPairs (x : xs) =
      sortPairs [y | y <- xs, fst y <= fst x]
        ++ [x]
        ++ sortPairs [y | y <- xs, fst y > fst x]

-- | Count the number of nodes in an expression tree (used for sorting
-- subexpressions by complexity in 'prettyCSE').
exprSize :: RadExpr k -> Int
exprSize (Lit _) = 1
exprSize (Neg a) = 1 + exprSize a
exprSize (Add a b) = 1 + exprSize a + exprSize b
exprSize (Mul a b) = 1 + exprSize a + exprSize b
exprSize (Inv a) = 1 + exprSize a
exprSize (Root _ a) = 1 + exprSize a
exprSize (Pow a _) = 1 + exprSize a

-- | Pretty-print an expression, substituting named variables for
-- shared subexpressions found in the given map.  This is used by
-- 'prettyCSE' after shared subexpressions have been identified and
-- assigned names.
renderWith :: Map (RadExpr Rational) String -> RadExpr Rational -> String
renderWith names = go 0
  where
    go p e
      | Just name <- Map.lookup e names = parensIf (p > precAtom) name
    go p e = pp p e

    pp _ (Lit r) = prettyRat r
    pp p (Neg e) =
      parensIf (p > precNeg) $ "-" ++ go precNeg e
    pp p e@(Add _ _) =
      parensIf (p > precAdd) $ renderTerms (flattenAdd e)
    pp p (Mul a (Inv b)) =
      parensIf (p > precMul) $ go precMul a ++ "/" ++ go precPow b
    pp p e@(Mul _ _) =
      parensIf (p > precMul) $ renderFactors (flattenMul e)
    pp p (Inv e) =
      parensIf (p > precMul) $ "1/" ++ go precPow e
    pp _ (Root 2 (Lit (-1))) = "i"
    pp _ (Root 2 e) = "\x221A" ++ radicand e
    pp _ (Root 3 e) = "\x221B" ++ radicand e
    pp _ (Root n e) = show n ++ "\x221A" ++ radicand e
    pp _ (Pow _ 0) = "1"
    pp p (Pow e n)
      | n < 0 = pp p (Inv (Pow e (negate n)))
      | n == 1 = go p e
      | otherwise = parensIf (p > precPow) $ go precPow e ++ "^" ++ show n

    radicand e@(Lit r)
      | r >= 0 && denominator r == 1 = show (numerator r)
      | otherwise = "(" ++ go 0 e ++ ")"
    radicand e
      | isSimple e = go precPow e
      | otherwise = "(" ++ go 0 e ++ ")"

    isSimple (Lit _) = True
    isSimple (Root _ _) = True
    isSimple e = Map.member e names -- named subexpressions are "atomic"
    flattenAdd (Add a b) = flattenAdd a ++ flattenAdd b
    flattenAdd (Neg e) = map (Bifunctor.first not) (flattenAdd e)
    flattenAdd (Lit r) | r < 0 = [(False, Lit (negate r))]
    flattenAdd (Mul (Neg a) b) = [(False, Mul a b)]
    flattenAdd e@(Mul _ _) = case flattenMul e of
      (Lit r : rest) | r < 0 -> [(False, rebuildMul (Lit (negate r) : rest))]
      _ -> [(True, e)]
    flattenAdd e = [(True, e)]

    rebuildMul [] = Lit 1
    rebuildMul [x] = x
    rebuildMul (x : xs) = foldl Mul x xs

    renderTerms [] = "0"
    renderTerms ((s, t) : rest) =
      let hd = if s then go precAdd t else "-" ++ go precMul t
       in hd ++ concatMap renderRest rest
    renderRest (True, e) = " + " ++ go precAdd e
    renderRest (False, e) = " - " ++ go precMul e

    flattenMul (Mul a b) = flattenMul a ++ flattenMul b
    flattenMul e = [e]

    renderFactors [] = "1"
    renderFactors [x] = go precMul x
    renderFactors (Lit c : rest)
      | c == 1 = joinWith "\xB7" (map (go precPow) rest)
      | c == -1 = "-" ++ joinWith "\xB7" (map (go precPow) rest)
      | otherwise = prettyRat c ++ "\xB7" ++ joinWith "\xB7" (map (go precPow) rest)
    renderFactors fs = joinWith "\xB7" (map (go precPow) fs)

-- --------------------------------------------------------------------------
-- Basic (non-CSE) pretty-printing
-- --------------------------------------------------------------------------

-- | Precedence levels used by both 'prettyPrec' and 'prettyCSE'.
precAdd, precMul, precNeg, precPow, precAtom :: Int
-- | Addition/subtraction precedence (lowest).
precAdd = 1
-- | Multiplication/division precedence.
precMul = 2
-- | Unary negation precedence.
precNeg = 3
-- | Exponentiation precedence.
precPow = 4
-- | Atomic expression precedence (highest -- never parenthesized).
precAtom = 5

-- | Render a radical expression with a given surrounding precedence level.
--
-- Inserts parentheses only when the current expression's precedence is
-- lower than the surrounding context. Pass @0@ for the outermost call.
prettyPrec :: Int -> RadExpr Rational -> String
prettyPrec _ (Lit r) = prettyRat r
prettyPrec p (Neg e) = case e of
  -- Neg of a sum: distribute the sign into terms
  Add _ _ ->
    parensIf (p > precAdd) $ renderTermsBasic (map negTerm (flattenAddBasic e))
  -- Neg of a product with literal coefficient: absorb sign
  Mul (Lit c) rest ->
    prettyPrec p (Mul (Lit (negate c)) rest)
  -- Neg of a literal: just negate
  Lit r ->
    prettyPrec p (Lit (negate r))
  -- Otherwise: prefix minus
  _ ->
    parensIf (p > precNeg) $ "-" ++ prettyPrec precNeg e
  where
    negTerm (s, t) = (not s, t)
prettyPrec p e@(Add _ _) =
  parensIf (p > precAdd) $ renderTermsBasic (flattenAddBasic e)
prettyPrec p (Mul a (Inv b)) =
  parensIf (p > precMul) $
    prettyPrec precMul a ++ "/" ++ prettyPrec precPow b
prettyPrec p e@(Mul _ _) =
  parensIf (p > precMul) $ renderFactorsBasic (flattenMulBasic e)
prettyPrec p (Inv e) =
  parensIf (p > precMul) $ "1/" ++ prettyPrec precPow e
prettyPrec _ (Root 2 (Lit (-1))) = "i"
prettyPrec _ (Root 2 e) = "\x221A" ++ prettyRadicandBasic e
prettyPrec _ (Root 3 e) = "\x221B" ++ prettyRadicandBasic e
prettyPrec _ (Root n e) = show n ++ "\x221A" ++ prettyRadicandBasic e
prettyPrec _ (Pow _ 0) = "1"
prettyPrec p (Pow e n)
  | n < 0 = prettyPrec p (Inv (Pow e (negate n)))
  | n == 1 = prettyPrec p e
  | otherwise = parensIf (p > precPow) $ prettyPrec precPow e ++ "^" ++ show n

-- --------------------------------------------------------------------------
-- Shared helpers
-- --------------------------------------------------------------------------

-- | Wrap a string in parentheses if the condition is true.
parensIf :: Bool -> String -> String
parensIf True s = "(" ++ s ++ ")"
parensIf False s = s

-- | Render a rational number, using parentheses for non-integer fractions.
prettyRat :: Rational -> String
prettyRat r
  | d == 1 = show n'
  | n' < 0 = "(-" ++ show (abs n') ++ "/" ++ show d ++ ")"
  | otherwise = "(" ++ show n' ++ "/" ++ show d ++ ")"
  where
    n' = numerator r
    d = denominator r

-- | Join a list of strings with a separator (e.g. middle dot for multiplication).
joinWith :: String -> [String] -> String
joinWith _ [] = ""
joinWith _ [x] = x
joinWith sep (x : xs) = x ++ concatMap (sep ++) xs

-- Basic helpers (no CSE)

-- | Render a radicand for the basic (non-CSE) renderer.  Simple
-- expressions (literals, roots) are rendered without parentheses;
-- compound expressions are parenthesized.
prettyRadicandBasic :: RadExpr Rational -> String
prettyRadicandBasic (Lit r)
  | r >= 0 && denominator r == 1 = show (numerator r)
prettyRadicandBasic e
  | isSimpleBasic e = prettyPrec precPow e
  | otherwise = "(" ++ pretty e ++ ")"
  where
    isSimpleBasic (Lit _) = True
    isSimpleBasic (Root _ _) = True
    isSimpleBasic _ = False

-- | Flatten an additive chain into signed terms (basic renderer).
flattenAddBasic :: RadExpr Rational -> [(Bool, RadExpr Rational)]
flattenAddBasic (Add a b) = flattenAddBasic a ++ flattenAddBasic b
flattenAddBasic (Neg e) = map (Bifunctor.first not) (flattenAddBasic e)
flattenAddBasic (Lit r) | r < 0 = [(False, Lit (negate r))]
flattenAddBasic (Mul (Neg a) b) = [(False, Mul a b)]
flattenAddBasic e@(Mul _ _) = case flattenMulBasic e of
  (Lit r : rest) | r < 0 -> [(False, rebuildMul (Lit (negate r) : rest))]
  _ -> [(True, e)]
  where
    rebuildMul [] = Lit 1
    rebuildMul [x] = x
    rebuildMul (x : xs) = foldl Mul x xs
flattenAddBasic e = [(True, e)]

-- | Render a list of signed terms as an addition/subtraction chain (basic renderer).
renderTermsBasic :: [(Bool, RadExpr Rational)] -> String
renderTermsBasic [] = "0"
renderTermsBasic ((s, t) : rest) =
  let hd = if s then prettyPrec precAdd t else "-" ++ prettyPrec precMul t
   in hd ++ concatMap rr rest
  where
    rr (True, e) = " + " ++ prettyPrec precAdd e
    rr (False, e) = " - " ++ prettyPrec precMul e

-- | Flatten a multiplicative chain into a list of factors (basic renderer).
flattenMulBasic :: RadExpr Rational -> [RadExpr Rational]
flattenMulBasic (Mul a b) = flattenMulBasic a ++ flattenMulBasic b
flattenMulBasic e = [e]

-- | Render a list of factors as a product with middle-dot separators (basic renderer).
-- A leading coefficient of @1@ is elided; @-1@ renders as a prefix @-@.
renderFactorsBasic :: [RadExpr Rational] -> String
renderFactorsBasic [] = "1"
renderFactorsBasic [x] = prettyPrec precMul x
renderFactorsBasic (Lit c : rest)
  | c == 1 = joinWith "\xB7" (map (prettyPrec precPow) rest)
  | c == -1 = "-" ++ joinWith "\xB7" (map (prettyPrec precPow) rest)
  | otherwise = prettyRat c ++ "\xB7" ++ joinWith "\xB7" (map (prettyPrec precPow) rest)
renderFactorsBasic fs = joinWith "\xB7" (map (prettyPrec precPow) fs)
