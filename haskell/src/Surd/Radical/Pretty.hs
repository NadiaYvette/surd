-- | Pretty-printing radical expressions in human-readable mathematical notation.
--
-- Includes common subexpression elimination (CSE) for readability:
-- repeated subexpressions are named and shown as @let@ bindings.
module Surd.Radical.Pretty
  ( pretty
  , prettyPrec
  , prettyCSE
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ratio (numerator, denominator)
import Surd.Types

-- | Render a radical expression as a human-readable string.
pretty :: RadExpr Rational -> String
pretty = prettyPrec 0

-- --------------------------------------------------------------------------
-- CSE pretty-printing
-- --------------------------------------------------------------------------

-- | Render with CSE: repeated subexpressions are shown as named intermediates.
prettyCSE :: RadExpr Rational -> String
prettyCSE expr =
  let -- Count subexpression occurrences
      counts = countSubs expr
      -- Keep subexpressions that appear 2+ times and are worth naming
      shared = Map.keys $ Map.filter (>= 2) $
               Map.filterWithKey (\k _ -> worthNaming k) counts
      -- Sort by size (smaller first → defined before larger ones that use them)
      sorted = sortBySize shared
      -- Assign names
      nameList = zipWith (\i _ -> varName i) [0..] sorted
      nameMap = Map.fromList (zip sorted nameList)
      -- Build bindings: each binding can reference earlier-defined names
      bindings = zipWith (\sub name ->
        let availNames = Map.filterWithKey (\k _ -> k /= sub) nameMap
        in "  " ++ name ++ " = " ++ renderWith availNames sub
        ) sorted nameList
      body = renderWith nameMap expr
  in case bindings of
    [] -> body
    _  -> "let\n" ++ unlines bindings ++ "in " ++ body

-- | Generate variable names: a, b, ..., z, a1, b1, ..., z1, a2, ...
varName :: Int -> String
varName i
  | i < 26    = [toEnum (fromEnum 'a' + i)]
  | otherwise = [toEnum (fromEnum 'a' + (i `mod` 26))] ++ show (i `div` 26)

-- | Count occurrences of every subexpression in the tree.
countSubs :: Ord k => RadExpr k -> Map (RadExpr k) Int
countSubs = go Map.empty
  where
    go m e = let m' = Map.insertWith (+) e 1 m in case e of
      Lit _     -> m'
      Neg a     -> go m' a
      Add a b   -> go (go m' a) b
      Mul a b   -> go (go m' a) b
      Inv a     -> go m' a
      Root _ a  -> go m' a
      Pow a _   -> go m' a

-- | Is a subexpression complex enough to be worth naming?
worthNaming :: RadExpr k -> Bool
worthNaming (Lit _)       = False
worthNaming (Neg (Lit _)) = False
worthNaming (Root _ (Lit _)) = False
worthNaming _             = True

-- | Sort subexpressions by size (number of nodes), smallest first.
sortBySize :: [RadExpr Rational] -> [RadExpr Rational]
sortBySize = map snd . sortPairs . map (\e -> (exprSize e, e))
  where
    sortPairs [] = []
    sortPairs (x:xs) = sortPairs [y | y <- xs, fst y <= fst x]
                    ++ [x]
                    ++ sortPairs [y | y <- xs, fst y > fst x]

exprSize :: RadExpr k -> Int
exprSize (Lit _)     = 1
exprSize (Neg a)     = 1 + exprSize a
exprSize (Add a b)   = 1 + exprSize a + exprSize b
exprSize (Mul a b)   = 1 + exprSize a + exprSize b
exprSize (Inv a)     = 1 + exprSize a
exprSize (Root _ a)  = 1 + exprSize a
exprSize (Pow a _)   = 1 + exprSize a

-- | Pretty-print substituting names for shared subexpressions.
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

    pp _ (Root 2 e) = "√" ++ radicand e
    pp _ (Root 3 e) = "∛" ++ radicand e
    pp _ (Root n e) = show n ++ "√" ++ radicand e

    pp p (Pow e n) =
      parensIf (p > precPow) $ go precPow e ++ "^" ++ show n

    radicand e@(Lit r)
      | r >= 0 && denominator r == 1 = show (numerator r)
      | otherwise = "(" ++ go 0 e ++ ")"
    radicand e
      | isSimple e = go precPow e
      | otherwise  = "(" ++ go 0 e ++ ")"

    isSimple (Lit _)     = True
    isSimple (Root _ _)  = True
    isSimple e           = Map.member e names  -- named subexpressions are "atomic"

    flattenAdd (Add a b) = flattenAdd a ++ flattenAdd b
    flattenAdd (Neg e)   = map (\(s, t) -> (not s, t)) (flattenAdd e)
    flattenAdd (Lit r) | r < 0 = [(False, Lit (negate r))]
    flattenAdd e = [(True, e)]

    renderTerms [] = "0"
    renderTerms ((s, t):rest) =
      let first = if s then go precAdd t else "-" ++ go precNeg t
      in first ++ concatMap renderRest rest
    renderRest (True,  e) = " + " ++ go precAdd e
    renderRest (False, e) = " - " ++ go precNeg e

    flattenMul (Mul a b) = flattenMul a ++ flattenMul b
    flattenMul e         = [e]

    renderFactors [] = "1"
    renderFactors [x] = go precMul x
    renderFactors (Lit c : rest)
      | c == 1    = joinWith "·" (map (go precPow) rest)
      | c == -1   = "-" ++ joinWith "·" (map (go precPow) rest)
      | otherwise = prettyRat c ++ "·" ++ joinWith "·" (map (go precPow) rest)
    renderFactors fs = joinWith "·" (map (go precPow) fs)

-- --------------------------------------------------------------------------
-- Basic (non-CSE) pretty-printing
-- --------------------------------------------------------------------------

precAdd, precMul, precNeg, precPow, precAtom :: Int
precAdd = 1
precMul = 2
precNeg = 3
precPow = 4
precAtom = 5

prettyPrec :: Int -> RadExpr Rational -> String
prettyPrec _ (Lit r) = prettyRat r
prettyPrec p (Neg e) =
  parensIf (p > precNeg) $ "-" ++ prettyPrec precNeg e
prettyPrec p e@(Add _ _) =
  parensIf (p > precAdd) $ renderTermsBasic (flattenAddBasic e)
prettyPrec p (Mul a (Inv b)) =
  parensIf (p > precMul) $
    prettyPrec precMul a ++ "/" ++ prettyPrec precPow b
prettyPrec p e@(Mul _ _) =
  parensIf (p > precMul) $ renderFactorsBasic (flattenMulBasic e)
prettyPrec p (Inv e) =
  parensIf (p > precMul) $ "1/" ++ prettyPrec precPow e
prettyPrec _ (Root 2 e) = "√" ++ prettyRadicandBasic e
prettyPrec _ (Root 3 e) = "∛" ++ prettyRadicandBasic e
prettyPrec _ (Root n e) = show n ++ "√" ++ prettyRadicandBasic e
prettyPrec p (Pow e n) =
  parensIf (p > precPow) $ prettyPrec precPow e ++ "^" ++ show n

-- --------------------------------------------------------------------------
-- Shared helpers
-- --------------------------------------------------------------------------

parensIf :: Bool -> String -> String
parensIf True  s = "(" ++ s ++ ")"
parensIf False s = s

prettyRat :: Rational -> String
prettyRat r
  | d == 1 && n' >= 0 = show n'
  | d == 1            = "(" ++ show n' ++ ")"
  | n' < 0            = "(-" ++ show (abs n') ++ "/" ++ show d ++ ")"
  | otherwise          = "(" ++ show n' ++ "/" ++ show d ++ ")"
  where
    n' = numerator r
    d  = denominator r

joinWith :: String -> [String] -> String
joinWith _ []     = ""
joinWith _ [x]    = x
joinWith sep (x:xs) = x ++ concatMap (sep ++) xs

-- Basic helpers (no CSE)

prettyRadicandBasic :: RadExpr Rational -> String
prettyRadicandBasic (Lit r)
  | r >= 0 && denominator r == 1 = show (numerator r)
prettyRadicandBasic e
  | isSimpleBasic e = prettyPrec precPow e
  | otherwise       = "(" ++ pretty e ++ ")"
  where
    isSimpleBasic (Lit _)    = True
    isSimpleBasic (Root _ _) = True
    isSimpleBasic _          = False

flattenAddBasic :: RadExpr Rational -> [(Bool, RadExpr Rational)]
flattenAddBasic (Add a b) = flattenAddBasic a ++ flattenAddBasic b
flattenAddBasic (Neg e)   = map (\(s, t) -> (not s, t)) (flattenAddBasic e)
flattenAddBasic (Lit r) | r < 0 = [(False, Lit (negate r))]
flattenAddBasic e = [(True, e)]

renderTermsBasic :: [(Bool, RadExpr Rational)] -> String
renderTermsBasic [] = "0"
renderTermsBasic ((s, t):rest) =
  let first = if s then prettyPrec precAdd t else "-" ++ prettyPrec precNeg t
  in first ++ concatMap rr rest
  where
    rr (True,  e) = " + " ++ prettyPrec precAdd e
    rr (False, e) = " - " ++ prettyPrec precNeg e

flattenMulBasic :: RadExpr Rational -> [RadExpr Rational]
flattenMulBasic (Mul a b) = flattenMulBasic a ++ flattenMulBasic b
flattenMulBasic e         = [e]

renderFactorsBasic :: [RadExpr Rational] -> String
renderFactorsBasic [] = "1"
renderFactorsBasic [x] = prettyPrec precMul x
renderFactorsBasic (Lit c : rest)
  | c == 1    = joinWith "·" (map (prettyPrec precPow) rest)
  | c == -1   = "-" ++ joinWith "·" (map (prettyPrec precPow) rest)
  | otherwise = prettyRat c ++ "·" ++ joinWith "·" (map (prettyPrec precPow) rest)
renderFactorsBasic fs = joinWith "·" (map (prettyPrec precPow) fs)
