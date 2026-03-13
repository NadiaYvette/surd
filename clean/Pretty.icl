implementation module Pretty

import StdEnv
import RadExpr
import Rational
import Data.Integer
from Data.Map import :: Map(..), newMap, get, put, del, fromList, foldrWithKey

mapToList :: !(Map k v) -> [(k, v)]
mapToList m = foldrWithKey (\k v acc -> [(k, v) : acc]) [] m

// ---------------------------------------------------------------------------
// Precedence levels
// ---------------------------------------------------------------------------

precAdd :== 1
precMul :== 2
precNeg :== 3
precPow :== 4
precAtom :== 5

// ---------------------------------------------------------------------------
// Basic (non-CSE) pretty-printing
// ---------------------------------------------------------------------------

pretty :: !(RadExpr Rational) -> {#Char}
pretty e = prettyPrec 0 e

prettyPrec :: !Int !(RadExpr Rational) -> {#Char}
prettyPrec _ (Lit r) = prettyRat r
prettyPrec p (Neg e) = case e of
    (Add _ _)
        -> parensIf (p > precAdd) (renderTermsBasic (map negTerm (flattenAddBasic e)))
    (Mul (Lit c) rest)
        -> prettyPrec p (Mul (Lit (~ c)) rest)
    (Lit r)
        -> prettyPrec p (Lit (~ r))
    _
        -> parensIf (p > precNeg) ("-" +++ prettyPrec precNeg e)
prettyPrec p e=:(Add _ _) =
    parensIf (p > precAdd) (renderTermsBasic (flattenAddBasic e))
prettyPrec p (Mul a (Inv b)) =
    parensIf (p > precMul) (prettyPrec precMul a +++ "/" +++ prettyPrec precPow b)
prettyPrec p e=:(Mul _ _) =
    parensIf (p > precMul) (renderFactorsBasic (flattenMulBasic e))
prettyPrec p (Inv e) =
    parensIf (p > precMul) ("1/" +++ prettyPrec precPow e)
prettyPrec _ (Root 2 (Lit r))
    | r == ~ one = "i"
prettyPrec _ (Root 2 e) = "\xE2\x88\x9A" +++ prettyRadicandBasic e   // √ (UTF-8)
prettyPrec _ (Root 3 e) = "\xE2\x88\x9B" +++ prettyRadicandBasic e   // ∛ (UTF-8)
prettyPrec _ (Root n e) = toString n +++ "\xE2\x88\x9A" +++ prettyRadicandBasic e
prettyPrec _ (Pow _ 0) = "1"
prettyPrec p (Pow e n)
    | n < 0 = prettyPrec p (Inv (Pow e (0 - n)))
    | n == 1 = prettyPrec p e
    = parensIf (p > precPow) (prettyPrec precPow e +++ "^" +++ toString n)

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

negTerm :: !(!Bool, !RadExpr Rational) -> (Bool, RadExpr Rational)
negTerm (s, t) = (not s, t)

parensIf :: !Bool !{#Char} -> {#Char}
parensIf True s = "(" +++ s +++ ")"
parensIf False s = s

prettyRat :: !Rational -> {#Char}
prettyRat r
    # n = numer r
    # d = denom r
    | d == one = toString n
    | n < zero = "(-" +++ toString (abs n) +++ "/" +++ toString d +++ ")"
    = "(" +++ toString n +++ "/" +++ toString d +++ ")"
where
    zero = toInteger 0
    one = toInteger 1

prettyRadicandBasic :: !(RadExpr Rational) -> {#Char}
prettyRadicandBasic (Lit r)
    | isNonNegInt r = toString (numer r)
where
    isNonNegInt :: !Rational -> Bool
    isNonNegInt r = denom r == toInteger 1 && not (numer r < toInteger 0)
prettyRadicandBasic e
    | isSimpleBasic e = prettyPrec precPow e
    = "(" +++ pretty e +++ ")"

isSimpleBasic :: !(RadExpr Rational) -> Bool
isSimpleBasic (Lit _) = True
isSimpleBasic (Root _ _) = True
isSimpleBasic _ = False

flattenAddBasic :: !(RadExpr Rational) -> [(Bool, RadExpr Rational)]
flattenAddBasic (Add a b) = flattenAddBasic a ++ flattenAddBasic b
flattenAddBasic (Neg e) = map negTerm (flattenAddBasic e)
flattenAddBasic (Lit r)
    | r < zero = [(False, Lit (~ r))]
flattenAddBasic (Mul (Neg a) b) = [(False, Mul a b)]
flattenAddBasic e=:(Mul _ _) = case flattenMulBasic e of
    [Lit r : rest]
        | r < zero -> [(False, rebuildMulList [Lit (~ r) : rest])]
    _ -> [(True, e)]
flattenAddBasic e = [(True, e)]

rebuildMulList :: ![RadExpr Rational] -> RadExpr Rational
rebuildMulList [] = Lit one
rebuildMulList [x] = x
rebuildMulList [x:xs] = foldl Mul x xs

renderTermsBasic :: ![(Bool, RadExpr Rational)] -> {#Char}
renderTermsBasic [] = "0"
renderTermsBasic [(s, t) : rest]
    # hd = if s (prettyPrec precAdd t) ("-" +++ prettyPrec precMul t)
    = hd +++ concatS (map renderRest rest)
where
    renderRest :: !(Bool, RadExpr Rational) -> {#Char}
    renderRest (True, e) = " + " +++ prettyPrec precAdd e
    renderRest (False, e) = " - " +++ prettyPrec precMul e

flattenMulBasic :: !(RadExpr Rational) -> [RadExpr Rational]
flattenMulBasic (Mul a b) = flattenMulBasic a ++ flattenMulBasic b
flattenMulBasic e = [e]

renderFactorsBasic :: ![RadExpr Rational] -> {#Char}
renderFactorsBasic [] = "1"
renderFactorsBasic [x] = prettyPrec precMul x
renderFactorsBasic [Lit c : rest]
    | c == one  = joinWith "\xC2\xB7" (map (prettyPrec precPow) rest)  // · (UTF-8)
    | c == ~ one = "-" +++ joinWith "\xC2\xB7" (map (prettyPrec precPow) rest)
    = prettyRat c +++ "\xC2\xB7" +++ joinWith "\xC2\xB7" (map (prettyPrec precPow) rest)
renderFactorsBasic fs = joinWith "\xC2\xB7" (map (prettyPrec precPow) fs)

joinWith :: !{#Char} ![{#Char}] -> {#Char}
joinWith _ [] = ""
joinWith _ [x] = x
joinWith sep [x:xs] = x +++ concatS (map (\s -> sep +++ s) xs)

concatS :: ![{#Char}] -> {#Char}
concatS [] = ""
concatS [x:xs] = x +++ concatS xs

// ---------------------------------------------------------------------------
// CSE pretty-printing
// ---------------------------------------------------------------------------

prettyCSE :: !(RadExpr Rational) -> {#Char}
prettyCSE expr
    # counts = countSubs expr
    # shared = [k \\ (k, v) <- mapToList counts | v >= 2 && worthNaming k]
    # sorted = sortByExprSize shared
    # nameList = [varName i \\ i <- [0..] & _ <- sorted]
    # nameMap = fromList [(e, n) \\ e <- sorted & n <- nameList]
    # bindings = [mkBinding sub name nameMap \\ sub <- sorted & name <- nameList]
    # body = renderWith nameMap expr
    = case bindings of
        [] -> body
        _  -> "let\n" +++ concatS (map (\b -> b +++ "\n") bindings) +++ "in " +++ body
where
    mkBinding :: !(RadExpr Rational) !{#Char} !(Map (RadExpr Rational) {#Char}) -> {#Char}
    mkBinding sub name nameMap
        # availNames = del sub nameMap
        = "  " +++ name +++ " = " +++ renderWith availNames sub

varName :: !Int -> {#Char}
varName i
    | i < 26 = toString (toChar (toInt 'a' + i))
    = toString (toChar (toInt 'a' + (i rem 26))) +++ toString (i / 26)

countSubs :: !(RadExpr Rational) -> Map (RadExpr Rational) Int
countSubs expr = go newMap expr
where
    go :: !(Map (RadExpr Rational) Int) !(RadExpr Rational) -> Map (RadExpr Rational) Int
    go m e
        # m` = case get e m of
            ?Just n -> put e (n + 1) m
            ?None   -> put e 1 m
        = case e of
            (Lit _)    -> m`
            (Neg a)    -> go m` a
            (Add a b)  -> go (go m` a) b
            (Mul a b)  -> go (go m` a) b
            (Inv a)    -> go m` a
            (Root _ a) -> go m` a
            (Pow a _)  -> go m` a

worthNaming :: !(RadExpr Rational) -> Bool
worthNaming (Lit _) = False
worthNaming (Neg (Lit _)) = False
worthNaming (Root _ (Lit _)) = False
worthNaming _ = True

sortByExprSize :: ![RadExpr Rational] -> [RadExpr Rational]
sortByExprSize xs = map snd (sort [(exprSize e, e) \\ e <- xs])

renderWith :: !(Map (RadExpr Rational) {#Char}) !(RadExpr Rational) -> {#Char}
renderWith names expr = go 0 expr
where
    go :: !Int !(RadExpr Rational) -> {#Char}
    go p e = case get e names of
        ?Just name -> parensIf (p > precAtom) name
        ?None      -> pp p e

    pp :: !Int !(RadExpr Rational) -> {#Char}
    pp _ (Lit r) = prettyRat r
    pp p (Neg e) = parensIf (p > precNeg) ("-" +++ go precNeg e)
    pp p e=:(Add _ _)
        = parensIf (p > precAdd) (renderTermsCSE names (flattenAddCSE names e))
    pp p (Mul a (Inv b))
        = parensIf (p > precMul) (go precMul a +++ "/" +++ go precPow b)
    pp p e=:(Mul _ _)
        = parensIf (p > precMul) (renderFactorsCSE names (flattenMulCSE e))
    pp p (Inv e) = parensIf (p > precMul) ("1/" +++ go precPow e)
    pp _ (Root 2 (Lit r))
        | r == ~ one = "i"
    pp _ (Root 2 e) = "\xE2\x88\x9A" +++ radicandCSE names e
    pp _ (Root 3 e) = "\xE2\x88\x9B" +++ radicandCSE names e
    pp _ (Root n e) = toString n +++ "\xE2\x88\x9A" +++ radicandCSE names e
    pp _ (Pow _ 0) = "1"
    pp p (Pow e n)
        | n < 0 = pp p (Inv (Pow e (0 - n)))
        | n == 1 = go p e
        = parensIf (p > precPow) (go precPow e +++ "^" +++ toString n)

radicandCSE :: !(Map (RadExpr Rational) {#Char}) !(RadExpr Rational) -> {#Char}
radicandCSE names (Lit r)
    | isNonNegIntR r = toString (numer r)
where
    isNonNegIntR :: !Rational -> Bool
    isNonNegIntR r = denom r == toInteger 1 && not (numer r < toInteger 0)
radicandCSE names e
    | isSimpleCSE names e = renderWith names e
    = "(" +++ renderWith names e +++ ")"
where
    isSimpleCSE :: !(Map (RadExpr Rational) {#Char}) !(RadExpr Rational) -> Bool
    isSimpleCSE _ (Lit _) = True
    isSimpleCSE _ (Root _ _) = True
    isSimpleCSE names e = case get e names of
        ?Just _ -> True
        ?None   -> False

flattenAddCSE :: !(Map (RadExpr Rational) {#Char}) !(RadExpr Rational) -> [(Bool, RadExpr Rational)]
flattenAddCSE names (Add a b) = flattenAddCSE names a ++ flattenAddCSE names b
flattenAddCSE names (Neg e) = map negTerm (flattenAddCSE names e)
flattenAddCSE names (Lit r)
    | r < zero = [(False, Lit (~ r))]
flattenAddCSE names (Mul (Neg a) b) = [(False, Mul a b)]
flattenAddCSE names e=:(Mul _ _) = case flattenMulCSE e of
    [Lit r : rest]
        | r < zero -> [(False, rebuildMulList [Lit (~ r) : rest])]
    _ -> [(True, e)]
flattenAddCSE _ e = [(True, e)]

flattenMulCSE :: !(RadExpr Rational) -> [RadExpr Rational]
flattenMulCSE (Mul a b) = flattenMulCSE a ++ flattenMulCSE b
flattenMulCSE e = [e]

renderTermsCSE :: !(Map (RadExpr Rational) {#Char}) ![(Bool, RadExpr Rational)] -> {#Char}
renderTermsCSE _ [] = "0"
renderTermsCSE names [(s, t) : rest]
    # hd = if s (renderWith names t) ("-" +++ renderWithPrec names precMul t)
    = hd +++ concatS (map rr rest)
where
    rr :: !(Bool, RadExpr Rational) -> {#Char}
    rr (True, e) = " + " +++ renderWith names e
    rr (False, e) = " - " +++ renderWithPrec names precMul e

renderWithPrec :: !(Map (RadExpr Rational) {#Char}) !Int !(RadExpr Rational) -> {#Char}
renderWithPrec names p e = case get e names of
    ?Just name -> parensIf (p > precAtom) name
    ?None      -> prettyPrec p e

renderFactorsCSE :: !(Map (RadExpr Rational) {#Char}) ![RadExpr Rational] -> {#Char}
renderFactorsCSE _ [] = "1"
renderFactorsCSE names [x] = renderWith names x
renderFactorsCSE names [Lit c : rest]
    | c == one = joinWith "\xC2\xB7" (map (renderWithPrec names precPow) rest)
    | c == ~ one = "-" +++ joinWith "\xC2\xB7" (map (renderWithPrec names precPow) rest)
    = prettyRat c +++ "\xC2\xB7" +++ joinWith "\xC2\xB7" (map (renderWithPrec names precPow) rest)
renderFactorsCSE names fs = joinWith "\xC2\xB7" (map (renderWithPrec names precPow) fs)
