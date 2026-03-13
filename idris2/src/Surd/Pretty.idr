module Surd.Pretty

import Surd.Rational
import Surd.Types
import Surd.Expr

import Data.SortedMap
import Data.List
import Data.Maybe
import Data.Nat

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

precAtom : Int
precAtom = 5

------------------------------------------------------------------------
-- Shared helpers
------------------------------------------------------------------------

parensIf : Bool -> String -> String
parensIf True  s = "(" ++ s ++ ")"
parensIf False s = s

prettyRat : Rational -> String
prettyRat r =
  let n = numer r
      d = denom r
  in if d == 1 then show n
     else if n < 0 then "(-" ++ show (abs n) ++ "/" ++ show d ++ ")"
     else "(" ++ show n ++ "/" ++ show d ++ ")"

joinWith : String -> List String -> String
joinWith _ [] = ""
joinWith _ [x] = x
joinWith sep (x :: xs) = x ++ concatMap (sep ++) xs

unlines : List String -> String
unlines [] = ""
unlines (x :: xs) = x ++ "\n" ++ Surd.Pretty.unlines xs

------------------------------------------------------------------------
-- Basic (non-CSE) pretty-printing
------------------------------------------------------------------------

flattenMulBasic : RadExpr Rational -> List (RadExpr Rational)
flattenMulBasic (Mul a b) = flattenMulBasic a ++ flattenMulBasic b
flattenMulBasic e = [e]

isSimpleBasic : RadExpr Rational -> Bool
isSimpleBasic (Lit _) = True
isSimpleBasic (Root _ _) = True
isSimpleBasic _ = False

mutual
  ||| Render a radical expression as a human-readable string.
  export
  pretty : RadExpr Rational -> String
  pretty = prettyPrec 0

  prettyRadicandBasic : RadExpr Rational -> String
  prettyRadicandBasic (Lit r) =
    if r >= Rational.zero && denom r == 1 then show (numer r)
    else "(" ++ pretty (Lit r) ++ ")"
  prettyRadicandBasic e =
    if isSimpleBasic e then prettyPrec precPow e
    else "(" ++ pretty e ++ ")"

  flattenAddBasic : RadExpr Rational -> List (Bool, RadExpr Rational)
  flattenAddBasic (Add a b) = flattenAddBasic a ++ flattenAddBasic b
  flattenAddBasic (Neg e) = map (\(s, t) => (not s, t)) (flattenAddBasic e)
  flattenAddBasic (Lit r) =
    if r < Rational.zero then [(False, Lit (negate r))]
    else [(True, Lit r)]
  flattenAddBasic (Mul (Neg a) b) = [(False, Mul a b)]
  flattenAddBasic e@(Mul _ _) = case flattenMulBasic e of
    (Lit r :: rest) =>
      if r < Rational.zero
        then [(False, rebuildMulB (Lit (negate r) :: rest))]
        else [(True, e)]
    _ => [(True, e)]
  flattenAddBasic e = [(True, e)]

  rebuildMulB : List (RadExpr Rational) -> RadExpr Rational
  rebuildMulB [] = Lit Rational.one
  rebuildMulB [x] = x
  rebuildMulB (x :: xs) = foldl Mul x xs

  renderTermsBasic : List (Bool, RadExpr Rational) -> String
  renderTermsBasic [] = "0"
  renderTermsBasic ((s, t) :: rest) =
    let hd = if s then prettyPrec precAdd t
             else "-" ++ prettyPrec precMul t
    in hd ++ concatMap rr rest
    where
      rr : (Bool, RadExpr Rational) -> String
      rr (True, e) = " + " ++ prettyPrec precAdd e
      rr (False, e) = " - " ++ prettyPrec precMul e

  renderFactorsBasic : List (RadExpr Rational) -> String
  renderFactorsBasic [] = "1"
  renderFactorsBasic [x] = prettyPrec precMul x
  renderFactorsBasic (Lit c :: rest) =
    if c == Rational.one then joinWith "\xB7" (map (prettyPrec precPow) rest)
    else if c == negate Rational.one then "-" ++ joinWith "\xB7" (map (prettyPrec precPow) rest)
    else prettyRat c ++ "\xB7" ++ joinWith "\xB7" (map (prettyPrec precPow) rest)
  renderFactorsBasic fs = joinWith "\xB7" (map (prettyPrec precPow) fs)

  ||| Render a radical expression with a given precedence context.
  export
  prettyPrec : Int -> RadExpr Rational -> String
  prettyPrec _ (Lit r) = prettyRat r
  prettyPrec p (Neg e) = case e of
    Add _ _ =>
      parensIf (p > precAdd) $ renderTermsBasic (map (\(s, t) => (not s, t)) (flattenAddBasic e))
    Mul (Lit c) rest =>
      prettyPrec p (Mul (Lit (negate c)) rest)
    Lit r =>
      prettyPrec p (Lit (negate r))
    _ =>
      parensIf (p > precNeg) $ "-" ++ prettyPrec precNeg e
  prettyPrec p e@(Add _ _) =
    parensIf (p > precAdd) $ renderTermsBasic (flattenAddBasic e)
  prettyPrec p (Mul a (Inv b)) =
    parensIf (p > precMul) $ prettyPrec precMul a ++ "/" ++ prettyPrec precPow b
  prettyPrec p e@(Mul _ _) =
    parensIf (p > precMul) $ renderFactorsBasic (flattenMulBasic e)
  prettyPrec p (Inv e) =
    parensIf (p > precMul) $ "1/" ++ prettyPrec precPow e
  prettyPrec _ (Root 2 (Lit r)) =
    if r == negate Rational.one then "i"
    else "\x221A" ++ prettyRadicandBasic (Lit r)
  prettyPrec _ (Root 2 e) = "\x221A" ++ prettyRadicandBasic e
  prettyPrec _ (Root 3 e) = "\x221B" ++ prettyRadicandBasic e
  prettyPrec _ (Root n e) = show n ++ "\x221A" ++ prettyRadicandBasic e
  prettyPrec _ (Pow _ 0) = "1"
  prettyPrec p (Pow e n) =
    if n < 0 then prettyPrec p (Inv (Pow e (negate n)))
    else if n == 1 then prettyPrec p e
    else parensIf (p > precPow) $ prettyPrec precPow e ++ "^" ++ show n

------------------------------------------------------------------------
-- CSE pretty-printing
------------------------------------------------------------------------

||| Generate variable names: a, b, ..., z, a1, b1, ...
charToStr : Char -> String
charToStr c = strCons c ""

varName : Nat -> String
varName i =
  let j : Integer = cast i
  in if j < 26
       then charToStr (chr (ord 'a' + cast j))
       else charToStr (chr (ord 'a' + cast (mod j 26))) ++ show (div j 26)

||| Count occurrences of every subexpression in the tree.
countSubs : Ord k => RadExpr k -> SortedMap (RadExpr k) Nat
countSubs = go empty
  where
    go : SortedMap (RadExpr k) Nat -> RadExpr k -> SortedMap (RadExpr k) Nat
    go m e =
      let old = fromMaybe 0 (lookup e m)
          m' = insert e (old + 1) m
      in case e of
           Lit _ => m'
           Neg a => go m' a
           Add a b => go (go m' a) b
           Mul a b => go (go m' a) b
           Inv a => go m' a
           Root _ a => go m' a
           Pow a _ => go m' a

||| Is a subexpression complex enough to be worth naming?
worthNaming : RadExpr k -> Bool
worthNaming (Lit _) = False
worthNaming (Neg (Lit _)) = False
worthNaming (Root _ (Lit _)) = False
worthNaming _ = True

||| Sort subexpressions by size (smaller first).
sortBySize : List (RadExpr Rational) -> List (RadExpr Rational)
sortBySize = map snd . sortBy (\a, b => compare (fst a) (fst b)) . map (\e => (exprSize e, e))

||| Pretty-print substituting names for shared subexpressions.
renderWith : SortedMap (RadExpr Rational) String -> RadExpr Rational -> String
renderWith names = go 0
  where
    mutual
      go : Int -> RadExpr Rational -> String
      go p e = case lookup e names of
        Just name => parensIf (p > precAtom) name
        Nothing => pp p e

      radicand : RadExpr Rational -> String
      radicand e@(Lit r) =
        if r >= Rational.zero && denom r == 1 then show (numer r)
        else "(" ++ go 0 e ++ ")"
      radicand e =
        if isSimple e then go precPow e
        else "(" ++ go 0 e ++ ")"

      isSimple : RadExpr Rational -> Bool
      isSimple (Lit _) = True
      isSimple (Root _ _) = True
      isSimple e = case lookup e names of
                     Just _ => True
                     Nothing => False

      flattenAddCSE : RadExpr Rational -> List (Bool, RadExpr Rational)
      flattenAddCSE (Add a b) = flattenAddCSE a ++ flattenAddCSE b
      flattenAddCSE (Neg e) = map (\(s, t) => (not s, t)) (flattenAddCSE e)
      flattenAddCSE (Lit r) =
        if r < Rational.zero then [(False, Lit (negate r))]
        else [(True, Lit r)]
      flattenAddCSE (Mul (Neg a) b) = [(False, Mul a b)]
      flattenAddCSE e@(Mul _ _) = case flattenMulCSE e of
        (Lit r :: rest) =>
          if r < Rational.zero
            then [(False, rebuildMulCSE (Lit (negate r) :: rest))]
            else [(True, e)]
        _ => [(True, e)]
      flattenAddCSE e = [(True, e)]

      flattenMulCSE : RadExpr Rational -> List (RadExpr Rational)
      flattenMulCSE (Mul a b) = flattenMulCSE a ++ flattenMulCSE b
      flattenMulCSE e = [e]

      rebuildMulCSE : List (RadExpr Rational) -> RadExpr Rational
      rebuildMulCSE [] = Lit Rational.one
      rebuildMulCSE [x] = x
      rebuildMulCSE (x :: xs) = foldl Mul x xs

      renderTermsCSE : List (Bool, RadExpr Rational) -> String
      renderTermsCSE [] = "0"
      renderTermsCSE ((s, t) :: rest) =
        let hd = if s then go precAdd t else "-" ++ go precMul t
        in hd ++ concatMap rr rest
        where
          rr : (Bool, RadExpr Rational) -> String
          rr (True, e) = " + " ++ go precAdd e
          rr (False, e) = " - " ++ go precMul e

      renderFactorsCSE : List (RadExpr Rational) -> String
      renderFactorsCSE [] = "1"
      renderFactorsCSE [x] = go precMul x
      renderFactorsCSE (Lit c :: rest) =
        if c == Rational.one then joinWith "\xB7" (map (go precPow) rest)
        else if c == negate Rational.one then "-" ++ joinWith "\xB7" (map (go precPow) rest)
        else prettyRat c ++ "\xB7" ++ joinWith "\xB7" (map (go precPow) rest)
      renderFactorsCSE fs = joinWith "\xB7" (map (go precPow) fs)

      pp : Int -> RadExpr Rational -> String
      pp _ (Lit r) = prettyRat r
      pp p (Neg e) =
        parensIf (p > precNeg) $ "-" ++ go precNeg e
      pp p e@(Add _ _) =
        parensIf (p > precAdd) $ renderTermsCSE (flattenAddCSE e)
      pp p (Mul a (Inv b)) =
        parensIf (p > precMul) $ go precMul a ++ "/" ++ go precPow b
      pp p e@(Mul _ _) =
        parensIf (p > precMul) $ renderFactorsCSE (flattenMulCSE e)
      pp p (Inv e) =
        parensIf (p > precMul) $ "1/" ++ go precPow e
      pp _ (Root 2 (Lit r)) =
        if r == negate Rational.one then "i"
        else "\x221A" ++ radicand (Lit r)
      pp _ (Root 2 e) = "\x221A" ++ radicand e
      pp _ (Root 3 e) = "\x221B" ++ radicand e
      pp _ (Root n e) = show n ++ "\x221A" ++ radicand e
      pp _ (Pow _ 0) = "1"
      pp p (Pow e n) =
        if n < 0 then pp p (Inv (Pow e (negate n)))
        else if n == 1 then go p e
        else parensIf (p > precPow) $ go precPow e ++ "^" ++ show n

||| Render with CSE: repeated subexpressions are shown as named intermediates.
export
prettyCSE : RadExpr Rational -> String
prettyCSE expr =
  let counts = countSubs expr
      allPairs : List (RadExpr Rational, Nat)
      allPairs = Data.SortedMap.toList counts
      shared = map (\p => fst p) $ filter (\p => snd p >= 2 && worthNaming (fst p)) allPairs
      sorted = sortBySize shared
      nameList = zipWith (\i, _ => varName i) [0 .. length sorted] sorted
      nameMap = fromList (zip sorted nameList)
      bindings = zipWith
        (\sub, name =>
          let availNames = delete sub nameMap
          in "  " ++ name ++ " = " ++ renderWith availNames sub)
        sorted nameList
      body = renderWith nameMap expr
  in case bindings of
       [] => body
       _ => "let\n" ++ Surd.Pretty.unlines bindings ++ "in " ++ body
