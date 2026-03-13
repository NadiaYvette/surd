implementation module Expr

import StdEnv
import RadExpr
import Rational

freeOf :: (k -> Bool) !(RadExpr k) -> Bool
freeOf p (Lit k) = p k
freeOf p (Neg a) = freeOf p a
freeOf p (Add a b) = freeOf p a && freeOf p b
freeOf p (Mul a b) = freeOf p a && freeOf p b
freeOf p (Inv a) = freeOf p a
freeOf p (Root _ a) = freeOf p a
freeOf p (Pow a _) = freeOf p a

collectRadicals :: !(RadExpr k) -> [(Int, RadExpr k)] | == k
collectRadicals expr = nub (go expr)
where
    go :: !(RadExpr k) -> [(Int, RadExpr k)] | == k
    go (Lit _) = []
    go (Neg a) = go a
    go (Add a b) = go a ++ go b
    go (Mul a b) = go a ++ go b
    go (Inv a) = go a
    go (Pow a _) = go a
    go (Root n a) = go a ++ [(n, a)]

    nub :: [(Int, RadExpr k)] -> [(Int, RadExpr k)] | == k
    nub [] = []
    nub [x:xs] = [x : nub (filter (\y -> not (pairEq x y)) xs)]

    pairEq :: (Int, RadExpr k) (Int, RadExpr k) -> Bool | == k
    pairEq (n1, e1) (n2, e2) = n1 == n2 && e1 == e2

topoSortRadicals :: [(Int, RadExpr k)] -> [(Int, RadExpr k)] | == k
topoSortRadicals rads = go [] rads
where
    go :: [(Int, RadExpr k)] [(Int, RadExpr k)] -> [(Int, RadExpr k)] | == k
    go sorted [] = sorted
    go sorted remaining
        # ready = [r \\ r <- remaining | allRootsResolved sorted (snd r)]
        # remaining` = [r \\ r <- remaining | not (elemPair r ready)]
        | isEmpty ready = sorted ++ remaining
        = go (sorted ++ ready) remaining`

    elemPair :: (Int, RadExpr k) [(Int, RadExpr k)] -> Bool | == k
    elemPair _ [] = False
    elemPair (n1,e1) [(n2,e2):rest]
        | n1 == n2 && e1 == e2 = True
        = elemPair (n1,e1) rest

allRootsResolved :: [(Int, RadExpr k)] !(RadExpr k) -> Bool | == k
allRootsResolved _ (Lit _) = True
allRootsResolved resolved (Neg a) = allRootsResolved resolved a
allRootsResolved resolved (Add a b) = allRootsResolved resolved a && allRootsResolved resolved b
allRootsResolved resolved (Mul a b) = allRootsResolved resolved a && allRootsResolved resolved b
allRootsResolved resolved (Inv a) = allRootsResolved resolved a
allRootsResolved resolved (Pow a _) = allRootsResolved resolved a
allRootsResolved resolved (Root n a) = elemPair (n, a) resolved
where
    elemPair :: (Int, RadExpr k) [(Int, RadExpr k)] -> Bool | == k
    elemPair _ [] = False
    elemPair (n1,e1) [(n2,e2):rest]
        | n1 == n2 && e1 == e2 = True
        = elemPair (n1,e1) rest
