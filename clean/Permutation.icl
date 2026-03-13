implementation module Permutation

import StdEnv
from Data.Map import :: Map(..), newMap, get, put, toAscList, toList, fromList, mapSize, foldrWithKey

:: Perm = { pN :: !Int, pImg :: ![Int] }

permN :: !Perm -> Int
permN p = p.pN

permId :: !Int -> Perm
permId n = { pN = n, pImg = [0 .. n - 1] }

fromCycles :: !Int ![[Int]] -> Perm
fromCycles n cycles
    # base = fromList [(i, i) \\ i <- [0 .. n - 1]]
    # final = foldl applyCyc base cycles
    = { pN = n, pImg = map snd (toAscList final) }
where
    applyCyc :: !(Map Int Int) ![Int] -> Map Int Int
    applyCyc m [] = m
    applyCyc m cyc=:[c0:_]
        # pairs = zip2 cyc (tl cyc ++ [c0])
        = foldl (\m` (a, b) -> put a b m`) m pairs

fromMapping :: ![Int] -> Perm
fromMapping img = { pN = length img, pImg = img }

permApply :: !Perm !Int -> Int
permApply p i = p.pImg !! i

permCompose :: !Perm !Perm -> Perm
permCompose s t = { pN = s.pN, pImg = [s.pImg !! (t.pImg !! i) \\ i <- [0 .. s.pN - 1]] }

permInverse :: !Perm -> Perm
permInverse p
    # m = fromList [(p.pImg !! i, i) \\ i <- [0 .. p.pN - 1]]
    = { pN = p.pN, pImg = map snd (toAscList m) }

permIsId :: !Perm -> Bool
permIsId p = p.pImg == [0 .. p.pN - 1]

permOrder :: !Perm -> Int
permOrder p
    | permIsId p = 1
    = foldl lcmI 1 [length c \\ c <- permCycles p]

lcmI :: !Int !Int -> Int
lcmI a b = a * b / gcdI a b

gcdI :: !Int !Int -> Int
gcdI a 0 = abs a
gcdI a b = gcdI b (a rem b)

permCycles :: !Perm -> [[Int]]
permCycles p = go (createArray p.pN False) 0
where
    go :: *{#Bool} !Int -> [[Int]]
    go vis i
        | i >= p.pN = []
        | vis.[i] = go vis (i + 1)
        | p.pImg !! i == i
            # vis = {vis & [i] = True}
            = go vis (i + 1)
        # (cyc, vis`) = traceCycle vis i
        = [cyc : go vis` (i + 1)]

    traceCycle :: *{#Bool} !Int -> ([Int], *{#Bool})
    traceCycle vis start
        # vis = {vis & [start] = True}
        = traceLoop vis start (p.pImg !! start) [start]

    traceLoop :: *{#Bool} !Int !Int ![Int] -> ([Int], *{#Bool})
    traceLoop vis start current acc
        | current == start = (reverse acc, vis)
        # vis = {vis & [current] = True}
        = traceLoop vis start (p.pImg !! current) [current:acc]

permSign :: !Perm -> Int
permSign p
    # k = sum [length c - 1 \\ c <- permCycles p]
    = if (isEven k) 1 (~1)

instance == Perm where
    (==) a b = a.pN == b.pN && a.pImg == b.pImg

instance < Perm where
    (<) a b
        | a.pN <> b.pN = a.pN < b.pN
        = a.pImg < b.pImg

instance toString Perm where
    toString p
        | permIsId p = "id"
        = concatStrs [showCyc c \\ c <- permCycles p]
    where
        showCyc cs = "(" +++ concatStrs (intersperse " " (map toString cs)) +++ ")"
        intersperse :: a [a] -> [a]
        intersperse _ [] = []
        intersperse _ [x] = [x]
        intersperse sep [x:xs] = [x, sep : intersperse sep xs]
        concatStrs :: [{#Char}] -> {#Char}
        concatStrs [] = ""
        concatStrs [s:ss] = s +++ concatStrs ss

// ─── Schreier-Sims ───
:: BSGS = { bBase :: ![Int], bTrans :: ![Map Int Perm], bN :: !Int }

schreierSims :: !Int ![Perm] -> BSGS
schreierSims n gens
    | isEmpty gens || all permIsId gens = { bBase = [], bTrans = [], bN = n }
    = saturate n gens (foldl (siftInsert n gens) { bBase = [], bTrans = [], bN = n } gens)

siftInsert :: !Int ![Perm] !BSGS !Perm -> BSGS
siftInsert n _ bsgs g
    # r = sift bsgs g
    | permIsId r = bsgs
    = extendBSGS n bsgs r

sift :: !BSGS !Perm -> Perm
sift bsgs h = go 0 h
where
    go i h`
        | i >= length bsgs.bBase = h`
        # img = permApply h` (bsgs.bBase !! i)
        = case get img (bsgs.bTrans !! i) of
            ?None -> h`
            ?Just u -> go (i + 1) (permCompose (permInverse u) h`)

extendBSGS :: !Int !BSGS !Perm -> BSGS
extendBSGS n bsgs g
    # fixedLevels = length (takeWhile (\b -> permApply g b == b) bsgs.bBase)
    | fixedLevels >= length bsgs.bBase
        # pt = hd [i \\ i <- [0 .. n - 1] | permApply g i <> i]
        = { bBase = bsgs.bBase ++ [pt]
          , bTrans = bsgs.bTrans ++ [buildTransversal n pt [g]]
          , bN = n }
    # existingReps = [snd kv \\ kv <- toList (bsgs.bTrans !! fixedLevels)]
    # newTrans = buildTransversal n (bsgs.bBase !! fixedLevels) [g : existingReps]
    = { bBase = bsgs.bBase
      , bTrans = take fixedLevels bsgs.bTrans ++ [newTrans] ++ drop (fixedLevels + 1) bsgs.bTrans
      , bN = n }

buildTransversal :: !Int !Int ![Perm] -> Map Int Perm
buildTransversal n pt gs = bfs (put pt (permId n) newMap) [pt]
where
    bfs visited [] = visited
    bfs visited [x:queue]
        # ux = case get x visited of
            ?Just u -> u
            ?None -> permId n
        # candidates = [(permApply g x, permCompose g ux) \\ g <- gs]
        # new = [(y, u) \\ (y, u) <- candidates | isNothing (get y visited)]
        # visited` = foldl (\m (k, v) -> put k v m) visited new
        = bfs visited` (queue ++ [fst p \\ p <- new])

    isNothing :: (?a) -> Bool
    isNothing ?None = True
    isNothing _ = False

saturate :: !Int ![Perm] !BSGS -> BSGS
saturate n gens bsgs
    # sGens = schreierGens n gens bsgs
    # bsgs` = foldl (siftInsert n gens) bsgs sGens
    | groupOrder bsgs` == groupOrder bsgs = bsgs`
    = saturate n gens bsgs`

schreierGens :: !Int ![Perm] !BSGS -> [Perm]
schreierGens n gens bsgs
    = flatten [schreierGensAt n gens bsgs i transversal \\ (i, transversal) <- zip2 [0..] bsgs.bTrans]

schreierGensAt :: !Int ![Perm] !BSGS !Int !(Map Int Perm) -> [Perm]
schreierGensAt n gens bsgs i transversal
    # b = bsgs.bBase !! i
    # allSGs = [makeSchreierGen n b transversal u s \\ (_, u) <- toList transversal, s <- gens]
    = [sg \\ sg <- allSGs | not (permIsId sg)]

makeSchreierGen :: !Int !Int !(Map Int Perm) !Perm !Perm -> Perm
makeSchreierGen n b transversal u s
    # beta = permApply u b
    # s_beta = permApply s beta
    # u_s_beta = lookupOrId n s_beta transversal
    = permCompose (permInverse u_s_beta) (permCompose s u)

lookupOrId :: !Int !Int !(Map Int Perm) -> Perm
lookupOrId n key m
    = case get key m of
        ?Just u -> u
        ?None -> permId n

groupOrder :: !BSGS -> Int
groupOrder bsgs = foldl (\acc t -> acc * mapSize t) 1 bsgs.bTrans

groupContains :: !BSGS !Perm -> Bool
groupContains bsgs p = permIsId (sift bsgs p)
