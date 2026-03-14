implementation module RadicalTower

import StdEnv
import RadExpr
import Rational
import Poly
import Identify
import TransitiveGroup
import Resolvent
import Eval
import DAG
import RootOfUnity
import Data.Integer

// ─── Entry point ───

solveViaTowerN :: !GaloisResult !(Poly Rational) -> ?([RadExpr Rational])
solveViaTowerN gr f
    | not gr.grGroup.tgSolvable = ?None
    | degree f == 5 = solveViaTower gr f
    = solveSolvablePrime gr.grGroup f gr.grRoots

// ─── Degree-5 fast path ───

solveViaTower :: !GaloisResult !(Poly Rational) -> ?([RadExpr Rational])
solveViaTower gr f
    | not gr.grGroup.tgSolvable = ?None
    | length gr.grRoots <> 5 = ?None
    # cs = getCoeffsRT f
    # lc = last cs
    # monicCs = [c / lc \\ c <- cs]
    # a4 = monicCs !! 4
    # shiftVal = a4 / ratFromInt (~5)
    # shift = Lit shiftVal
    # depRoots = [(re - ratToReal shiftVal, im) \\ (re, im) <- gr.grRoots]
    # n = 5
    # tau = 2.0 * pi / toReal n
    // Find cyclic ordering on depressed roots
    = case findCyclicOrdering5 depRoots gr.grGroup.tgName of
        ?None -> ?None
        ?Just ordering ->
            solveWithOrdering5 depRoots ordering shift gr.grGroup.tgName n tau gr.grRoots

solveWithOrdering5 :: ![(Real, Real)] ![Int] !(RadExpr Rational) !{#Char} !Int !Real ![(Real, Real)] -> ?([RadExpr Rational])
solveWithOrdering5 depRoots ordering shift groupName n tau origRoots
    # orderedRoots = [depRoots !! i \\ i <- ordering]
    // Compute R_j and R_j^5
    # resolvents = [computeResolvent orderedRoots tau j \\ j <- [0 .. n - 1]]
    # resolventPowers = [cpowR (resolvents !! j) 5 \\ j <- [0 .. n - 1]]
    // DFT: d_s = (1/5) Σ_j ω₅^{-js} R_j^5
    # dVals = [computeDS orderedRoots resolvents n s \\ s <- [0 .. n - 1]]
    // Match d_s
    = case matchDs5 groupName dVals of
        ?None -> ?None
        ?Just dExprs ->
            buildRoots5 dExprs resolvents resolventPowers shift n tau origRoots

buildRoots5 :: ![RadExpr Rational] ![(Real, Real)] ![(Real, Real)] !(RadExpr Rational) !Int !Real ![(Real, Real)] -> ?([RadExpr Rational])
buildRoots5 dExprs resolvents resolventPowers shift n tau roots
    // R_j^5 = Σ_s d_s · ω₅^{js}
    # rjPowExprs = [foldl1Expr Add [Mul (dExprs !! s) (omegaPow5 ((j * s) rem 5)) \\ s <- [0..4]] \\ j <- [1..4]]
    // Branch selection
    # rjExprs = [selectBranch5 (rjPowExprs !! (j - 1)) (resolvents !! j) \\ j <- [1..4]]
    # allR = [Lit (ratFromInt 0) : rjExprs]
    // Inverse DFT: α_k = (1/5) Σ_j ω₅^{-jk} R_j
    # rootExprs = [dagFoldE (Mul (Inv (Lit (ratFromInt 5)))
                    (foldl1Expr Add [Mul (omegaPow5 ((5 - (j * k) rem 5) rem 5)) (allR !! j)
                                    \\ j <- [0..4]]))
                  \\ k <- [0..4]]
    // Un-depress
    # finalExprs = [dagFoldE (Add e shift) \\ e <- rootExprs]
    // Match to original root ordering
    = ?Just (matchToOriginal finalExprs roots)

// ─── Generalised prime-degree solver ───

solveSolvablePrime :: !TransitiveGroup !(Poly Rational) ![(Real, Real)] -> ?([RadExpr Rational])
solveSolvablePrime tg f numRoots
    # n = degree f
    // Step 1: Depress
    # cs = getCoeffsRT f
    # lc = last cs
    # monicCs = [c / lc \\ c <- cs]
    # an1 = monicCs !! (n - 1)
    # shiftVal = Neg (Mul (Inv (Lit (ratFromInt n))) (Lit an1))
    # shiftR = an1 / ratFromInt (~n)
    # depRoots = [(re - ratToReal shiftR, im) \\ (re, im) <- numRoots]
    // Step 2: Find cyclic ordering
    = case findCyclicOrderingN depRoots n tg of
        ?None -> ?None
        ?Just ordering ->
            solvePrimeWithOrdering tg f numRoots depRoots ordering n shiftVal

solvePrimeWithOrdering :: !TransitiveGroup !(Poly Rational) ![(Real, Real)] ![(Real, Real)] ![Int] !Int !(RadExpr Rational) -> ?([RadExpr Rational])
solvePrimeWithOrdering tg f numRoots depRoots ordering n shiftVal
    # orderedRoots = [depRoots !! i \\ i <- ordering]
    // Step 3: Lagrange resolvents R_j = Σ_k ω_n^{jk} α_k
    # rjVals = [computeResolventN orderedRoots n j \\ j <- [0 .. n - 1]]
    # rjPows = [cpowR (rjVals !! j) n \\ j <- [0 .. n - 1]]
    // Step 4: DFT: d_s = (1/n) Σ_j ω_n^{-js} R_j^n
    # dVals = [computeDSN rjPows n s \\ s <- [0 .. n - 1]]
    // Step 5: Match d_s to radical expressions
    = case matchDsGeneral tg dVals n of
        ?None -> ?None
        ?Just dExprs ->
            finishPrimeSolve tg f numRoots dExprs rjVals n shiftVal

finishPrimeSolve :: !TransitiveGroup !(Poly Rational) ![(Real, Real)] ![RadExpr Rational] ![(Real, Real)] !Int !(RadExpr Rational) -> ?([RadExpr Rational])
finishPrimeSolve tg f numRoots dExprs rjVals n shiftVal
    # omExpr = omegaNExpr n
    // Step 6: R_j^n = Σ_s d_s · ω_n^{js}
    # rjPowExprs = [foldl1Expr Add [Mul (dExprs !! s) (omegaPowN n omExpr ((j * s) rem n)) \\ s <- [0 .. n - 1]]
                   \\ j <- [1 .. n - 1]]
    // Step 7: Branch selection
    # rjExprs = [selectBranchN n omExpr (rjPowExprs !! (j - 1)) (rjVals !! j) \\ j <- [1 .. n - 1]]
    # allR = [Lit (ratFromInt 0) : rjExprs]
    // Step 8: Inverse DFT: α_k = (1/n) Σ_j ω_n^{-jk} R_j
    # rootExprs = [dagFoldE (Mul (Inv (Lit (ratFromInt n)))
                    (foldl1Expr Add [Mul (omegaPowN n omExpr ((n - (j * k) rem n) rem n)) (allR !! j)
                                    \\ j <- [0 .. n - 1]]))
                  \\ k <- [0 .. n - 1]]
    // Step 9: Un-depress
    # finalExprs = [dagFoldE (Add e shiftVal) \\ e <- rootExprs]
    = ?Just (matchToOriginal finalExprs numRoots)

// ─── Cyclic ordering (degree 5 fast path) ───

findCyclicOrdering5 :: ![(Real, Real)] !{#Char} -> ?([Int])
findCyclicOrdering5 roots groupName
    # rest = [1, 2, 3, 4]
    # orderings = [0 : p \\ p <- permsI rest]
    # scored = [(o, scoreOrdering5 roots o groupName) \\ o <- orderings]
    # sorted = sortBy (\(_, a) (_, b) -> a < b) scored
    = case sorted of
        [(bestO, bestScore), (_, secondScore) : _]
            | bestScore < 10.0 && bestScore < 0.5 * secondScore -> ?Just bestO
            | bestScore < 5.0 -> ?Just bestO
        [(bestO, bestScore) : _]
            | bestScore < 5.0 -> ?Just bestO
        _ -> ?None

scoreOrdering5 :: ![(Real, Real)] ![Int] !{#Char} -> Real
scoreOrdering5 roots ordering groupName
    # ordered = [roots !! i \\ i <- ordering]
    # rjPows = [cpowR (computeResolvent ordered (2.0 * pi / 5.0) j) 5 \\ j <- [0..4]]
    # dVals = [computeDS ordered
                [computeResolvent ordered (2.0 * pi / 5.0) j \\ j <- [0..4]]
                5 s
              \\ s <- [0..4]]
    = case groupName of
        "C5"  -> sumR [scoreRational d \\ d <- dVals]
        "D5"  -> scoreRational (hd dVals)
                 + scoreRational (cadd (dVals !! 1) (dVals !! 4))
                 + scoreRational (cmulR (dVals !! 1) (dVals !! 4))
                 + scoreRational (cadd (dVals !! 2) (dVals !! 3))
                 + scoreRational (cmulR (dVals !! 2) (dVals !! 3))
        "F20" -> scoreRational (hd dVals)
                 + scoreRational (foldl cadd (0.0, 0.0) [dVals !! s \\ s <- [1..4]])
                 + scoreRational (sumC [cmulR (dVals !! i) (dVals !! j)
                                       \\ i <- [1..4], j <- [i+1..4]])
                 + scoreRational (sumC [cmulR (cmulR (dVals !! i) (dVals !! j)) (dVals !! k)
                                       \\ i <- [1..4], j <- [i+1..4], k <- [j+1..4]])
                 + scoreRational (cmulR (cmulR (dVals !! 1) (dVals !! 2))
                                        (cmulR (dVals !! 3) (dVals !! 4)))
        _     -> 1.0E10

sumR :: ![Real] -> Real
sumR [] = 0.0
sumR [x:xs] = x + sumR xs

// ─── Cyclic ordering (general prime degree) ───

findCyclicOrderingN :: ![(Real, Real)] !Int !TransitiveGroup -> ?([Int])
findCyclicOrderingN roots n tg
    | n == 5 = findCyclicOrdering5 roots tg.tgName
    | n <= 8
        // For small n, brute-force all (n-1)! orderings
        # rest = [1 .. n - 1]
        # orderings = [0 : p \\ p <- permsI rest]
        # scored = [(o, scoreOrderingN roots o n tg) \\ o <- orderings]
        # sorted = sortBy (\(_, a) (_, b) -> a < b) scored
        = case sorted of
            [(bestO, bestScore), (_, secondScore) : _]
                | bestScore < 5.0 * toReal n && bestScore < 0.5 * secondScore -> ?Just bestO
                | bestScore < toReal n -> ?Just bestO
            [(bestO, bestScore) : _]
                | bestScore < toReal n -> ?Just bestO
            _ -> ?None
    // For large n, try (n-1) rotations
    = findCyclicOrderingByRotation roots n tg

findCyclicOrderingByRotation :: ![(Real, Real)] !Int !TransitiveGroup -> ?([Int])
findCyclicOrderingByRotation roots n tg
    # candidates = [(ordering, scoreOrderingN roots ordering n tg)
                   \\ next <- [1 .. n - 1]
                   , let ordering = buildOrdering roots n 0 next
                   | length ordering == n]
    # sorted = sortBy (\(_, a) (_, b) -> a < b) candidates
    = case sorted of
        [(bestO, bestScore) : _]
            | bestScore < 5.0 * toReal n -> ?Just bestO
        _ -> ?None

buildOrdering :: ![(Real, Real)] !Int !Int !Int -> [Int]
buildOrdering roots n start next
    # step = csub (roots !! next) (roots !! start)
    = go [start, next] next [next, start] step
where
    go :: ![Int] !Int ![Int] !(Real, Real) -> [Int]
    go used pos acc step
        | length acc >= n = reverse acc
        # target = cadd (roots !! pos) step
        # unused = [i \\ i <- [0 .. n - 1] | not (isMember i used)]
        = case unused of
            [] -> reverse acc
            _  ->
                # closest = hd (sortBy (\i j -> cmagR (csub (roots !! i) target)
                                              < cmagR (csub (roots !! j) target)) unused)
                -> go [closest : used] closest [closest : acc] step

scoreOrderingN :: ![(Real, Real)] ![Int] !Int !TransitiveGroup -> Real
scoreOrderingN roots ordering n tg
    # ordered = [roots !! i \\ i <- ordering]
    # rjVals = [computeResolventN ordered n j \\ j <- [0 .. n - 1]]
    # rjPows = [cpowR (rjVals !! j) n \\ j <- [0 .. n - 1]]
    # dVals = [computeDSN rjPows n s \\ s <- [0 .. n - 1]]
    # d = tg.tgOrder / n
    | d == 1
        // Cyclic: all d_s should be rational
        = sumR [scoreRational dv \\ dv <- dVals]
    | d == 2
        // Dihedral: d_0 rational, conjugate pairs
        = scoreRational (hd dVals)
          + sumR [scoreRational (cadd (dVals !! s) (dVals !! (n - s)))
                  + scoreRational (cmulR (dVals !! s) (dVals !! (n - s)))
                 \\ s <- [1 .. n / 2]]
    // General: simplified check - all d_s rationality
    = sumR [scoreRational dv \\ dv <- dVals]

// ─── Resolvent computation ───

computeResolvent :: ![(Real, Real)] !Real !Int -> (Real, Real)
computeResolvent roots tau j
    = foldl (\(re, im) k ->
        let omk = (cos (tau * toReal (j * k)), sin (tau * toReal (j * k)))
        in caddRT (re, im) (cmulRT omk (roots !! k)))
        (0.0, 0.0) [0 .. length roots - 1]

computeResolventN :: ![(Real, Real)] !Int !Int -> (Real, Real)
computeResolventN roots n j
    # tau = 2.0 * pi / toReal n
    = foldl (\acc k ->
        let omk = (cos (tau * toReal (j * k)), sin (tau * toReal (j * k)))
        in caddRT acc (cmulRT omk (roots !! k)))
        (0.0, 0.0) [0 .. n - 1]

computeDS :: ![(Real, Real)] ![(Real, Real)] !Int !Int -> (Real, Real)
computeDS _ resolvents n s
    # tau = 2.0 * pi / toReal n
    # rjPows = [cpowR (resolvents !! j) n \\ j <- [0 .. n - 1]]
    # invN = 1.0 / toReal n
    = cmulS invN (foldl (\acc j ->
        let angle = tau * toReal ((n - (j * s) rem n) rem n)
            omk = (cos angle, sin angle)
        in caddRT acc (cmulRT omk (rjPows !! j)))
        (0.0, 0.0) [0 .. n - 1])

computeDSN :: ![(Real, Real)] !Int !Int -> (Real, Real)
computeDSN rjPows n s
    # tau = 2.0 * pi / toReal n
    # invN = 1.0 / toReal n
    = cmulS invN (foldl (\acc j ->
        let angle = tau * toReal ((n - (j * s) rem n) rem n)
            omk = (cos angle, sin angle)
        in caddRT acc (cmulRT omk (rjPows !! j)))
        (0.0, 0.0) [0 .. n - 1])

cmulS :: !Real !(Real, Real) -> (Real, Real)
cmulS s (re, im) = (s * re, s * im)

// ─── DFT coefficient matching ───

matchDs5 :: !{#Char} ![(Real, Real)] -> ?([RadExpr Rational])
matchDs5 "C5" dVals = matchDsToQ dVals
matchDs5 "D5" dVals = matchDsD5 dVals
matchDs5 "F20" dVals = matchDsF20 dVals
matchDs5 _ _ = ?None

matchDsGeneral :: !TransitiveGroup ![(Real, Real)] !Int -> ?([RadExpr Rational])
matchDsGeneral tg dVals n
    # d = tg.tgOrder / n
    | d == 1 = matchDsAllRational dVals
    | d == 2 = matchDsDihedralN dVals n
    = matchDsViaOrbits tg dVals n

matchDsToQ :: ![(Real, Real)] -> ?([RadExpr Rational])
matchDsToQ dVals = matchAllRat dVals

matchDsAllRational :: ![(Real, Real)] -> ?([RadExpr Rational])
matchDsAllRational dVals = matchAllRat dVals

matchAllRat :: ![(Real, Real)] -> ?([RadExpr Rational])
matchAllRat [] = ?Just []
matchAllRat [(re, im) : rest]
    | abs im > 0.05 = ?None
    = case matchAllRat rest of
        ?None -> ?None
        ?Just restExprs -> ?Just [Lit (approxRatRT re) : restExprs]

matchDsD5 :: ![(Real, Real)] -> ?([RadExpr Rational])
matchDsD5 dVals
    | length dVals < 5 = ?None
    # d0 = dVals !! 0
    # d1 = dVals !! 1
    # d2 = dVals !! 2
    # d3 = dVals !! 3
    # d4 = dVals !! 4
    | abs (snd d0) > 0.1 = ?None
    # d0Expr = Lit (approxRatRT (fst d0))
    // {d_1, d_4}: t² - s₁t + p₁ = 0
    # s1C = cadd d1 d4
    # p1C = cmulRT d1 d4
    | abs (snd s1C) > 0.1 = ?None
    | abs (snd p1C) > 0.1 = ?None
    # s1 = Lit (approxRatRT (fst s1C))
    # p1 = Lit (approxRatRT (fst p1C))
    # s1R = approxRatRT (fst s1C)
    # p1R = approxRatRT (fst p1C)
    # disc1R = s1R * s1R - ratFromInt 4 * p1R
    # disc1Expr = rSub (Mul s1 s1) (Mul (Lit (ratFromInt 4)) p1)
    # sqrtDisc1 = Root 2 disc1Expr
    # d1Plus = Mul (Inv (Lit (ratFromInt 2))) (Add s1 sqrtDisc1)
    # d1Minus = Mul (Inv (Lit (ratFromInt 2))) (rSub s1 sqrtDisc1)
    # sqrtDisc1Val = if (ratToReal disc1R >= 0.0) (sqrt (ratToReal disc1R), 0.0)
                     (0.0, sqrt (abs (ratToReal disc1R)))
    # d1PlusVal = cmulS 0.5 (cadd s1C sqrtDisc1Val)
    # (d1Expr, d4Expr) = if (cmagR (csub d1PlusVal d1) < cmagR (csub d1PlusVal d4))
                          (d1Plus, d1Minus) (d1Minus, d1Plus)
    // {d_2, d_3}: t² - s₂t + p₂ = 0
    # s2C = cadd d2 d3
    # p2C = cmulRT d2 d3
    | abs (snd s2C) > 0.1 = ?None
    | abs (snd p2C) > 0.1 = ?None
    # s2 = Lit (approxRatRT (fst s2C))
    # p2 = Lit (approxRatRT (fst p2C))
    # s2R = approxRatRT (fst s2C)
    # p2R = approxRatRT (fst p2C)
    # disc2R = s2R * s2R - ratFromInt 4 * p2R
    # disc2Expr = rSub (Mul s2 s2) (Mul (Lit (ratFromInt 4)) p2)
    # sqrtDisc2 = Root 2 disc2Expr
    # d2Plus = Mul (Inv (Lit (ratFromInt 2))) (Add s2 sqrtDisc2)
    # d2Minus = Mul (Inv (Lit (ratFromInt 2))) (rSub s2 sqrtDisc2)
    # sqrtDisc2Val = if (ratToReal disc2R >= 0.0) (sqrt (ratToReal disc2R), 0.0)
                     (0.0, sqrt (abs (ratToReal disc2R)))
    # d2PlusVal = cmulS 0.5 (cadd s2C sqrtDisc2Val)
    # (d2Expr, d3Expr) = if (cmagR (csub d2PlusVal d2) < cmagR (csub d2PlusVal d3))
                          (d2Plus, d2Minus) (d2Minus, d2Plus)
    = ?Just [d0Expr, d1Expr, d2Expr, d3Expr, d4Expr]

matchDsF20 :: ![(Real, Real)] -> ?([RadExpr Rational])
matchDsF20 dVals
    | length dVals < 5 = ?None
    # d0 = dVals !! 0
    | abs (snd d0) > 0.1 = ?None
    # d0Expr = Lit (approxRatRT (fst d0))
    = case mapM_matchQOmega5 [dVals !! i \\ i <- [1..4]] of
        ?None -> ?None
        ?Just restExprs -> ?Just [d0Expr : restExprs]

mapM_matchQOmega5 :: ![(Real, Real)] -> ?([RadExpr Rational])
mapM_matchQOmega5 [] = ?Just []
mapM_matchQOmega5 [d:ds]
    = case matchQOmega5 d of
        ?None -> ?None
        ?Just e -> case mapM_matchQOmega5 ds of
            ?None -> ?None
            ?Just rest -> ?Just [e : rest]

// ─── Generalised dihedral matching (degree n) ───

matchDsDihedralN :: ![(Real, Real)] !Int -> ?([RadExpr Rational])
matchDsDihedralN dVals n
    # d0 = hd dVals
    | abs (snd d0) > 0.05 = ?None
    # d0Expr = Lit (approxRatRT (fst d0))
    # halfN = (n - 1) / 2
    = case matchConjPairs dVals n 1 halfN of
        ?None -> ?None
        ?Just pairExprs ->
            # result = fillInPairs n d0Expr pairExprs
            -> ?Just result

matchConjPairs :: ![(Real, Real)] !Int !Int !Int -> ?([(RadExpr Rational, RadExpr Rational)])
matchConjPairs _ _ s halfN | s > halfN = ?Just []
matchConjPairs dVals n s halfN
    = case matchConjPair (dVals !! s) (dVals !! (n - s)) of
        ?None -> ?None
        ?Just pair -> case matchConjPairs dVals n (s + 1) halfN of
            ?None -> ?None
            ?Just rest -> ?Just [pair : rest]

matchConjPair :: !(Real, Real) !(Real, Real) -> ?((RadExpr Rational, RadExpr Rational))
matchConjPair d1 d2
    # s = cadd d1 d2
    # p = cmulRT d1 d2
    | abs (snd s) > 0.05 = ?None
    | abs (snd p) > 0.05 = ?None
    # sR = approxRatRT (fst s)
    # pR = approxRatRT (fst p)
    # sExpr = Lit sR
    # pExpr = Lit pR
    # discR = sR * sR - ratFromInt 4 * pR
    # discExpr = rSub (Mul sExpr sExpr) (Mul (Lit (ratFromInt 4)) pExpr)
    # sqrtDisc = Root 2 discExpr
    # ePlus = Mul (Inv (Lit (ratFromInt 2))) (Add sExpr sqrtDisc)
    # eMinus = Mul (Inv (Lit (ratFromInt 2))) (rSub sExpr sqrtDisc)
    # sqrtDiscVal = if (ratToReal discR >= 0.0)
                    (sqrt (ratToReal discR), 0.0)
                    (0.0, sqrt (abs (ratToReal discR)))
    # d1PlusVal = cmulS 0.5 (cadd s sqrtDiscVal)
    | cmagR (csub d1PlusVal d1) < cmagR (csub d1PlusVal d2)
        = ?Just (ePlus, eMinus)
    = ?Just (eMinus, ePlus)

fillInPairs :: !Int !(RadExpr Rational) ![(RadExpr Rational, RadExpr Rational)] -> [RadExpr Rational]
fillInPairs n d0Expr pairExprs
    // Build a list of (index, expr) assignments, then assemble the result
    # assignments = [(0, d0Expr) : flattenPairs 1 pairExprs n]
    = [lookupOr (Lit (ratFromInt 0)) i assignments \\ i <- [0 .. n - 1]]

flattenPairs :: !Int ![(RadExpr Rational, RadExpr Rational)] !Int -> [(Int, RadExpr Rational)]
flattenPairs _ [] _ = []
flattenPairs s [(eS, eNS) : rest] n = [(s, eS), (n - s, eNS) : flattenPairs (s + 1) rest n]

lookupOr :: !(RadExpr Rational) !Int ![(Int, RadExpr Rational)] -> RadExpr Rational
lookupOr def _ [] = def
lookupOr def i [(j, e) : rest]
    | i == j = e
    = lookupOr def i rest

// ─── Orbit-based matching for larger stabilisers ───

matchDsViaOrbits :: !TransitiveGroup ![(Real, Real)] !Int -> ?([RadExpr Rational])
matchDsViaOrbits tg dVals n
    // Try all rational first
    = case matchDsAllRational dVals of
        ?Just exprs -> ?Just exprs
        ?None ->
            // d_0 rational, rest via Q(ω_n)
            case matchRatStrict (hd dVals) of
                ?None -> ?None
                ?Just d0Expr ->
                    case mapM_matchQOmegaN n (tl dVals) of
                        ?None -> ?None
                        ?Just restExprs -> ?Just [d0Expr : restExprs]

matchRatStrict :: !(Real, Real) -> ?(RadExpr Rational)
matchRatStrict (re, im)
    | abs im > 0.05 = ?None
    = ?Just (Lit (approxRatRT re))

mapM_matchQOmegaN :: !Int ![(Real, Real)] -> ?([RadExpr Rational])
mapM_matchQOmegaN _ [] = ?Just []
mapM_matchQOmegaN n [d:ds]
    = case matchQOmegaN n d of
        ?None -> ?None
        ?Just e -> case mapM_matchQOmegaN n ds of
            ?None -> ?None
            ?Just rest -> ?Just [e : rest]

// Match to Q(ω_n): try single-term r·ω_n^k, then 2-term, then give up.
matchQOmegaN :: !Int !(Real, Real) -> ?(RadExpr Rational)
matchQOmegaN n d
    # tau = 2.0 * pi / toReal n
    # candidates = [(k, cmulRT d (cos (tau * toReal (~k)), sin (tau * toReal (~k))),
                     scoreRational (cmulRT d (cos (tau * toReal (~k)), sin (tau * toReal (~k)))))
                   \\ k <- [0 .. n - 1]]
    # (bestK, bestV, bestScore) = hd (sortBy (\(_, _, a) (_, _, b) -> a < b) candidates)
    | bestScore < 0.01
        # r = approxRatRT (fst bestV)
        # omExpr = omegaNExpr n
        | bestK == 0 = ?Just (Lit r)
        = ?Just (Mul (Lit r) (omegaPowN n omExpr bestK))
    // Try 2-term decomposition
    = matchTwoTermQOmegaN n d

matchTwoTermQOmegaN :: !Int !(Real, Real) -> ?(RadExpr Rational)
matchTwoTermQOmegaN n d
    # tau = 2.0 * pi / toReal n
    # omExpr = omegaNExpr n
    # twoTermCands = [(err, j, aR, k, bR)
                     \\ j <- [0 .. n - 1], k <- [j + 1 .. n - 1]
                     , let wj = (cos (tau * toReal j), sin (tau * toReal j))
                     , let wk = (cos (tau * toReal k), sin (tau * toReal k))
                     , let det = fst wj * snd wk - snd wj * fst wk
                     | abs det > 0.0000000001
                     , let a = (fst d * snd wk - snd d * fst wk) / det
                     , let b = (fst wj * snd d - snd wj * fst d) / det
                     , let aR = approxRatRT a
                     , let bR = approxRatRT b
                     , let reconRe = ratToReal aR * fst wj + ratToReal bR * fst wk
                     , let reconIm = ratToReal aR * snd wj + ratToReal bR * snd wk
                     , let err = abs (reconRe - fst d) + abs (reconIm - snd d)
                     | err < 0.01]
    = case sortBy (\(a, _, _, _, _) (b, _, _, _, _) -> a < b) twoTermCands of
        [(_, j, aR, k, bR) : _]
            # termJ = if (j == 0) (Lit aR) (Mul (Lit aR) (omegaPowN n omExpr j))
            # termK = if (k == 0) (Lit bR) (Mul (Lit bR) (omegaPowN n omExpr k))
            -> ?Just (Add termJ termK)
        [] -> ?None

// ─── Q(ω₅) matching ───

matchQOmega5 :: !(Real, Real) -> ?(RadExpr Rational)
matchQOmega5 d
    = case matchSingleOmega5 d of
        ?Just e -> ?Just e
        ?None -> case matchTwoTermOmega5 d of
            ?Just e -> ?Just e
            ?None -> matchGeneralQOmega5 d

matchSingleOmega5 :: !(Real, Real) -> ?(RadExpr Rational)
matchSingleOmega5 d
    # candidates = [(k, cmulRT d (omega5Conj k), scoreRational (cmulRT d (omega5Conj k)))
                   \\ k <- [0..4]]
    # (bestK, bestV, bestScore) = hd (sortBy (\(_, _, a) (_, _, b) -> a < b) candidates)
    | bestScore < 0.01
        # r = approxRatRT (fst bestV)
        | bestK == 0 = ?Just (Lit r)
        = ?Just (Mul (Lit r) (omegaPow5 bestK))
    = ?None

matchTwoTermOmega5 :: !(Real, Real) -> ?(RadExpr Rational)
matchTwoTermOmega5 d
    # tries = [(k, r, csub d (cmulS (ratToReal r) (omega5Val k)))
              \\ k <- [1..4]
              , let bApprox = fst (cmulRT d (omega5Conj k))
              , let r = approxRatRT bApprox]
    # scored = [(k, r, rv, scoreRational rv) \\ (k, r, rv) <- tries]
    # (bestK, bestR, _, bestScore) = hd (sortBy (\(_, _, _, a) (_, _, _, b) -> a < b) scored)
    | bestScore < 0.01
        # a = approxRatRT (fst (csub d (cmulS (ratToReal bestR) (omega5Val bestK))))
        = ?Just (Add (Lit a) (Mul (Lit bestR) (omegaPow5 bestK)))
    = ?None

matchGeneralQOmega5 :: !(Real, Real) -> ?(RadExpr Rational)
matchGeneralQOmega5 (re, im)
    # s1 = sin (2.0 * pi / 5.0)
    # s2 = sin (4.0 * pi / 5.0)
    # c1 = cos (2.0 * pi / 5.0)
    # c2 = cos (4.0 * pi / 5.0)
    # candidates = [(err, a0R, a1R, a2R, a3R)
                   \\ a1R <- candidateRats (im / s1)
                   , let a1 = ratToReal a1R
                   , let v = (im - a1 * s1) / s2
                   , vR <- candidateRats v
                   , uR <- candidateRats ((re - a1 * c1) / c2) ++ [ratFromInt 0]
                   , let a0D = re - a1 * c1 - ratToReal uR * c2
                   , a0R <- candidateRats a0D
                   , let a2R = (uR + vR) / ratFromInt 2
                   , let a3R = (uR - vR) / ratFromInt 2
                   , let recon = ratToReal a0R + ratToReal a1R * c1 + (ratToReal a2R + ratToReal a3R) * c2
                   , let reconIm = ratToReal a1R * s1 + (ratToReal a2R - ratToReal a3R) * s2
                   , let err = abs (recon - re) + abs (reconIm - im)
                   | err < 0.01]
    | isEmpty candidates = ?None
    # (err, a0R, a1R, a2R, a3R) = hd (sortBy (\(a, _, _, _, _) (b, _, _, _, _) -> a < b) candidates)
    | err < 0.01 = ?Just (buildQOmega5Expr a0R a1R a2R a3R)
    = ?None

candidateRats :: !Real -> [Rational]
candidateRats x
    # r = approxRatRT x
    | abs (ratToReal r - x) < 0.01 = [r]
    = []

buildQOmega5Expr :: !Rational !Rational !Rational !Rational -> RadExpr Rational
buildQOmega5Expr a0 a1 a2 a3
    # terms = [(a0, 0), (a1, 1), (a2, 2), (a3, 3)]
    # nonzero = [(c, k) \\ (c, k) <- terms | c <> ratFromInt 0]
    = case [mkTermOm5 c k \\ (c, k) <- nonzero] of
        [] -> Lit (ratFromInt 0)
        [t] -> t
        [t:ts] -> foldl Add t ts

mkTermOm5 :: !Rational !Int -> RadExpr Rational
mkTermOm5 c 0 = Lit c
mkTermOm5 c k = Mul (Lit c) (omegaPow5 k)

// ─── ω₅ expressions ───

omega5Expr :: RadExpr Rational
omega5Expr
    # cos5 = Mul (Inv (Lit (ratFromInt 4))) (rSub (Root 2 (Lit (ratFromInt 5))) (Lit (ratFromInt 1)))
    # sin5 = Mul (Inv (Lit (ratFromInt 4))) (Root 2 (Add (Lit (ratFromInt 10)) (Mul (Lit (ratFromInt 2)) (Root 2 (Lit (ratFromInt 5))))))
    # i = Root 2 (Lit (ratFromInt (~1)))
    = Add cos5 (Mul i sin5)

omegaPow5 :: !Int -> RadExpr Rational
omegaPow5 0 = Lit (ratFromInt 1)
omegaPow5 1 = omega5Expr
omegaPow5 k = Pow omega5Expr (k rem 5)

omega5Val :: !Int -> (Real, Real)
omega5Val k = (cos (2.0 * pi * toReal k / 5.0), sin (2.0 * pi * toReal k / 5.0))

omega5Conj :: !Int -> (Real, Real)
omega5Conj k = (cos (2.0 * pi * toReal (~k) / 5.0), sin (2.0 * pi * toReal (~k) / 5.0))

// ─── ω_n expressions (general) ───

omegaNExpr :: !Int -> RadExpr Rational
omegaNExpr 5 = omega5Expr  // fast path
omegaNExpr n
    = case cosOfUnity n of
        ?Just cosE -> case sinOfUnity n of
            ?Just sinE ->
                # i = Root 2 (Lit (ratFromInt (~1)))
                -> Add cosE (Mul i sinE)
            ?None -> fallbackOmega n
        ?None -> fallbackOmega n

fallbackOmega :: !Int -> RadExpr Rational
fallbackOmega n
    # theta = 2.0 * pi / toReal n
    # cosVal = cos theta
    # sinVal = sin theta
    = Add (Lit (approxRatRT cosVal))
          (Mul (Root 2 (Lit (ratFromInt (~1)))) (Lit (approxRatRT sinVal)))

omegaPowN :: !Int !(RadExpr Rational) !Int -> RadExpr Rational
omegaPowN _ _ 0 = Lit (ratFromInt 1)
omegaPowN n omExpr 1 = omExpr
omegaPowN n omExpr k
    # k` = k rem n
    | k` == 0 = Lit (ratFromInt 1)
    | k` == 1 = omExpr
    = Pow omExpr k`

// ─── Branch selection ───

selectBranch5 :: !(RadExpr Rational) !(Real, Real) -> RadExpr Rational
selectBranch5 rj5Expr targetVal
    # rj5Val = dagEvalComplex (toDAG rj5Expr)
    # principalVal = cnthroot 5 rj5Val
    # principalRoot = Root 5 rj5Expr
    # scored = [(k, cmagR (csub (cmulRT (omega5Val k) principalVal) targetVal)) \\ k <- [0..4]]
    # bestK = fst (hd (sortBy (\(_, a) (_, b) -> a < b) scored))
    | bestK == 0 = principalRoot
    = Mul (omegaPow5 bestK) principalRoot

selectBranchN :: !Int !(RadExpr Rational) !(RadExpr Rational) !(Real, Real) -> RadExpr Rational
selectBranchN n omExpr rjnExpr targetVal
    # rjnVal = dagEvalComplex (toDAG rjnExpr)
    # principalVal = cnthroot n rjnVal
    # principalRoot = Root n rjnExpr
    # tau = 2.0 * pi / toReal n
    # scored = [(k, cmagR (csub (cmulRT (cos (tau * toReal k), sin (tau * toReal k)) principalVal) targetVal))
               \\ k <- [0 .. n - 1]]
    # bestK = fst (hd (sortBy (\(_, a) (_, b) -> a < b) scored))
    | bestK == 0 = principalRoot
    = Mul (omegaPowN n omExpr bestK) principalRoot

// Principal complex nth root
cnthroot :: !Int !(Real, Real) -> (Real, Real)
cnthroot n (re, im)
    # r = sqrt (re*re + im*im)
    # theta = atan2 im re
    # rn = r ^ (1.0 / toReal n)
    # tn = theta / toReal n
    = (rn * cos tn, rn * sin tn)

// ─── Scoring ───

scoreRational :: !(Real, Real) -> Real
scoreRational (re, im) = abs im + fracPart re

fracPart :: !Real -> Real
fracPart x = abs (x - toReal (toInt (x + if (x >= 0.0) 0.5 (~0.5))))

// ─── Match to original root ordering ───

matchToOriginal :: ![RadExpr Rational] ![(Real, Real)] -> [RadExpr Rational]
matchToOriginal exprs numRoots
    # exprVals = [(e, dagEvalComplex (toDAG e)) \\ e <- exprs]
    = [fst (hd (sortBy (\(_, v1) (_, v2) ->
        cmagR (csub v1 t) < cmagR (csub v2 t)) exprVals))
      \\ t <- numRoots]

// ─── DAG helpers ───

dagFoldE :: !(RadExpr Rational) -> RadExpr Rational
dagFoldE e = fromDAG (dagFoldConstants (toDAG e))

// ─── Permutations ───

permsI :: ![Int] -> [[Int]]
permsI [] = [[]]
permsI xs = [x : rest \\ (x, ys) <- picksI xs, rest <- permsI ys]

picksI :: ![Int] -> [(Int, [Int])]
picksI [] = []
picksI [y:ys] = [(y, ys) : [(z, [y:zs]) \\ (z, zs) <- picksI ys]]

// ─── Complex arithmetic ───

sumC :: ![(Real, Real)] -> (Real, Real)
sumC [] = (0.0, 0.0)
sumC [x:xs] = cadd x (sumC xs)

caddRT :: !(Real, Real) !(Real, Real) -> (Real, Real)
caddRT (a, b) (c, d) = (a + c, b + d)

cmulRT :: !(Real, Real) !(Real, Real) -> (Real, Real)
cmulRT (a, b) (c, d) = (a*c - b*d, a*d + b*c)

cpowR :: !(Real, Real) !Int -> (Real, Real)
cpowR _ 0 = (1.0, 0.0)
cpowR z n
    | isOdd n = cmulRT z (cpowR z (n - 1))
    = let half = cpowR z (n / 2) in cmulRT half half

// ─── Misc helpers ───

getCoeffsRT :: !(Poly Rational) -> [Rational]
getCoeffsRT (Poly cs) = cs

foldl1Expr :: ((RadExpr Rational) (RadExpr Rational) -> RadExpr Rational) ![RadExpr Rational] -> RadExpr Rational
foldl1Expr _ [x] = x
foldl1Expr f [x:xs] = foldl f x xs
foldl1Expr _ [] = Lit (ratFromInt 0)

approxRatRT :: !Real -> Rational
approxRatRT x
    # candidates = [(abs (toReal n / toReal d - x), mkRational (toInteger n) (toInteger d))
                    \\ d <- [1..1000], let n = toInt (x * toReal d + if (x >= 0.0) 0.5 (~0.5))
                    | abs (toReal n / toReal d - x) < 0.000001]
    | isEmpty candidates = ratFromInt (toInt (if (x >= 0.0) (x + 0.5) (x - 0.5)))
    = snd (hd (sortBy (\(a,_) (b,_) -> a < b) candidates))

ratToReal :: !Rational -> Real
ratToReal r = toReal (toInt (numer r)) / toReal (toInt (denom r))

pi :: Real
pi = 3.14159265358979323846
