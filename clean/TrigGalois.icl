implementation module TrigGalois

import StdEnv
import RadExpr
import Rational
import Positive
import PrimeFactors
import Eval
import DAG
import Data.Integer

// ─── Period state ───
:: PeriodState =
    { periodExpr  :: RadExpr Rational
    , periodElems :: [Int]
    , periodP     :: Int
    }

// ─── Euler totient ───
eulerTotientI :: !Int -> Int
eulerTotientI n
    | n <= 0 = abort "eulerTotientI: non-positive"
    | n == 1 = 1
    | otherwise
        # fs = factorise (unsafePositive n)
        = prodI [(p - 1) * intPow p (e - 1) \\ (p, e) <- fs]

prodI :: ![Int] -> Int
prodI [] = 1
prodI [x:xs] = x * prodI xs

intPow :: !Int !Int -> Int
intPow _ 0 = 1
intPow b e
    | isOdd e = b * intPow b (e - 1)
    = let half = intPow b (e / 2) in half * half

// ─── Modular exponentiation ───
modExp :: !Int !Int !Int -> Int
modExp _ 0 _ = 1
modExp b e m
    | isEven e = let half = modExp b (e / 2) m in (half * half) rem m
    = (b * modExp b (e - 1) m) rem m

// ─── Primitive root finding ───
primitiveRoot :: !Int -> ?(Int)
primitiveRoot p
    | p <= 1 = ?None
    | p == 2 = ?Just 1
    | otherwise
        # phi = p - 1
        # factors = primeFactors (unsafePositive phi)
        = findPrimRoot 2 p phi factors

findPrimRoot :: !Int !Int !Int ![Int] -> ?(Int)
findPrimRoot g p phi factors
    | g >= p = ?None
    | isPrimRoot g p phi factors = ?Just g
    = findPrimRoot (g + 1) p phi factors

isPrimRoot :: !Int !Int !Int ![Int] -> Bool
isPrimRoot g p phi factors
    = all (\q -> modExp g (phi / q) p <> 1) factors

primitiveRootMod :: !Int -> ?(Int)
primitiveRootMod n
    | n <= 0 = ?None
    | n <= 2 = ?Just 1
    | n == 4 = ?Just 3
    | otherwise
        # fs = factorise (unsafePositive n)
        = primRootModFromFactors n fs

primRootModFromFactors :: !Int ![(Int, Int)] -> ?(Int)
primRootModFromFactors n [(p, _)]
    | p > 2 = findPrimRootMod n
primRootModFromFactors n [(2, 1), (p, _)]
    | p > 2 = findPrimRootMod n
primRootModFromFactors _ _ = ?None

findPrimRootMod :: !Int -> ?(Int)
findPrimRootMod n
    # phi = eulerTotientI n
    # factors = primeFactors (unsafePositive phi)
    = findPRM 2 n phi factors

findPRM :: !Int !Int !Int ![Int] -> ?(Int)
findPRM g n phi factors
    | g >= n = ?None
    | gcdI g n == 1 && isPrimRoot g n phi factors = ?Just g
    = findPRM (g + 1) n phi factors

gcdI :: !Int !Int -> Int
gcdI a 0 = abs a
gcdI a b = gcdI b (a rem b)

// ─── All periods via Gauss ───
cosOfUnityViaGauss :: !Int -> ?(RadExpr Rational)
cosOfUnityViaGauss n
    = cosOfUnityViaGaussHelper (allPeriodsViaGauss n) n

cosOfUnityViaGaussHelper :: !(?([(Int, RadExpr Rational)])) !Int -> ?(RadExpr Rational)
cosOfUnityViaGaussHelper ?None _ = ?None
cosOfUnityViaGaussHelper (?Just periods) n
    = cosOfUnityFromPeriods (lookupPeriod 1 periods) (lookupPeriod (n - 1) periods)

cosOfUnityFromPeriods :: !(?(RadExpr Rational)) !(?(RadExpr Rational)) -> ?(RadExpr Rational)
cosOfUnityFromPeriods (?Just t) (?Just c) = ?Just (Mul (Inv (Lit (ratFromInt 2))) (Add t c))
cosOfUnityFromPeriods _ _ = ?None

lookupPeriod :: !Int ![(Int, RadExpr Rational)] -> ?(RadExpr Rational)
lookupPeriod _ [] = ?None
lookupPeriod k [(k`, e):rest]
    | k == k` = ?Just e
    = lookupPeriod k rest

allPeriodsViaGauss :: !Int -> ?([(Int, RadExpr Rational)])
allPeriodsViaGauss n
    | n <= 2 = ?None
    = allPeriodsHelper n (primitiveRootMod n)

allPeriodsHelper :: !Int !(?(Int)) -> ?([(Int, RadExpr Rational)])
allPeriodsHelper _ ?None = ?None
allPeriodsHelper n (?Just g)
    # phi = eulerTotientI n
    # fs = factorise (unsafePositive phi)
    # steps = flatten [repeatn e` q \\ (q, e`) <- reorderFactors n fs]
    # coprimeElems = [modExp g k n \\ k <- [0 .. phi - 1]]
    # initSumD = sum [cos (2.0 * 3.14159265358979323846 * toReal k / toReal n)
                        \\ k <- coprimeElems]
    # initSumR = ratFromInt (toInt (roundR initSumD))
    # initExpr = Lit initSumR
    # initPeriod = { periodExpr = initExpr
                   , periodElems = coprimeElems
                   , periodP = n
                   }
    # finalPeriods = foldl descendStep [initPeriod] steps
    # periodList = [(k, ps.periodExpr) \\ ps <- finalPeriods, k <- ps.periodElems]
    = ?Just periodList

roundR :: !Real -> Real
roundR x
    | x >= 0.0 = toReal (toInt (x + 0.5))
    = toReal (toInt (x - 0.5))

reorderFactors :: !Int ![(Int, Int)] -> [(Int, Int)]
reorderFactors n fs
    # nfs = factorise (unsafePositive n)
    = reorderFromNfs nfs fs

reorderFromNfs :: ![(Int, Int)] ![(Int, Int)] -> [(Int, Int)]
reorderFromNfs [(p, k)] fs
    | k > 1 = [(q, e) \\ (q, e) <- fs | q == p] ++ [(q, e) \\ (q, e) <- fs | q <> p]
reorderFromNfs _ fs = fs

// ─── Descent step ───
descendStep :: ![PeriodState] !Int -> [PeriodState]
descendStep periods q = flatten [splitPeriod periods q ps \\ ps <- periods]

splitPeriod :: ![PeriodState] !Int !PeriodState -> [PeriodState]
splitPeriod allPeriods q parent
    # elems = parent.periodElems
    # f = length elems
    # subF = f / q
    # subPeriodElems = [[elems !! (k + q * j) \\ j <- [0 .. subF - 1]] \\ k <- [0 .. q - 1]]
    # p = parent.periodP
    # subPeriodValues = [sumRootsOfUnity p es \\ es <- subPeriodElems]
    # subPeriodExprs = solvePeriodEq q allPeriods p (parent.periodExpr) subPeriodElems subPeriodValues
    = [{ periodExpr = expr, periodElems = elms, periodP = p }
       \\ (expr, elms) <- zip2 subPeriodExprs subPeriodElems]

// Sum of e^{2pi i k/n} for list of k values
sumRootsOfUnity :: !Int ![Int] -> (Real, Real)
sumRootsOfUnity n ks
    # tau = 2.0 * 3.14159265358979323846 / toReal n
    = foldl (\(re, im) k -> (re + cos (tau * toReal k), im + sin (tau * toReal k))) (0.0, 0.0) ks

// ─── Elementary symmetric polynomials ───
elementarySymmetricC :: ![(Real, Real)] -> [(Real, Real)]
elementarySymmetricC xs = [elemSym k xs \\ k <- [1 .. length xs]]

elemSym :: !Int ![(Real, Real)] -> (Real, Real)
elemSym 1 ys = foldl cadd (0.0, 0.0) ys
elemSym k ys = foldl cadd (0.0, 0.0) [prodC combo \\ combo <- choose k ys]

prodC :: ![(Real, Real)] -> (Real, Real)
prodC [] = (1.0, 0.0)
prodC [x:xs] = cmulT x (prodC xs)

choose :: !Int ![(Real, Real)] -> [[(Real, Real)]]
choose 0 _ = [[]]
choose _ [] = []
choose k [y:ys] = [[y:rest] \\ rest <- choose (k - 1) ys] ++ choose k ys

// Complex arithmetic
cadd :: !(Real, Real) !(Real, Real) -> (Real, Real)
cadd (a, b) (c, d) = (a + c, b + d)

cmulT :: !(Real, Real) !(Real, Real) -> (Real, Real)
cmulT (a, b) (c, d) = (a*c - b*d, a*d + b*c)

cmag :: !(Real, Real) -> Real
cmag (a, b) = sqrt (a*a + b*b)

// ─── Period equation solving ───
solvePeriodEq :: !Int ![PeriodState] !Int !(RadExpr Rational)
              ![[Int]] ![(Real, Real)] -> [RadExpr Rational]
solvePeriodEq 2 allPeriods p e1Expr _ numVals
    // Quadratic: t^2 - e1*t + e2 = 0
    # symFuncs = elementarySymmetricC numVals
    # e2Expr = matchToPeriodExpr allPeriods p (symFuncs !! 1)
    # disc = Add (Mul e1Expr e1Expr) (Neg (Mul (Lit (ratFromInt 4)) e2Expr))
    # sqrtDisc = Root 2 disc
    # root1 = Mul (Inv (Lit (ratFromInt 2))) (Add e1Expr sqrtDisc)
    # root2 = Mul (Inv (Lit (ratFromInt 2))) (Add e1Expr (Neg sqrtDisc))
    = assignByValue [dagFold root1, dagFold root2] numVals
solvePeriodEq 3 allPeriods p e1Expr _ numVals
    // Cubic via Cardano
    # symFuncs = elementarySymmetricC numVals
    # e2Expr = matchToPeriodExpr allPeriods p (symFuncs !! 1)
    # e3Expr = matchToPeriodExpr allPeriods p (symFuncs !! 2)
    # pExpr = Add e2Expr (Neg (Mul (Inv (Lit (ratFromInt 3))) (Mul e1Expr e1Expr)))
    # rat2over27 = mkRational (toInteger 2) (toInteger 27)
    # qExpr = Add (Add (Neg e3Expr) (Mul (Inv (Lit (ratFromInt 3))) (Mul e1Expr e2Expr)))
                  (Neg (Mul (Lit rat2over27) (Mul e1Expr (Mul e1Expr e1Expr))))
    # delta = Add (Mul (Inv (Lit (ratFromInt 4))) (Mul qExpr qExpr))
                  (Mul (Inv (Lit (ratFromInt 27))) (Mul pExpr (Mul pExpr pExpr)))
    # sqrtDelta = Root 2 delta
    # negQHalf = Mul (Inv (Lit (ratFromInt (~2)))) qExpr
    # u1 = Root 3 (Add negQHalf sqrtDelta)
    # u2 = Mul (Neg pExpr) (Inv (Mul (Lit (ratFromInt 3)) u1))
    # shift = Mul (Inv (Lit (ratFromInt 3))) e1Expr
    # omega = Mul (Inv (Lit (ratFromInt 2))) (Add (Lit (ratFromInt (~1))) (Root 2 (Lit (ratFromInt (~3)))))
    # omegaBar = Mul (Inv (Lit (ratFromInt 2))) (Add (Lit (ratFromInt (~1))) (Neg (Root 2 (Lit (ratFromInt (~3))))))
    # root0 = Add (Add u1 u2) shift
    # root1 = Add (Add (Mul omega u1) (Mul omegaBar u2)) shift
    # root2 = Add (Add (Mul omegaBar u1) (Mul omega u2)) shift
    = assignByValue [dagFold root0, dagFold root1, dagFold root2] numVals
solvePeriodEq q allPeriods p parentExpr subPeriodElems numVals
    = solvePeriodViaResolvent q allPeriods p parentExpr subPeriodElems numVals

dagFold :: !(RadExpr Rational) -> RadExpr Rational
dagFold e = fromDAG (dagFoldConstants (toDAG e))

// ─── Lagrange resolvent for q >= 5 ───
solvePeriodViaResolvent :: !Int ![PeriodState] !Int !(RadExpr Rational)
                        ![[Int]] ![(Real, Real)] -> [RadExpr Rational]
solvePeriodViaResolvent q allPeriods p parentExpr subPeriodElems _
    # tau = 2.0 * 3.14159265358979323846 / toReal q
    # subPeriodValues = [sumRootsOfUnity p es \\ es <- subPeriodElems]
    # omegaC = [(cos (tau * toReal m), sin (tau * toReal m)) \\ m <- [0 .. q - 1]]
    # resolvents = [foldl cadd (0.0, 0.0)
                     [cmulT (omegaC !! ((j * k) rem q)) (subPeriodValues !! k)
                      \\ k <- [0 .. q - 1]]
                   \\ j <- [0 .. q - 1]]
    # resolventPowers = [cpowT (resolvents !! j) q \\ j <- [0 .. q - 1]]
    # dCoeffs = [computeDCoeff q omegaC resolventPowers s \\ s <- [0 .. q - 1]]
    # periodVals = [sumRootsOfUnity p ps.periodElems \\ ps <- allPeriods]
    # dExprs = [matchToPeriodExprC allPeriods p (dCoeffs !! s) periodVals \\ s <- [0 .. q - 1]]
    # cosBaseExpr = cosOfUnityViaGaussDefault q
    # omegaPowers = [omegaPowerExpr q cosBaseExpr m \\ m <- [0 .. q - 1]]
    # resolventPowerExprs =
        [foldl1 Add [Mul (dExprs !! s) (omegaPowers !! ((j * s) rem q)) \\ s <- [0 .. q - 1]]
         \\ j <- [1 .. q - 1]]
    # resolventExprs = [selectBranch q omegaPowers (resolventPowerExprs !! (j - 1)) (resolvents !! j)
                        \\ j <- [1 .. q - 1]]
    # allRes = [parentExpr : resolventExprs]
    = [Mul (Inv (Lit (ratFromInt q)))
           (foldl1 Add [Mul (omegaPowers !! ((q - ((j * k) rem q)) rem q)) (allRes !! j)
                         \\ j <- [0 .. q - 1]])
       \\ k <- [0 .. q - 1]]

cosOfUnityViaGaussDefault :: !Int -> RadExpr Rational
cosOfUnityViaGaussDefault q
    = cosOfUnityViaGaussDefault2 (cosOfUnityViaGauss q)

cosOfUnityViaGaussDefault2 :: !(?(RadExpr Rational)) -> RadExpr Rational
cosOfUnityViaGaussDefault2 (?Just e) = e
cosOfUnityViaGaussDefault2 ?None = Lit (ratFromInt 0)

computeDCoeff :: !Int ![(Real, Real)] ![(Real, Real)] !Int -> (Real, Real)
computeDCoeff q omegaC resolventPowers s
    # (re, im) = foldl cadd (0.0, 0.0)
          [cmulT (omegaC !! ((q - ((j * s) rem q)) rem q))
                 (resolventPowers !! j)
           \\ j <- [0 .. q - 1]]
    = (re / toReal q, im / toReal q)

foldl1 :: (a a -> a) ![a] -> a
foldl1 _ [x] = x
foldl1 f [x:xs] = foldl f x xs
foldl1 _ [] = abort "foldl1: empty list"

cpowT :: !(Real, Real) !Int -> (Real, Real)
cpowT _ 0 = (1.0, 0.0)
cpowT z n
    | n < 0
        # (a, b) = cpowT z (0 - n)
        # m = a*a + b*b
        = (a / m, ~ b / m)
    | isOdd n = cmulT z (cpowT z (n - 1))
    = let half = cpowT z (n / 2) in cmulT half half

// ─── omega^m as RadExpr ───
omegaPowerExpr :: !Int !(RadExpr Rational) !Int -> RadExpr Rational
omegaPowerExpr _ _ 0 = Lit (ratFromInt 1)
omegaPowerExpr q cosBase m
    # m` = m rem q
    | m` == 0 = Lit (ratFromInt 1)
    # cosM = chebyshevT m` cosBase
    # sin2M = Add (Lit (ratFromInt 1)) (Neg (Mul cosM cosM))
    # sinM = Root 2 sin2M
    # sinMSigned = if (2 * m` < q) sinM (Neg sinM)
    # i = Root 2 (Lit (ratFromInt (~1)))
    = Add cosM (Mul i sinMSigned)

chebyshevT :: !Int !(RadExpr Rational) -> RadExpr Rational
chebyshevT 0 _ = Lit (ratFromInt 1)
chebyshevT 1 x = x
chebyshevT k x = goCheb 2 (Lit (ratFromInt 1)) x k x
where
    goCheb n t0 t1 kk xx
        | n > kk = t1
        # t2 = Add (Mul (Mul (Lit (ratFromInt 2)) xx) t1) (Neg t0)
        = goCheb (n + 1) t1 t2 kk xx

// ─── Matching ───
matchToPeriodExpr :: ![PeriodState] !Int !(Real, Real) -> RadExpr Rational
matchToPeriodExpr periods p target
    # nearestInt = toInt (roundR (fst target))
    | cmag (fst target - toReal nearestInt, snd target) < 0.00000001
        = Lit (ratFromInt nearestInt)
    # periodVals = [sumRootsOfUnity p ps.periodElems \\ ps <- periods]
    = matchToPeriodExprC periods p target periodVals

matchToPeriodExprC :: ![PeriodState] !Int !(Real, Real) ![(Real, Real)] -> RadExpr Rational
matchToPeriodExprC periods p target periodVals
    # results = [matchSingle target pv \\ pv <- periodVals]
    # singleMatches = [(i, c, a) \\ (?Just (c, a)) <- results & i <- [0..]]
    | not (isEmpty singleMatches)
        # (i, c, a) = hd singleMatches
        # constPart = Lit (ratFromInt c)
        # periodPart = if (a == 0) (Lit (ratFromInt 0))
                       (if (a == 1) (periods !! i).periodExpr
                        (Mul (Lit (ratFromInt a)) (periods !! i).periodExpr))
        = Add constPart periodPart
    # re = fst target
    # im = snd target
    | abs im < 0.000000000001 = Lit (approxRat re)
    = Add (Lit (approxRat re)) (Mul (Lit (approxRat im)) (Root 2 (Lit (ratFromInt (~1)))))

matchSingle :: !(Real, Real) !(Real, Real) -> ?((Int, Int))
matchSingle target v
    | abs (snd v) > 0.0000000001
        # a = toInt (roundR (snd target / snd v))
        # remainder_re = fst target - toReal a * fst v
        # c = toInt (roundR remainder_re)
        # err = cmag (fst target - toReal c - toReal a * fst v,
                      snd target - toReal a * snd v)
        | err / max 1.0 (cmag target) < 0.000001 = ?Just (c, a)
        = ?None
    | abs (fst v) > 0.0000000001
        # a = toInt (roundR (fst target / fst v))
        # c = toInt (roundR (fst target - toReal a * fst v))
        # err = cmag (fst target - toReal c - toReal a * fst v, snd target)
        | err / max 1.0 (cmag target) < 0.000001 = ?Just (c, a)
        = ?None
    = ?None

approxRat :: !Real -> Rational
approxRat x
    # candidates = [(abs (toReal n / toReal d - x), mkRational (toInteger n) (toInteger d))
                    \\ d <- [1..1000]
                    , let n = toInt (roundR (x * toReal d))
                    | abs (toReal n / toReal d - x) < 0.000001]
    | isEmpty candidates = ratFromInt (toInt (roundR x))
    = snd (hd (sortBy (\(a,_) (b,_) -> a < b) candidates))

// ─── Branch selection ───
selectBranch :: !Int ![RadExpr Rational] !(RadExpr Rational) !(Real, Real) -> RadExpr Rational
selectBranch q omegaPowers rjqExpr targetVal
    # principalRoot = Root q rjqExpr
    # rjqVal = evalComplex rjqExpr
    # rMag = cmag rjqVal
    # rPhase = atan2R (snd rjqVal) (fst rjqVal)
    # rn = rMag ^ (1.0 / toReal q)
    # an = rPhase / toReal q
    # principalVal = (rn * cos an, rn * sin an)
    # tau = 2.0 * 3.14159265358979323846 / toReal q
    # scored = [scoreCandidate targetVal principalVal tau k \\ k <- [0 .. q - 1]]
    # best = hd (sortBy (\(_,a) (_,b) -> a < b) scored)
    # bestK = fst best
    | bestK == 0 = principalRoot
    = Mul (omegaPowers !! bestK) principalRoot

scoreCandidate :: !(Real, Real) !(Real, Real) !Real !Int -> (Int, Real)
scoreCandidate targetVal principalVal tau k
    # omk = (cos (tau * toReal k), sin (tau * toReal k))
    # candidate = cmulT omk principalVal
    = (k, cmag (fst targetVal - fst candidate, snd targetVal - snd candidate))

atan2R :: !Real !Real -> Real
atan2R y x
    | x > 0.0 = atan (y / x)
    | x < 0.0 && y >= 0.0 = atan (y / x) + 3.14159265358979323846
    | x < 0.0 && y < 0.0  = atan (y / x) - 3.14159265358979323846
    | x == 0.0 && y > 0.0  = 3.14159265358979323846 / 2.0
    | x == 0.0 && y < 0.0  = ~ (3.14159265358979323846 / 2.0)
    = 0.0

// ─── Assign by value ───
assignByValue :: ![RadExpr Rational] ![(Real, Real)] -> [RadExpr Rational]
assignByValue exprs vals
    # exprVals = [(e, evalComplex e) \\ e <- exprs]
    = [pickClosest exprVals v \\ v <- vals]
where
    pickClosest evs target
        = fst (hd (sortBy (\(_, a) (_, b) ->
            cmag (fst a - fst target, snd a - snd target) <
            cmag (fst b - fst target, snd b - snd target)) evs))
