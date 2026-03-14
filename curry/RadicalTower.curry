--- Radical tower construction for solvable polynomials.
---
--- Given an irreducible polynomial f(x) in Q[x] of degree n with
--- solvable Galois group G, construct radical expressions for its
--- roots via Lagrange resolvents descending through the composition
--- series of G.
---
--- Supports all prime degrees via the generalized 9-step pipeline:
---   1. Depress (eliminate second-highest term)
---   2. Find cyclic ordering
---   3. Lagrange resolvents R_j
---   4. DFT: d_s from R_j^n
---   5. Coefficient matching (rational or via orbits)
---   6. Reconstruct R_j^n as RadExpr
---   7. Branch selection
---   8. Inverse DFT to recover roots
---   9. Un-depress and match to original ordering
module RadicalTower
  ( solveViaTower
  , solveViaTowerN
  , showRadicalTower
  ) where

import Rational
import Poly
import RadExpr
import Eval (evalDouble, evalComplex, complexNthRoot)
import TransitiveGroup (TransitiveGroup, tgSolvable, tgOrder, tgName,
                         tgDegree)
import RootOfUnity (cosOfUnity, sinOfUnity)
import TrigGalois (allPeriodsViaGauss)

--- Local aliases.
rZero :: Rational
rZero = Rational.fromInt 0

rOne :: Rational
rOne = Rational.fromInt 1

------------------------------------------------------------------------
-- Entry points
------------------------------------------------------------------------

--- Solve a degree-5 polynomial via radical tower (legacy interface).
solveViaTower :: Poly -> TransitiveGroup -> Maybe [RadExpr Rational]
solveViaTower p g
  | not (tgSolvable g) = Nothing
  | degree p /= 5      = Nothing
  | otherwise           = solveSolvablePrime p g

--- Solve a polynomial of any prime degree via radical tower.
solveViaTowerN :: Poly -> TransitiveGroup -> Maybe [RadExpr Rational]
solveViaTowerN p g
  | not (tgSolvable g) = Nothing
  | otherwise           = solveSolvablePrime p g

------------------------------------------------------------------------
-- Core algorithm: generalized for all prime degrees
------------------------------------------------------------------------

--- Core 9-step pipeline for solvable polynomials of prime degree n.
solveSolvablePrime :: Poly -> TransitiveGroup -> Maybe [RadExpr Rational]
solveSolvablePrime p g =
  let n = degree p
      cs = polyCoeffsRT p
      lc = lastOfR cs
      monicCs = map (\c -> ratDiv c lc) cs
      an1 = monicCs !! (n - 1)
      shiftVal = ratNeg (ratDiv an1 (Rational.fromInt n))
      -- Approximate roots via Newton's method
      approxRoots = approximateRoots p
  in case approxRoots of
       Nothing -> Nothing
       Just numRoots ->
         let depRoots = map (\r -> complexSub r (ratToComplex shiftVal)) numRoots
         in case findCyclicOrdering depRoots n g of
              Nothing -> Nothing
              Just ordering ->
                let orderedRoots = [depRoots !! i | i <- ordering]
                    -- Step 3: Lagrange resolvents R_j = Σ_k ω_n^{jk} α_k
                    rjVals = [lagrangeResolvent orderedRoots n j | j <- [0 .. n - 1]]
                    rjPows = [complexPow (rjVals !! j) n | j <- [0 .. n - 1]]
                    -- Step 4: DFT: d_s = (1/n) Σ_j ω_n^{-js} R_j^n
                    dVals = [dftCoeff rjPows n s | s <- [0 .. n - 1]]
                    -- Step 5: Match d_s to radical expressions
                in case matchDsGeneral g dVals n of
                     Nothing -> Nothing
                     Just dExprs ->
                       let -- Step 6: R_j^n = Σ_s d_s · ω_n^{js}
                           omExpr = omegaNExpr n
                           rjPowExprs =
                             [ foldl1R Add
                                 [Mul (dExprs !! s) (omegaPowNExpr n omExpr ((j * s) `mod` n))
                                 | s <- [0 .. n - 1]]
                             | j <- [1 .. n - 1]
                             ]
                           -- Step 7: Branch selection
                           rjExprs =
                             [ selectBranchN n omExpr (rjPowExprs !! (j - 1)) (rjVals !! j)
                             | j <- [1 .. n - 1]
                             ]
                           allR = Lit rZero : rjExprs
                           -- Step 8: Inverse DFT: α_k = (1/n) Σ_j ω_n^{-jk} R_j
                           rootExprs =
                             [ Mul (Inv (Lit (Rational.fromInt n)))
                                   (foldl1R Add
                                     [ Mul (omegaPowNExpr n omExpr ((n - (j * k) `mod` n) `mod` n))
                                           (allR !! j)
                                     | j <- [0 .. n - 1]
                                     ])
                             | k <- [0 .. n - 1]
                             ]
                           -- Step 9: Un-depress
                           finalExprs = map (\e -> Add e (Lit shiftVal)) rootExprs
                       in Just (matchToOriginal finalExprs numRoots)

------------------------------------------------------------------------
-- Root approximation
------------------------------------------------------------------------

--- Approximate all roots of a polynomial using Newton's method.
approximateRoots :: Poly -> Maybe [(Float, Float)]
approximateRoots p =
  let d = degree p
      starts = [(cos (2.0 * pi * Prelude.fromInt k / Prelude.fromInt d),
                 sin (2.0 * pi * Prelude.fromInt k / Prelude.fromInt d))
               | k <- [0 .. d - 1]]
      roots = map (newtonComplex p 50) starts
  in Just roots

--- Newton's method for complex roots of a polynomial.
newtonComplex :: Poly -> Int -> (Float, Float) -> (Float, Float)
newtonComplex p iters z
  | iters == 0 = z
  | otherwise  =
      let p' = diffPoly p
          fz = evalPolyComplex p z
          fpz = evalPolyComplex p' z
          (fr, fi) = fz
          (gr, gi) = fpz
          d = gr * gr + gi * gi
      in if d < 1.0e-30 then z
         else let dr = (fr * gr + fi * gi) / d
                  di = (fi * gr - fr * gi) / d
                  z' = (fst z - dr, snd z - di)
              in newtonComplex p (iters - 1) z'

--- Evaluate polynomial at a complex point.
evalPolyComplex :: Poly -> (Float, Float) -> (Float, Float)
evalPolyComplex (Poly cs) z = case cs of
  [] -> (0.0, 0.0)
  _  -> foldr (\c acc -> complexAdd (ratToComplex c) (complexMul z acc))
              (0.0, 0.0) cs

------------------------------------------------------------------------
-- Cyclic ordering
------------------------------------------------------------------------

--- Find a cyclic ordering of the n numerical roots such that the
--- Galois generator acts as the n-cycle (0 1 ... n-1).
---
--- For n <= 8, tries all (n-1)! orderings (brute force).
--- For larger n, tries (n-1) rotations via greedy nearest-neighbour.
findCyclicOrdering :: [(Float, Float)] -> Int -> TransitiveGroup
                   -> Maybe [Int]
findCyclicOrdering roots n tg
  | n <= 8 =
      let rest = [1 .. n - 1]
          orderings = [0 : perm | perm <- permsOf rest]
          scored = [(o, scoreOrdering roots o n tg) | o <- orderings]
          sorted = sortByScore scored
      in case sorted of
           ((bestO, bestS) : (_, secondS) : _)
             | bestS < 5.0 * Prelude.fromInt n && bestS < 0.5 * secondS -> Just bestO
             | bestS < Prelude.fromInt n -> Just bestO
           ((bestO, bestS) : _)
             | bestS < Prelude.fromInt n -> Just bestO
           _ -> Nothing
  | otherwise = findCyclicOrderingByRotation roots n tg

--- For large primes, test (n-1) choices for α_1 = σ(α_0) and build
--- full ordering via greedy nearest-neighbour.
findCyclicOrderingByRotation :: [(Float, Float)] -> Int -> TransitiveGroup
                             -> Maybe [Int]
findCyclicOrderingByRotation roots n tg =
  let candidates =
        [ (ordering, scoreOrdering roots ordering n tg)
        | next <- [1 .. n - 1]
        , let ordering = buildOrdering roots n 0 next
        , length ordering == n
        ]
      sorted = sortByScore candidates
  in case sorted of
       ((bestO, bestS) : _)
         | bestS < 5.0 * Prelude.fromInt n -> Just bestO
       _ -> Nothing

--- Build ordering by greedy nearest-neighbour from a starting pair.
buildOrdering :: [(Float, Float)] -> Int -> Int -> Int -> [Int]
buildOrdering roots n start next =
  let step = complexSub (roots !! next) (roots !! start)
      go used pos acc
        | length acc >= n = reverse acc
        | otherwise =
            let target = complexAdd (roots !! pos) step
                unused = [i | i <- [0 .. n - 1], not (elem i used)]
            in case unused of
                 [] -> reverse acc
                 _  -> let closest = minimumByDist roots target unused
                        in go (closest : used) closest (closest : acc)
  in go [start, next] next [next, start]

--- Score a candidate ordering by measuring how close the DFT
--- coefficients' orbit symmetric functions are to rationals.
scoreOrdering :: [(Float, Float)] -> [Int] -> Int -> TransitiveGroup
              -> Float
scoreOrdering roots ordering n tg =
  let ordered = [roots !! i | i <- ordering]
      rjPows = [complexPow (lagrangeResolvent ordered n j) n | j <- [0 .. n - 1]]
      dVals = [dftCoeff rjPows n s | s <- [0 .. n - 1]]
      groupOrder = tgOrder tg
      d = groupOrder `div` n
  in if d == 1
     then -- Cyclic: all d_s should be rational
       sumFloat [scoreRational dv | dv <- dVals]
     else if d == 2
     then -- Dihedral: d_0 rational, conjugate pairs
       scoreRational (headOf dVals)
       + sumFloat [scoreRational (complexAdd (dVals !! s) (dVals !! (n - s)))
                   + scoreRational (complexMul (dVals !! s) (dVals !! (n - s)))
                  | s <- [1 .. n `div` 2]]
     else -- General: simplified - check all d_s for rationality
       sumFloat [scoreRational dv | dv <- dVals]

--- How close is a complex number to a rational?
scoreRational :: (Float, Float) -> Float
scoreRational (re, im) = absF im + fracPart re

fracPart :: Float -> Float
fracPart x = absF (x - Prelude.fromInt (roundF x))

------------------------------------------------------------------------
-- DFT coefficient matching
------------------------------------------------------------------------

--- Match DFT coefficients for a general solvable group at prime degree.
matchDsGeneral :: TransitiveGroup -> [(Float, Float)] -> Int
               -> Maybe [RadExpr Rational]
matchDsGeneral tg dVals n =
  let d = tgOrder tg `div` n
  in if d == 1
     then matchDsAllRational dVals
     else if d == 2
     then matchDsDihedral dVals n
     else matchDsViaOrbits tg dVals n

--- All d_s must be rational (cyclic Galois group).
matchDsAllRational :: [(Float, Float)] -> Maybe [RadExpr Rational]
matchDsAllRational = mapMaybe matchRatC

--- Dihedral group: d_0 rational, conjugate pairs {d_s, d_{n-s}}.
matchDsDihedral :: [(Float, Float)] -> Int -> Maybe [RadExpr Rational]
matchDsDihedral dVals n =
  case matchRatC (headOf dVals) of
    Nothing -> Nothing
    Just d0Expr ->
      let halfN = (n - 1) `div` 2
          pairs = mapMaybe2 (\s -> matchConjPair (dVals !! s) (dVals !! (n - s)))
                            [1 .. halfN]
      in case pairs of
           Nothing -> Nothing
           Just pairExprs ->
             let result = replicate n (Lit rZero)
                 rPairs = concatMap (\(s, (eS, eNS)) ->
                            [(s, eS), (n - s, eNS)])
                          (zip [1..] pairExprs)
                 filled = fillIn (d0Expr : tail result) rPairs
             in Just filled

--- Match via orbit structure for larger stabiliser groups.
--- Tries rational first, then Q(ω_n) decomposition.
matchDsViaOrbits :: TransitiveGroup -> [(Float, Float)] -> Int
                 -> Maybe [RadExpr Rational]
matchDsViaOrbits _tg dVals n =
  case matchDsAllRational dVals of
    Just exprs -> Just exprs
    Nothing ->
      case matchRatC (headOf dVals) of
        Nothing -> Nothing
        Just d0Expr ->
          case mapMaybe (matchQOmegaN n) (tail dVals) of
            Nothing -> Nothing
            Just restExprs -> Just (d0Expr : restExprs)

--- Match a complex value to a rational.
matchRatC :: (Float, Float) -> Maybe (RadExpr Rational)
matchRatC (re, im)
  | absF im > 0.1 = Nothing
  | otherwise = Just (Lit (approxRat re))

--- Match a conjugate pair {d_s, d_{n-s}} to quadratic expressions.
matchConjPair :: (Float, Float) -> (Float, Float)
              -> Maybe (RadExpr Rational, RadExpr Rational)
matchConjPair d1 d2 =
  let s = complexAdd d1 d2
      p = complexMul d1 d2
  in case (matchRatC s, matchRatC p) of
       (Just sExpr, Just pExpr) ->
         let sR = approxRat (fst s)
             pR = approxRat (fst p)
             discR = ratSub (ratMul sR sR) (ratMul (Rational.fromInt 4) pR)
             discExpr = Add (Mul sExpr sExpr) (Neg (Mul (Lit (Rational.fromInt 4)) pExpr))
             sqrtDisc = Root 2 discExpr
             ePlus  = Mul (Inv (Lit (Rational.fromInt 2))) (Add sExpr sqrtDisc)
             eMinus = Mul (Inv (Lit (Rational.fromInt 2))) (Add sExpr (Neg sqrtDisc))
             -- Determine which branch matches d1
             sqrtDiscVal =
               if ratGe discR rZero
               then (sqrt (ratToFloat discR), 0.0)
               else (0.0, sqrt (ratToFloat (ratNeg discR)))
             d1PlusVal = complexScale 0.5 (complexAdd s sqrtDiscVal)
         in if complexDist d1PlusVal d1 < complexDist d1PlusVal d2
            then Just (ePlus, eMinus)
            else Just (eMinus, ePlus)
       _ -> Nothing

--- Match a complex number to an element of Q(ω_n).
matchQOmegaN :: Int -> (Float, Float) -> Maybe (RadExpr Rational)
matchQOmegaN n d =
  -- Try single-term: d ≈ r · ω_n^k
  let candidates =
        [ (k, complexMul d (omegaNC n (negate k)),
           scoreRational (complexMul d (omegaNC n (negate k))))
        | k <- [0 .. n - 1]
        ]
      (bestK, bestV, bestScore) = minimumByThird candidates
  in if bestScore < 0.01
     then let r = approxRat (fst bestV)
              omExpr = omegaNExpr n
          in Just (if bestK == 0
                   then Lit r
                   else Mul (Lit r) (omegaPowNExpr n omExpr bestK))
     else matchGeneralQOmegaN n d

--- General Q(ω_n) decomposition: try 2-term decompositions.
matchGeneralQOmegaN :: Int -> (Float, Float) -> Maybe (RadExpr Rational)
matchGeneralQOmegaN n d =
  let omExpr = omegaNExpr n
      twoTermCandidates =
        [ (err, j, aR, k, bR)
        | j <- [0 .. n - 1]
        , k <- [j + 1 .. n - 1]
        , let wj = omegaNC n j
              wk = omegaNC n k
              det = fst wj * snd wk - snd wj * fst wk
        , absF det > 1.0e-10
        , let a = (fst d * snd wk - snd d * fst wk) / det
              b = (fst wj * snd d - snd wj * fst d) / det
              aR = approxRat a
              bR = approxRat b
              recon = complexAdd (complexScale (ratToFloat aR) wj)
                                 (complexScale (ratToFloat bR) wk)
              err = complexMag (complexSub recon d)
        , err < 0.01
        ]
  in case sortByFirst twoTermCandidates of
       ((_, j, aR, k, bR) : _) ->
         let termJ = if j == 0 then Lit aR
                     else Mul (Lit aR) (omegaPowNExpr n omExpr j)
             termK = if k == 0 then Lit bR
                     else Mul (Lit bR) (omegaPowNExpr n omExpr k)
         in Just (Add termJ termK)
       [] -> Nothing

------------------------------------------------------------------------
-- ω_n expressions
------------------------------------------------------------------------

--- ω_n = e^{2πi/n} as a radical expression.
--- For n=5, uses explicit cos/sin. For other n, tries allPeriodsViaGauss,
--- falling back to cos + i·sin with rational approximations.
omegaNExpr :: Int -> RadExpr Rational
omegaNExpr 5 = omega5Expr
omegaNExpr n =
  case allPeriodsViaGauss n of
    Just periods ->
      case lookupAL 1 periods of
        Just zeta -> zeta
        Nothing   -> fallbackOmega n
    Nothing -> fallbackOmega n

--- ω₅ = cos(2π/5) + i·sin(2π/5) as a radical expression.
omega5Expr :: RadExpr Rational
omega5Expr =
  let cos5 = Mul (Inv (Lit (Rational.fromInt 4)))
                 (Add (Lit (Rational.fromInt (negate 1)))
                      (Root 2 (Lit (Rational.fromInt 5))))
      sin5 = Mul (Inv (Lit (Rational.fromInt 4)))
                 (Root 2 (Add (Lit (Rational.fromInt 10))
                              (Mul (Lit (Rational.fromInt 2))
                                   (Root 2 (Lit (Rational.fromInt 5))))))
      i = Root 2 (Lit (Rational.fromInt (negate 1)))
  in Add cos5 (Mul i sin5)

--- Fallback: build ω_n from numerical approximation.
fallbackOmega :: Int -> RadExpr Rational
fallbackOmega n =
  let theta = 2.0 * pi / Prelude.fromInt n
      cosVal = cos theta
      sinVal = sin theta
      i = Root 2 (Lit (Rational.fromInt (negate 1)))
  in Add (Lit (approxRat cosVal)) (Mul i (Lit (approxRat sinVal)))

--- ω_n^k as a radical expression.
omegaPowNExpr :: Int -> RadExpr Rational -> Int -> RadExpr Rational
omegaPowNExpr _ _ 0 = Lit rOne
omegaPowNExpr _ omExpr 1 = omExpr
omegaPowNExpr n omExpr k =
  let k' = k `mod` n
  in if k' == 0 then Lit rOne
     else if k' == 1 then omExpr
     else Pow omExpr k'

--- ω_n^k as a complex Float pair (numerical).
omegaNC :: Int -> Int -> (Float, Float)
omegaNC n k =
  let theta = 2.0 * pi * Prelude.fromInt k / Prelude.fromInt n
  in (cos theta, sin theta)

------------------------------------------------------------------------
-- Branch selection
------------------------------------------------------------------------

--- Select the correct branch of the n-th root of R_j^n.
--- Tries all n candidates ω_n^k · ⁿ√(R_j^n) and picks the one
--- closest to the known numerical value.
selectBranchN :: Int -> RadExpr Rational -> RadExpr Rational
              -> (Float, Float) -> RadExpr Rational
selectBranchN n omExpr rjnExpr targetVal =
  let rjnVal = evalComplex rjnExpr
      mag = complexMag rjnVal
      ph = floatAtan2 (snd rjnVal) (fst rjnVal)
      principalMag = mag ** (1.0 / Prelude.fromInt n)
      principalPh = ph / Prelude.fromInt n
      principalVal = (principalMag * cos principalPh,
                      principalMag * sin principalPh)
      principalRoot = Root n rjnExpr
      scored = [ (k, complexDist (complexMul (omegaNC n k) principalVal) targetVal)
               | k <- [0 .. n - 1]
               ]
      bestK = fst (minimumBySnd scored)
  in if bestK == 0
     then principalRoot
     else Mul (omegaPowNExpr n omExpr bestK) principalRoot

------------------------------------------------------------------------
-- Lagrange resolvent & DFT
------------------------------------------------------------------------

--- R_j = Σ_k ω_n^{jk} α_k
lagrangeResolvent :: [(Float, Float)] -> Int -> Int -> (Float, Float)
lagrangeResolvent roots n j =
  foldl complexAdd (0.0, 0.0)
    [complexMul (omegaNC n (j * k)) (roots !! k) | k <- [0 .. n - 1]]

--- d_s = (1/n) Σ_j ω_n^{-js} R_j^n
dftCoeff :: [(Float, Float)] -> Int -> Int -> (Float, Float)
dftCoeff rjPows n s =
  complexScale (1.0 / Prelude.fromInt n)
    (foldl complexAdd (0.0, 0.0)
      [complexMul (omegaNC n (n - (j * s) `mod` n)) (rjPows !! j)
      | j <- [0 .. n - 1]])

------------------------------------------------------------------------
-- Match to original root ordering
------------------------------------------------------------------------

--- Match radical expressions to original numerical roots by proximity.
matchToOriginal :: [RadExpr Rational] -> [(Float, Float)]
                -> [RadExpr Rational]
matchToOriginal exprs numRoots =
  let exprVals = [(e, evalComplex e) | e <- exprs]
  in [ fst (minimumBySndC [(e, complexDist v t) | (e, v) <- exprVals])
     | t <- numRoots
     ]

------------------------------------------------------------------------
-- Complex arithmetic helpers
------------------------------------------------------------------------

complexAdd :: (Float, Float) -> (Float, Float) -> (Float, Float)
complexAdd (a, b) (c, d) = (a + c, b + d)

complexSub :: (Float, Float) -> (Float, Float) -> (Float, Float)
complexSub (a, b) (c, d) = (a - c, b - d)

complexMul :: (Float, Float) -> (Float, Float) -> (Float, Float)
complexMul (a, b) (c, d) = (a * c - b * d, a * d + b * c)

complexPow :: (Float, Float) -> Int -> (Float, Float)
complexPow z n
  | n == 0    = (1.0, 0.0)
  | n == 1    = z
  | even n    = let half = complexPow z (n `div` 2)
                    (hr, hi) = half
                in (hr * hr - hi * hi, 2.0 * hr * hi)
  | otherwise = let (zr, zi) = z
                    (rr, ri) = complexPow z (n - 1)
                in (zr * rr - zi * ri, zr * ri + zi * rr)

complexScale :: Float -> (Float, Float) -> (Float, Float)
complexScale s (r, i) = (s * r, s * i)

complexMag :: (Float, Float) -> Float
complexMag (r, i) = sqrt (r * r + i * i)

complexDist :: (Float, Float) -> (Float, Float) -> Float
complexDist a b = complexMag (complexSub a b)

ratToComplex :: Rational -> (Float, Float)
ratToComplex r = (ratToFloat r, 0.0)

ratToFloat :: Rational -> Float
ratToFloat r = Prelude.fromInt (numerator r) / Prelude.fromInt (denominator r)

floatAtan2 :: Float -> Float -> Float
floatAtan2 y x
  | x > 0.0             = atan (y / x)
  | x < 0.0 && y >= 0.0 = atan (y / x) + pi
  | x < 0.0 && y < 0.0  = atan (y / x) - pi
  | x == 0.0 && y > 0.0 = pi / 2.0
  | x == 0.0 && y < 0.0 = negate (pi / 2.0)
  | otherwise            = 0.0

------------------------------------------------------------------------
-- Rational approximation
------------------------------------------------------------------------

--- Approximate a Float as a small-denominator rational.
approxRat :: Float -> Rational
approxRat x =
  let candidates =
        [ (absF (Prelude.fromInt n / Prelude.fromInt d - x), mkRat n d)
        | d <- [1 .. 10000]
        , let n = roundF (x * Prelude.fromInt d)
        , absF (Prelude.fromInt n / Prelude.fromInt d - x) < 1.0e-6
        ]
  in case candidates of
       [] -> Rational.fromInt (roundF x)
       _  -> snd (minimumByFst candidates)

------------------------------------------------------------------------
-- Generic helpers
------------------------------------------------------------------------

--- Extract coefficients from a Poly.
polyCoeffsRT :: Poly -> [Rational]
polyCoeffsRT (Poly cs) = cs

absF :: Float -> Float
absF x = if x < 0.0 then negate x else x

roundF :: Float -> Int
roundF x = if x >= 0.0 then truncate (x + 0.5) else negate (truncate (negate x + 0.5))

sumFloat :: [Float] -> Float
sumFloat = foldl (+) 0.0

headOf :: [a] -> a
headOf (x:_) = x
headOf [] = error "headOf: empty"

lastOfR :: [Rational] -> Rational
lastOfR [x] = x
lastOfR (_:xs) = lastOfR xs
lastOfR [] = error "lastOfR: empty"

--- foldl1 for RadExpr.
foldl1R :: (RadExpr Rational -> RadExpr Rational -> RadExpr Rational)
        -> [RadExpr Rational] -> RadExpr Rational
foldl1R _ [x] = x
foldl1R f (x:xs) = foldl f x xs
foldl1R _ [] = error "foldl1R: empty"

--- All permutations of a list.
permsOf :: [a] -> [[a]]
permsOf [] = [[]]
permsOf xs = [y : rest | (y, ys) <- picks xs, rest <- permsOf ys]
  where
    picks [] = []
    picks (z:zs) = (z, zs) : [(w, z:ws) | (w, ws) <- picks zs]

--- Sort (ordering, score) pairs by score.
sortByScore :: [([Int], Float)] -> [([Int], Float)]
sortByScore [] = []
sortByScore ((o,s):rest) =
  sortByScore [(o2,s2) | (o2,s2) <- rest, s2 <= s]
  ++ [(o,s)]
  ++ sortByScore [(o2,s2) | (o2,s2) <- rest, s2 > s]

--- Sort 5-tuples by first element.
sortByFirst :: [(Float, Int, Rational, Int, Rational)]
            -> [(Float, Int, Rational, Int, Rational)]
sortByFirst [] = []
sortByFirst (x:xs) =
  let (e,_,_,_,_) = x
  in sortByFirst [(y :: (Float, Int, Rational, Int, Rational))
                 | y@(e2,_,_,_,_) <- xs, e2 <= e]
     ++ [x]
     ++ sortByFirst [(y :: (Float, Int, Rational, Int, Rational))
                    | y@(e2,_,_,_,_) <- xs, e2 > e]

--- Minimum by second element of a pair.
minimumBySnd :: [(Int, Float)] -> (Int, Float)
minimumBySnd [x] = x
minimumBySnd (x:y:rest) =
  if snd x <= snd y then minimumBySnd (x:rest) else minimumBySnd (y:rest)
minimumBySnd [] = error "minimumBySnd: empty"

--- Minimum by second element for (RadExpr, Float).
minimumBySndC :: [(RadExpr Rational, Float)] -> (RadExpr Rational, Float)
minimumBySndC [x] = x
minimumBySndC (x:y:rest) =
  if snd x <= snd y then minimumBySndC (x:rest) else minimumBySndC (y:rest)
minimumBySndC [] = error "minimumBySndC: empty"

--- Minimum by third element.
minimumByThird :: [(Int, (Float,Float), Float)]
               -> (Int, (Float,Float), Float)
minimumByThird [x] = x
minimumByThird (x:y:rest) =
  let (_,_,sx) = x; (_,_,sy) = y
  in if sx <= sy then minimumByThird (x:rest) else minimumByThird (y:rest)
minimumByThird [] = error "minimumByThird: empty"

--- Minimum by first element.
minimumByFst :: [(Float, Rational)] -> (Float, Rational)
minimumByFst [x] = x
minimumByFst (x:y:rest) =
  if fst x <= fst y then minimumByFst (x:rest) else minimumByFst (y:rest)
minimumByFst [] = error "minimumByFst: empty"

--- Find closest root by distance.
minimumByDist :: [(Float,Float)] -> (Float,Float) -> [Int] -> Int
minimumByDist roots target (i:is) =
  foldl (\best j ->
    if complexDist (roots !! j) target < complexDist (roots !! best) target
    then j else best) i is
minimumByDist _ _ [] = error "minimumByDist: empty"

--- mapM for Maybe (monomorphic for RadExpr).
mapMaybe :: (a -> Maybe (RadExpr Rational)) -> [a]
         -> Maybe [RadExpr Rational]
mapMaybe _ [] = Just []
mapMaybe f (x:xs) = case f x of
  Nothing -> Nothing
  Just y  -> case mapMaybe f xs of
               Nothing -> Nothing
               Just ys -> Just (y : ys)

--- mapM for Maybe pairs.
mapMaybe2 :: (Int -> Maybe (RadExpr Rational, RadExpr Rational)) -> [Int]
          -> Maybe [(RadExpr Rational, RadExpr Rational)]
mapMaybe2 _ [] = Just []
mapMaybe2 f (x:xs) = case f x of
  Nothing -> Nothing
  Just y  -> case mapMaybe2 f xs of
               Nothing -> Nothing
               Just ys -> Just (y : ys)

--- Fill in values at given indices.
fillIn :: [a] -> [(Int, a)] -> [a]
fillIn arr [] = arr
fillIn arr ((idx, expr) : rest) =
  fillIn (take idx arr ++ [expr] ++ drop (idx + 1) arr) rest

--- Lookup in an association list.
lookupAL :: Eq a => a -> [(a, b)] -> Maybe b
lookupAL _ [] = Nothing
lookupAL k ((k', v) : rest) = if k == k' then Just v else lookupAL k rest

--- Show.
showRadicalTower :: [RadExpr Rational] -> String
showRadicalTower exprs =
  unlines (zipWith (\i e -> "root_" ++ show i ++ " = "
                            ++ showRadExpr showRat e)
                   [(1::Int)..] exprs)
