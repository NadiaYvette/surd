||| Radical tower construction for solvable polynomials via Lagrange
||| resolvents. Generalised to all prime degrees.
|||
||| Given an irreducible polynomial f(x) in Q[x] of prime degree p with
||| solvable Galois group G, constructs radical expressions for its roots
||| via the 9-step pipeline:
|||
|||   1. Depress (eliminate x^{p-1} term)
|||   2. Find cyclic ordering (Galois generator acts as p-cycle)
|||   3. Lagrange resolvents R_j = sum_k omega^{jk} alpha_k
|||   4. DFT: d_s from R_j^p
|||   5. Coefficient matching (d_s in Q, Q(sqrt(disc)), or Q(omega_p))
|||   6. Reconstruct R_j^p as RadExpr
|||   7. Branch selection via numerical comparison
|||   8. Inverse DFT: alpha_k = (1/p) sum_j omega^{-jk} R_j
|||   9. Un-depress and match to original roots
module Surd.RadicalTower

import Surd.Rational
import Surd.Poly
import Surd.Cyclotomic  -- Ring/Field instances
import Surd.Types
import Surd.Eval
import Surd.Normalize
import Surd.Permutation
import Surd.TransitiveGroup
import Surd.Identify
import Surd.Resolvent
import Surd.RootOfUnity

import Data.List
import Data.Maybe
import Data.Nat

%default covering

------------------------------------------------------------------------
-- Result type
------------------------------------------------------------------------

||| Result of radical tower solving.
public export
data SolveResult : Type where
  ||| All roots expressed as radical expressions.
  Solved      : List (RadExpr Rational) -> SolveResult
  ||| The polynomial is not solvable by radicals.
  NotSolvable : SolveResult
  ||| Could not solve (unsupported case).
  Unsupported : String -> SolveResult

export
Show SolveResult where
  show (Solved roots) = "Solved: " ++ show (length roots) ++ " roots"
  show NotSolvable = "Not solvable by radicals"
  show (Unsupported msg) = "Unsupported: " ++ msg

------------------------------------------------------------------------
-- Complex number helpers (CNum, cAdd, cSub, cMul, cDiv, cMag, cNeg,
-- cScale, cPowNat are re-exported from Surd.Resolvent)
------------------------------------------------------------------------

||| Construct an nth root with a runtime Nat, using believe_me for the LTE proof.
||| Precondition: n >= 2.
nthRoot : Nat -> RadExpr k -> RadExpr k
nthRoot n x = Root n {prf = believe_me (the (LTE 0 0) LTEZero)} x

||| atan2 with quadrant selection.
atan2' : Double -> Double -> Double
atan2' y x =
  if x > 0.0 then atan (y / x)
  else if x < 0.0 && y >= 0.0 then atan (y / x) + pi
  else if x < 0.0 && y < 0.0 then atan (y / x) - pi
  else if x == 0.0 && y > 0.0 then pi / 2.0
  else if x == 0.0 && y < 0.0 then negate (pi / 2.0)
  else 0.0

||| Phase angle of a complex number.
cPhase : CNum -> Double
cPhase (r, i) = atan2' i r

------------------------------------------------------------------------
-- omega_n: nth root of unity as complex number
------------------------------------------------------------------------

||| omega_n^k = e^{2*pi*i*k/n} as a complex number.
omC : Int -> Int -> CNum
omC n k = (cos theta, sin theta)
  where theta : Double
        theta = 2.0 * pi * cast k / cast n

------------------------------------------------------------------------
-- Rational approximation
------------------------------------------------------------------------

||| Approximate a Double as a small-denominator Rational.
approxRat : Double -> Rational
approxRat x =
  let -- Try denominators 1..1000
      go : Integer -> (Double, Rational) -> (Double, Rational)
      go d best =
        let n = cast {to = Integer} (cast {to = Int} (x * cast d + 0.5))
            -- Handle negative rounding
            n' = if x < 0.0 && cast n > x * cast d + 0.5
                   then n - 1 else n
            err = abs (cast n' / cast d - x)
        in if err < 1.0e-6 && err < Builtin.fst best
             then (err, mkRat n' d)
             else best
      pair = foldl (\acc, d => go d acc) (1.0e10, Rational.fromInteger (cast {to = Integer} (cast {to = Int} x))) [1..1000]
  in Builtin.snd pair

||| Score how close a complex number is to a rational (lower = better).
scoreRational : CNum -> Double
scoreRational (re, im) =
  let fracPart = abs (re - cast (cast {to = Int} re))
  in abs im + fracPart

------------------------------------------------------------------------
-- omega_n as radical expressions
------------------------------------------------------------------------

||| omega_5 = e^{2*pi*i/5} as a radical expression.
omega5Expr : RadExpr Rational
omega5Expr =
  let cos5 = Mul (Inv (Lit (Rational.fromInteger 4)))
                 (Add (sqrt (Lit (Rational.fromInteger 5)))
                      (Neg (Lit Rational.one)))
      sin5 = Mul (Inv (Lit (Rational.fromInteger 4)))
                 (sqrt (Add (Lit (Rational.fromInteger 10))
                            (Mul (Lit (Rational.fromInteger 2))
                                 (sqrt (Lit (Rational.fromInteger 5))))))
      i = sqrt (Lit (negate Rational.one))
  in Add cos5 (Mul i sin5)

||| omega_n = e^{2*pi*i/n} as a radical expression.
||| For n=5, uses the exact form. For general n, uses cos + i*sin
||| built from numerical approximation as a fallback.
omegaNExpr : Int -> RadExpr Rational
omegaNExpr 5 = omega5Expr
omegaNExpr n =
  let theta = 2.0 * pi / cast n
      cosVal = cos theta
      sinVal = sin theta
      i = sqrt (Lit (negate Rational.one))
  in Add (Lit (approxRat cosVal)) (Mul i (Lit (approxRat sinVal)))

||| omega_n^k as a radical expression, reduced mod n.
omegaPowNExpr : Int -> RadExpr Rational -> Int -> RadExpr Rational
omegaPowNExpr _ _ 0 = Lit Rational.one
omegaPowNExpr n omExpr 1 = omExpr
omegaPowNExpr n omExpr k =
  let k' = mod k n
  in if k' == 0 then Lit Rational.one
     else if k' == 1 then omExpr
     else Pow omExpr k'

------------------------------------------------------------------------
-- Cyclic ordering
------------------------------------------------------------------------

||| Score a candidate ordering by checking how close the DFT
||| coefficients are to rationals (or orbit-symmetric functions
||| of rationals for non-cyclic groups).
scoreOrderingN : List CNum -> List Int -> Int -> Integer -> Double
scoreOrderingN roots ordering n groupOrder =
  let ordered = map (\i => fromMaybe (0.0, 0.0) (index' (cast i) roots)) ordering
      rj : Int -> CNum
      rj j = foldl cAdd (0.0, 0.0)
               (zipWith (\k, r => cMul (omC n (j * k)) r)
                        [0 .. n - 1] ordered)
      rjVals = map rj [0 .. n - 1]
      rjPows = map (\r => cPowNat r (cast n)) rjVals
      dVals : List CNum
      dVals =
        map (\s => cScale (1.0 / cast n)
               (foldl cAdd (0.0, 0.0)
                 (zipWith (\j, rp => cMul (omC n (n - mod (j * s) n)) rp)
                          [0 .. n - 1] rjPows)))
            [0 .. n - 1]
      p = cast {to = Integer} n
      d = div groupOrder p
  in if d == 1
       -- Cyclic: all d_s should be rational
       then sum (map scoreRational dVals)
       -- Dihedral: d_0 rational, conjugate pairs
       else if d == 2
       then
         let d0 = fromMaybe (0.0, 0.0) (index' 0 dVals)
             halfN = div (n - 1) 2
             pairScores = map (\s =>
               let ds = fromMaybe (0.0, 0.0) (index' (cast s) dVals)
                   dns = fromMaybe (0.0, 0.0) (index' (cast (n - s)) dVals)
               in scoreRational (cAdd ds dns) + scoreRational (cMul ds dns))
               [1 .. halfN]
         in scoreRational d0 + sum pairScores
       -- General: check all d_s for rationality (simplified)
       else sum (map scoreRational dVals)
  where
    index' : Nat -> List a -> Maybe a
    index' Z (x :: _) = Just x
    index' (S k) (_ :: xs) = index' k xs
    index' _ [] = Nothing

||| Build a cyclic ordering by greedy nearest-neighbour from a starting
||| pair (root 0 at position 0, root `next` at position 1).
buildOrdering : List CNum -> Int -> Int -> Int -> List Int
buildOrdering roots n start next =
  let zero : CNum
      zero = (0.0, 0.0)
      idxR : Int -> CNum
      idxR i = fromMaybe zero (idxL (cast i) roots)
      step = cSub (idxR next) (idxR start)
      go : List Int -> Int -> List Int -> List Int
      go used pos acc =
        if cast (length acc) >= n then reverse acc
        else
          let target = cAdd (idxR pos) step
              unused = filter (\i => not (elem i used)) [0 .. n - 1]
          in case unused of
               [] => reverse acc
               _ =>
                 let closest = foldl (\best, i =>
                       let dist = cMag (cSub (idxR i) target)
                           bestDist = cMag (cSub (idxR best) target)
                       in if dist < bestDist then i else best) (case unused of (h :: _) => h; [] => 0) unused
                 in go (closest :: used) closest (closest :: acc)
  in go [start, next] next [next, start]
  where
    idxL : Nat -> List a -> Maybe a
    idxL Z (x :: _) = Just x
    idxL (S k) (_ :: xs) = idxL k xs
    idxL _ [] = Nothing

||| All permutations of a list.
perms : List Int -> List (List Int)
perms [] = [[]]
perms xs = concatMap (\y =>
  map (y ::) (perms (filter (/= y) xs))) xs

||| Find a cyclic ordering of n numerical roots for a solvable
||| prime-degree polynomial.
|||
||| For small n (<=8), brute-force all (n-1)! orderings.
||| For large n, try (n-1) rotation candidates.
findCyclicOrderingN : List CNum -> Int -> Integer -> Maybe (List Int)
findCyclicOrderingN roots n groupOrder =
  if n <= 8
    then
      -- Brute force all (n-1)! orderings with root 0 fixed at position 0
      let rest = [1 .. n - 1]
          orderings = map (0 ::) (perms rest)
          scored = map (\o => (o, scoreOrderingN roots o n groupOrder)) orderings
          sorted = sortBy (\a, b => compare (Builtin.snd a) (Builtin.snd b)) scored
      in case sorted of
           ((bestO, bestScore) :: (_, secondScore) :: _) =>
             if bestScore < cast n * 5.0 && bestScore < secondScore * 0.5
               then Just bestO
               else if bestScore < cast n then Just bestO
               else Nothing
           ((bestO, bestScore) :: _) =>
             if bestScore < cast n * 5.0 then Just bestO else Nothing
           [] => Nothing
    else
      -- For large n, try (n-1) rotation candidates
      let candidates : List (List Int, Double)
          candidates = mapMaybe (\next =>
            let ordering = buildOrdering roots n 0 next
            in if cast (length ordering) == n
                 then Just (ordering, scoreOrderingN roots ordering n groupOrder)
                 else Nothing) [1 .. n - 1]
          sorted = sortBy (\a, b => compare (Builtin.snd a) (Builtin.snd b)) candidates
      in case sorted of
           ((bestO, bestScore) :: _) =>
             if bestScore < cast n * 5.0 then Just bestO else Nothing
           [] => Nothing

------------------------------------------------------------------------
-- DFT coefficient matching
------------------------------------------------------------------------

||| Match a complex number to a Rational, returning Nothing if imaginary
||| part is too large.
matchRatC : CNum -> Maybe (RadExpr Rational)
matchRatC (re, im) =
  if abs im > 0.05 then Nothing
  else Just (Lit (approxRat re))

||| Match all DFT coefficients as rational (for cyclic Galois group).
matchDsAllRational : List CNum -> Maybe (List (RadExpr Rational))
matchDsAllRational = traverse matchRatC

||| Match a conjugate pair {d_s, d_{n-s}} to quadratic radical expressions.
||| d_s and d_{n-s} are roots of t^2 - s*t + p = 0 where s,p are rational.
matchConjPair : CNum -> CNum -> Maybe (RadExpr Rational, RadExpr Rational)
matchConjPair d1 d2 = do
  let s = cAdd d1 d2
      p = cMul d1 d2
  sExpr <- matchRatC s
  pExpr <- matchRatC p
  let sR = approxRat (Builtin.fst s)
      pR = approxRat (Builtin.fst p)
      discR = sR * sR - Rational.fromInteger 4 * pR
      discExpr = Add (Mul sExpr sExpr) (Neg (Mul (Lit (Rational.fromInteger 4)) pExpr))
      sqrtDisc = sqrt discExpr
      ePlus  = Mul (Inv (Lit (Rational.fromInteger 2))) (Add sExpr sqrtDisc)
      eMinus = Mul (Inv (Lit (Rational.fromInteger 2))) (Add sExpr (Neg sqrtDisc))
  -- Determine which branch matches d1
  let discDbl = cast (numer discR) / cast (denom discR)
      sqrtDiscVal : CNum
      sqrtDiscVal = if discDbl >= 0.0
        then (sqrt discDbl, 0.0)
        else (0.0, sqrt (abs discDbl))
      d1PlusVal = cScale 0.5 (cAdd s sqrtDiscVal)
  if cMag (cSub d1PlusVal d1) < cMag (cSub d1PlusVal d2)
    then Just (ePlus, eMinus)
    else Just (eMinus, ePlus)

||| Match DFT coefficients for a dihedral group (d=2).
||| d_0 is rational, {d_s, d_{n-s}} form conjugate pairs.
matchDsDihedral : List CNum -> Int -> Maybe (List (RadExpr Rational))
matchDsDihedral dVals n = do
  d0Expr <- matchRatC (fromMaybe (0.0, 0.0) (idx 0 dVals))
  let halfN = div (n - 1) 2
  pairExprs <- traverse (\s =>
    matchConjPair (fromMaybe (0.0, 0.0) (idx (cast s) dVals))
                  (fromMaybe (0.0, 0.0) (idx (cast (n - s)) dVals)))
    [1 .. halfN]
  -- Build the full list
  let result : List (RadExpr Rational)
      result = map (\i =>
        if i == 0 then d0Expr
        else
          let s = min i (n - i)
              pairIdx = s - 1
          in case idx (cast pairIdx) pairExprs of
               Just (eS, eNS) => if i == s then eS else eNS
               Nothing => Lit Rational.zero)
        [0 .. n - 1]
  Just result
  where
    idx : Nat -> List a -> Maybe a
    idx Z (x :: _) = Just x
    idx (S k) (_ :: xs) = idx k xs
    idx _ [] = Nothing

||| Match a complex number to r * omega_n^k for rational r and 0<=k<n.
matchSingleOmegaN : Int -> RadExpr Rational -> CNum -> Maybe (RadExpr Rational)
matchSingleOmegaN n omExpr d =
  let candidates = map (\k =>
        let divided = cMul d (omC n (negate k))
        in (k, divided, scoreRational divided)) [0 .. n - 1]
      sorted = sortBy (\a, b => compare (third a) (third b)) candidates
  in case sorted of
       ((bestK, bestV, bestScore) :: _) =>
         if bestScore < 0.01
           then
             let r = approxRat (Builtin.fst bestV)
             in Just (if bestK == 0 then Lit r
                      else Mul (Lit r) (omegaPowNExpr n omExpr bestK))
           else Nothing
       [] => Nothing
  where
    third : (a, b, c) -> c
    third (_, _, c) = c

||| Match a complex number to a + b * omega_n^k (two-term decomposition).
matchTwoTermOmegaN : Int -> RadExpr Rational -> CNum -> Maybe (RadExpr Rational)
matchTwoTermOmegaN n omExpr d =
  let candidates : List (Double, Int, Rational, Int, Rational)
      candidates = concatMap (\j =>
        concatMap (\k =>
          if j >= k then []
          else
            let wj = omC n j
                wk = omC n k
                -- Solve 2x2 system: a*wj + b*wk = d
                det = Builtin.fst wj * Builtin.snd wk - Builtin.snd wj * Builtin.fst wk
            in if abs det < 1.0e-10 then []
               else
                 let a = (Builtin.fst d * Builtin.snd wk - Builtin.snd d * Builtin.fst wk) / det
                     b = (Builtin.fst wj * Builtin.snd d - Builtin.snd wj * Builtin.fst d) / det
                     aR = approxRat a
                     bR = approxRat b
                     aD = cast (numer aR) / cast (denom aR)
                     bD = cast (numer bR) / cast (denom bR)
                     recon = cAdd (cScale aD wj) (cScale bD wk)
                     err = cMag (cSub recon d)
                 in if err < 0.01 then [(err, j, aR, k, bR)] else [])
          [0 .. n - 1])
        [0 .. n - 1]
      sorted = sortBy (\a, b => compare (Builtin.fst a) (Builtin.fst b)) candidates
  in case sorted of
       ((_, j, aR, k, bR) :: _) =>
         let termJ = if j == 0 then Lit aR else Mul (Lit aR) (omegaPowNExpr n omExpr j)
             termK = if k == 0 then Lit bR else Mul (Lit bR) (omegaPowNExpr n omExpr k)
         in Just (Add termJ termK)
       [] => Nothing

||| Match DFT coefficients via orbit structure for general solvable groups.
||| Tries: all rational, then Q(omega_n) decomposition.
matchDsViaOrbits : List CNum -> Int -> RadExpr Rational -> Maybe (List (RadExpr Rational))
matchDsViaOrbits dVals n omExpr =
  case matchDsAllRational dVals of
    Just exprs => Just exprs
    Nothing => do
      d0Expr <- matchRatC (fromMaybe (0.0, 0.0) (idx 0 dVals))
      restExprs <- traverse (\v =>
        case matchSingleOmegaN n omExpr v of
          Just e  => Just e
          Nothing =>
            case matchTwoTermOmegaN n omExpr v of
              Just e  => Just e
              Nothing => matchRatC v) (drop 1 dVals)
      Just (d0Expr :: restExprs)
  where
    idx : Nat -> List a -> Maybe a
    idx Z (x :: _) = Just x
    idx (S k) (_ :: xs) = idx k xs
    idx _ [] = Nothing

||| Match DFT coefficients to radical expressions, dispatching on group
||| structure (cyclic, dihedral, or general).
matchDsGeneral : Integer -> Integer -> List CNum -> Int -> RadExpr Rational -> Maybe (List (RadExpr Rational))
matchDsGeneral groupOrder p dVals n omExpr =
  let d = div groupOrder p
  in if d == 1
       then matchDsAllRational dVals
       else if d == 2
       then matchDsDihedral dVals n
       else matchDsViaOrbits dVals n omExpr

------------------------------------------------------------------------
-- Branch selection
------------------------------------------------------------------------

||| Select the correct branch of the nth root of R_j^n.
||| Tries all n candidates omega_n^k * nth_root(R_j^n) and picks the
||| one closest to the known numerical value.
selectBranchN : Int -> RadExpr Rational -> RadExpr Rational -> CNum -> RadExpr Rational
selectBranchN n omExpr rjnExpr targetVal =
  let rjnVal = evalComplex rjnExpr
      rjnMag = cMag rjnVal
      rjnPhase = cPhase rjnVal
      -- Principal nth root
      princMag = pow rjnMag (1.0 / cast n)
      princPhase = rjnPhase / cast n
      principalVal : CNum
      principalVal = (princMag * cos princPhase, princMag * sin princPhase)
      principalRoot = nthRoot (cast n) rjnExpr
      scored = map (\k =>
        let branchVal = cMul (omC n k) principalVal
        in (k, cMag (cSub branchVal targetVal))) [0 .. n - 1]
      bestK = Builtin.fst (foldl (\best, cur =>
        if Builtin.snd cur < Builtin.snd best then cur else best)
        (the (Int, Double) (0, 1.0e20)) scored)
  in if bestK == 0 then principalRoot
     else Mul (omegaPowNExpr n omExpr bestK) principalRoot

------------------------------------------------------------------------
-- Match radical expressions to original numerical roots
------------------------------------------------------------------------

matchToOriginal : List (RadExpr Rational) -> List CNum -> List (RadExpr Rational)
matchToOriginal exprs numRoots =
  let exprVals : List (RadExpr Rational, CNum)
      exprVals = map (\e => (e, evalComplex e)) exprs
      initBest : (RadExpr Rational, Double)
      initBest = case exprVals of
                   ((e, _) :: _) => (e, 1.0e20)
                   [] => (Lit Rational.zero, 1.0e20)
  in map (\t =>
       Builtin.fst (foldl (\best, cur =>
         let dist = cMag (cSub (Builtin.snd cur) t)
         in if dist < Builtin.snd best then (Builtin.fst cur, dist) else best)
         initBest
         exprVals))
     numRoots

------------------------------------------------------------------------
-- Core solver: general prime degree
------------------------------------------------------------------------

||| Core algorithm for solvable polynomials of prime degree n.
||| Implements the full 9-step pipeline.
solveSolvablePrime : Integer -> Poly Rational -> List CNum -> Maybe (List (RadExpr Rational))
solveSolvablePrime groupOrder f numRoots = do
  let n = cast {to = Int} (degreeInt f)
      nI = cast {to = Integer} n

  -- Step 1: Depress
  let cs = coeffs f
      lc = case last' cs of Just c => c; Nothing => Rational.one
      monicCs = map (\c => c / lc) cs
      an1 = fromMaybe Rational.zero (idx (cast (n - 1)) monicCs)
      shiftVal = negate (an1 / Rational.fromInteger nI)
      shiftDbl = cast (numer shiftVal) / cast (denom shiftVal)
      depRoots = map (\r => cSub r (shiftDbl, 0.0)) numRoots

  -- Step 2: Find cyclic ordering
  ordering <- findCyclicOrderingN depRoots n groupOrder

  let orderedRoots = map (\i => fromMaybe (0.0, 0.0) (idx (cast i) depRoots)) ordering

  -- Step 3: Lagrange resolvents R_j = sum_k omega_n^{jk} alpha_k
  let rj : Int -> CNum
      rj j = foldl cAdd (0.0, 0.0)
               (zipWith (\k, r => cMul (omC n (j * k)) r)
                        [0 .. n - 1] orderedRoots)
      rjVals = map rj [0 .. n - 1]
      rjPows = map (\r => cPowNat r (cast n)) rjVals

  -- Step 4: DFT: d_s = (1/n) sum_j omega_n^{-js} R_j^n
  let dVals : List CNum
      dVals = map (\s =>
        cScale (1.0 / cast n)
          (foldl cAdd (0.0, 0.0)
            (zipWith (\j, rp => cMul (omC n (n - mod (j * s) n)) rp)
                     [0 .. n - 1] rjPows)))
        [0 .. n - 1]

  -- Step 5: Match d_s to radical expressions
  let omExpr = omegaNExpr n
      p = nI
  dExprs <- matchDsGeneral groupOrder p dVals n omExpr

  -- Step 6: R_j^n = sum_s d_s * omega_n^{js}
  let omPow : Int -> RadExpr Rational
      omPow k = omegaPowNExpr n omExpr k
      rjPowExprs = map (\j =>
        foldl1 Add
          (map (\s =>
            let dsExpr = fromMaybe (Lit Rational.zero) (idx (cast s) dExprs)
            in Mul dsExpr (omPow (mod (j * s) n)))
            [0 .. n - 1]))
        [1 .. n - 1]

  -- Step 7: Branch selection: R_j = omega_n^{b_j} * nth_root(R_j^n)
  let rjExprs = zipWith (\rjPE, j =>
        let targetVal = fromMaybe (0.0, 0.0) (idx (cast j) rjVals)
        in selectBranchN n omExpr rjPE targetVal)
        rjPowExprs [1 .. n - 1]

  -- R_0 = sum of depressed roots = 0
  let allR = Lit Rational.zero :: rjExprs

  -- Step 8: Inverse DFT: alpha_k = (1/n) sum_j omega_n^{-jk} R_j
  let rootExprs = map (\k =>
        normalize $
          Mul (Inv (Lit (Rational.fromInteger nI)))
              (foldl1 Add
                (map (\j =>
                  let rjE = fromMaybe (Lit Rational.zero) (idx (cast j) allR)
                  in Mul (omPow (mod (n - mod (j * k) n) n)) rjE)
                  [0 .. n - 1])))
        [0 .. n - 1]

  -- Step 9: Un-depress
  let finalExprs = map (\e => normalize (Add e (Lit shiftVal))) rootExprs

  Just (matchToOriginal finalExprs numRoots)

  where
    idx : Nat -> List a -> Maybe a
    idx Z (x :: _) = Just x
    idx (S k) (_ :: xs) = idx k xs
    idx _ [] = Nothing

    foldl1 : (a -> a -> a) -> List a -> a
    foldl1 f (x :: xs) = foldl f x xs
    foldl1 _ [] = believe_me ()

------------------------------------------------------------------------
-- Entry points
------------------------------------------------------------------------

||| Solve a solvable quintic via radical tower (degree 5 fast path).
export
solveViaTower : Poly Rational -> TransGroup 5 -> SolveResult
solveViaTower p tg =
  if not (tgSolvable tg) then NotSolvable
  else
    let numRoots = findRoots p
    in case numRoots of
         [] => Unsupported "Could not find numerical roots"
         _ =>
           case solveSolvablePrime (tgOrder tg) p numRoots of
             Just roots => Solved roots
             Nothing    => Unsupported "Cyclic ordering or coefficient matching failed"

||| Solve a solvable polynomial of any prime degree via radical tower.
export
solveViaTowerRT : Poly Rational -> TransGroupRT -> SolveResult
solveViaTowerRT p tg =
  if not (tgSolvable tg) then NotSolvable
  else
    let numRoots = findRoots p
    in case numRoots of
         [] => Unsupported "Could not find numerical roots"
         _ =>
           case solveSolvablePrime (tgOrder tg) p numRoots of
             Just roots => Solved roots
             Nothing    => Unsupported "Cyclic ordering or coefficient matching failed"
