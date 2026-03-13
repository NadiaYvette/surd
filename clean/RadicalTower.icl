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
import Data.Integer

// Solve a solvable quintic via Lagrange resolvent descent.
solveViaTower :: !GaloisResult !(Poly Rational) -> ?([RadExpr Rational])
solveViaTower gr f
    | not gr.grGroup.tgSolvable = ?None
    # cs = getCoeffsRT f
    # a4 = cs !! 4
    # an = cs !! 5
    # shift = Neg (Mul (Inv (Lit (ratFromInt 5))) (Mul (Lit a4) (Inv (Lit an))))
    # roots = gr.grRoots
    = solveWithRoots roots shift

solveWithRoots :: ![(Real, Real)] !(RadExpr Rational) -> ?([RadExpr Rational])
solveWithRoots roots shift
    | length roots <> 5 = ?None
    # n = 5
    # tau = 2.0 * 3.14159265358979323846 / toReal n
    # resolvents = [computeResolvent roots tau j \\ j <- [0 .. n - 1]]
    # resolventPowers = [cpowR (resolvents !! j) 5 \\ j <- [0 .. n - 1]]
    # r0Expr = Lit (approxRatRT (fst (resolvents !! 0)))
    # rExprs = [buildResolventExpr n (resolventPowers !! j) (resolvents !! j) \\ j <- [1 .. n - 1]]
    # allRes = [r0Expr : rExprs]
    # rootExprs = [buildRootExpr n tau allRes shift k \\ k <- [0 .. n - 1]]
    = ?Just rootExprs

computeResolvent :: ![(Real, Real)] !Real !Int -> (Real, Real)
computeResolvent roots tau j
    = foldl (\(re, im) k ->
        let omk = (cos (tau * toReal (j * k)), sin (tau * toReal (j * k)))
        in caddRT (re, im) (cmulRT omk (roots !! k)))
        (0.0, 0.0) [0 .. length roots - 1]

buildResolventExpr :: !Int !(Real, Real) !(Real, Real) -> RadExpr Rational
buildResolventExpr n rjq _
    # rjqExpr = Lit (approxRatRT (fst rjq))
    # rjqExprFull = if (abs (snd rjq) < 0.00000001) rjqExpr
                    (Add rjqExpr (Mul (Lit (approxRatRT (snd rjq)))
                                      (Root 2 (Lit (ratFromInt (~1))))))
    = Root n rjqExprFull

buildRootExpr :: !Int !Real ![RadExpr Rational] !(RadExpr Rational) !Int -> RadExpr Rational
buildRootExpr n tau allRes shift k
    # terms = [buildTerm n tau allRes j k \\ j <- [0 .. n - 1]]
    # sumE = foldl1Expr Add terms
    = Add (Mul (Inv (Lit (ratFromInt n))) sumE) shift

buildTerm :: !Int !Real ![RadExpr Rational] !Int !Int -> RadExpr Rational
buildTerm n tau allRes j k
    # angle = tau * toReal (0 - j * k)
    # c_re = cos angle
    # c_im = sin angle
    | abs c_im < 0.0000000001 && abs (c_re - 1.0) < 0.0000000001
        = allRes !! j
    | abs c_im < 0.0000000001
        = Mul (Lit (approxRatRT c_re)) (allRes !! j)
    = Mul (Add (Lit (approxRatRT c_re))
               (Mul (Lit (approxRatRT c_im)) (Root 2 (Lit (ratFromInt (~1))))))
          (allRes !! j)

getCoeffsRT :: !(Poly Rational) -> [Rational]
getCoeffsRT (Poly cs) = cs

findCyclicOrdering :: ![(Real, Real)] !(Poly Rational) -> ?([(Real, Real)])
findCyclicOrdering roots _
    | length roots <> 5 = ?None
    = ?Just roots

foldl1Expr :: ((RadExpr Rational) (RadExpr Rational) -> RadExpr Rational) ![RadExpr Rational] -> RadExpr Rational
foldl1Expr _ [x] = x
foldl1Expr f [x:xs] = foldl f x xs
foldl1Expr _ [] = Lit (ratFromInt 0)

selectBranchRT :: !Int !(RadExpr Rational) !(Real, Real) -> RadExpr Rational
selectBranchRT _ expr _ = expr

approxRatRT :: !Real -> Rational
approxRatRT x
    # candidates = [(abs (toReal n / toReal d - x), mkRational (toInteger n) (toInteger d))
                    \\ d <- [1..1000], let n = toInt (x * toReal d + if (x >= 0.0) 0.5 (~0.5))
                    | abs (toReal n / toReal d - x) < 0.000001]
    | isEmpty candidates = ratFromInt (toInt (if (x >= 0.0) (x + 0.5) (x - 0.5)))
    = snd (hd (sortBy (\(a,_) (b,_) -> a < b) candidates))

caddRT :: !(Real, Real) !(Real, Real) -> (Real, Real)
caddRT (a, b) (c, d) = (a + c, b + d)

cmulRT :: !(Real, Real) !(Real, Real) -> (Real, Real)
cmulRT (a, b) (c, d) = (a*c - b*d, a*d + b*c)

cpowR :: !(Real, Real) !Int -> (Real, Real)
cpowR _ 0 = (1.0, 0.0)
cpowR z n
    | isOdd n = cmulRT z (cpowR z (n - 1))
    = let half = cpowR z (n / 2) in cmulRT half half
