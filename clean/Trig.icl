implementation module Trig

import StdEnv
import RadExpr
import Rational
import Poly
import Normalize
import NormalForm
import RootOfUnity
import Cyclotomic
import DAG
import Eval

gcdI :: !Int !Int -> Int
gcdI a 0 = abs a
gcdI a b = gcdI b (a rem b)

// ─── cosExact ───
cosExact :: !Int !Int -> TrigResult
cosExact p q
    | q <= 0 = abort "cosExact: non-positive denominator"
    # g = gcdI (abs p) q
    # p` = p / g
    # q` = q / g
    = cosReduced p` q`

cosReduced :: !Int !Int -> TrigResult
cosReduced p q
    # p` = p rem (2 * q)
    # p`` = if (p` < 0) (p` + 2 * q) p`
    = cosInRange p`` q

cosInRange :: !Int !Int -> TrigResult
cosInRange p q
    | p == 0 = Radical (Lit (ratFromInt 1))
    | 2 * p == q = Radical (Lit (ratFromInt 0))
    | p == q = Radical (Lit (ratFromInt (~1)))
    | 2 * p == 3 * q = Radical (Lit (ratFromInt 0))
    | 2 * p > q && p < q
        = case cosInRange (q - p) q of
            Radical e -> Radical (Neg e)
            other -> other
    | p > q && 2 * p < 3 * q
        = case cosInRange (p - q) q of
            Radical e -> Radical (Neg e)
            other -> other
    | 2 * p >= 3 * q
        = cosInRange (2 * q - p) q
    = cosFirstQuadrant p q

cosFirstQuadrant :: !Int !Int -> TrigResult
cosFirstQuadrant p q
    # g = gcdI p (2 * q)
    # n = 2 * q / g
    # k = p / g
    | k == 1
        = case cosOfUnity n of
            ?Just e -> Radical (safeDenestAndNormalize e)
            ?None -> MinPoly (cyclotomic n)
    = case cosOfUnity n of
        ?Just base -> Radical (safeDenestAndNormalize (chebyshev k base))
        ?None -> MinPoly (cyclotomic n)

safeDenestAndNormalize :: !(RadExpr Rational) -> RadExpr Rational
safeDenestAndNormalize e
    # dag = toDAG e
    # s = dagSize dag
    | s > 5000 = fromDAG (dagFoldConstants dag)
    | s <= 500 = normalize (fromDAG (dagFoldConstants dag))
    = fromDAG (dagFoldConstants dag)

chebyshev :: !Int !(RadExpr Rational) -> RadExpr Rational
chebyshev 0 _ = Lit (ratFromInt 1)
chebyshev 1 x = x
chebyshev k x = go 2 (Lit (ratFromInt 1)) x
where
    go n t0 t1
        | n > k = t1
        # t2 = Add (Mul (Mul (Lit (ratFromInt 2)) x) t1) (Neg t0)
        = go (n + 1) t1 t2

// ─── sinExact ───
sinExact :: !Int !Int -> TrigResult
sinExact p q
    | q <= 0 = abort "sinExact: non-positive denominator"
    # g = gcdI (abs p) q
    # p` = p / g
    # q` = q / g
    = sinReduced p` q`

sinReduced :: !Int !Int -> TrigResult
sinReduced p q
    # p` = p rem (2 * q)
    # p`` = if (p` < 0) (p` + 2 * q) p`
    # positive = p`` >= 0 && p`` <= q
    | p`` == 0 || p`` == q = Radical (Lit (ratFromInt 0))
    = case cosExact p q of
        Radical c -> Radical (safeDenestAndNormalize (let sin2 = normalize (Add (Lit (ratFromInt 1)) (Neg (Mul c c)))
                                                          sinExpr = Root 2 sin2
                                                      in if positive sinExpr (Neg sinExpr)))
        other -> other

// ─── simplifyTrigResult ───
simplifyTrigResult :: !TrigResult -> TrigResult
simplifyTrigResult (Radical e) = Radical (normalize e)
simplifyTrigResult other = other
