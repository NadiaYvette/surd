implementation module RootBound

import StdEnv
import Poly
import Rational

// Cauchy bound: 1 + max(|a_i/a_n|) for i = 0..n-1
cauchyBound :: !(Poly Rational) -> Rational
cauchyBound (Poly []) = zero
cauchyBound (Poly cs)
    | length cs <= 1 = zero
    # lc = last cs
    | lc == zero = abort "cauchyBound: zero leading coefficient"
    # ratios = [abs (c / lc) \\ c <- init cs]
    = one + maxRat ratios

maxRat :: ![Rational] -> Rational
maxRat [] = zero
maxRat [x] = x
maxRat [x:xs] = let m = maxRat xs in if (x < m) m x
