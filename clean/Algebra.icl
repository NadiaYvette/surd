implementation module Algebra

import StdEnv

// Repeated-squaring exponentiation in any Ring.
rpow :: !a !Int -> a | Ring a
rpow _ 0 = rone
rpow x n
    | n < 0 = abort "rpow: negative exponent (use Field.finv for inverses)"
    | isOdd n = rmul x (rpow x (n - 1))
    = let half = rpow x (n / 2) in rmul half half
