implementation module Elliptic

import StdEnv
import RadExpr
import Rational
import Poly
import Normalize
import Resolvent
import Data.Integer

reduceElliptic :: !Bool !EllipticIntegrand -> ?(EllipticResult)
reduceElliptic _ ei
    # d = degree ei.eiRadicand
    | d < 3 || d > 4 = ?None
    // Find roots of radicand
    # roots = complexRootsOf ei.eiRadicand
    # realRoots = sortBy (<) [fst r \\ r <- roots | abs (snd r) < 0.000001]
    | length realRoots < d = ?None  // not all roots are real
    // Convert to Rational
    # ratRoots = [approxRatE r \\ r <- realRoots]
    // Compute modulus k^2 from cross-ratio of roots
    # (modSq, modExpr) = computeModulus d ratRoots
    = ?Just { erModulus = Root 2 modExpr
            , erModulusSq = modExpr
            , erRoots = ratRoots
            , erDescription = "F(phi, k) with k^2 = " +++ toString modSq
            }

computeModulus :: !Int ![Rational] -> (Rational, RadExpr Rational)
computeModulus 3 [e1, e2, e3]
    // For cubic with roots e1 <= e2 <= e3:
    // k^2 = (e2 - e1) / (e3 - e1)
    # modSq = (e2 - e1) / (e3 - e1)
    = (modSq, Lit modSq)
computeModulus 4 [e1, e2, e3, e4]
    // For quartic with roots e1 <= e2 <= e3 <= e4:
    // k^2 = ((e2-e1)(e4-e3)) / ((e3-e1)(e4-e2))
    # modSq = ((e2 - e1) * (e4 - e3)) / ((e3 - e1) * (e4 - e2))
    = (modSq, Lit modSq)
computeModulus _ _ = (zero, Lit zero)

approxRatE :: !Real -> Rational
approxRatE x
    # candidates = [(abs (toReal n / toReal d - x), mkRational (toInteger n) (toInteger d))
                    \\ d <- [1..1000], let n = toInt (x * toReal d + if (x >= 0.0) 0.5 (~0.5))
                    | abs (toReal n / toReal d - x) < 0.000001]
    | isEmpty candidates = ratFromInt (toInt (if (x >= 0.0) (x + 0.5) (x - 0.5)))
    = snd (hd (sortBy (\(a,_) (b,_) -> a < b) candidates))

prettyEllipticResult :: !EllipticResult -> {#Char}
prettyEllipticResult er
    = "  " +++ er.erDescription +++ "\n"
      +++ "  Roots: " +++ showRoots er.erRoots +++ "\n"
      +++ "  k = " +++ toString (er.erModulus) +++ "\n"

showRoots :: ![Rational] -> {#Char}
showRoots [] = "[]"
showRoots [x] = toString x
showRoots [x:xs] = toString x +++ ", " +++ showRoots xs

latexEllipticResult :: !EllipticResult -> {#Char}
latexEllipticResult er
    = "  $" +++ er.erDescription +++ "$\n"
