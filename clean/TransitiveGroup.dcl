definition module TransitiveGroup

// Database of transitive subgroups of S_n for Galois group identification.
// For prime degree p, solvable transitive subgroups are Z/p ⋊ H where
// H ranges over divisors of p-1. Non-solvable: A_p and S_p.
// Degree 5 uses a hard-coded table; all other primes are computed at runtime
// from the AGL(1,p) structure.

from Permutation import :: Perm

:: TransitiveGroup =
    { tgName               :: !{#Char}
    , tgDegree             :: !Int
    , tgOrder              :: !Int
    , tgGenerators         :: ![Perm]
    , tgSolvable           :: !Bool
    , tgCompositionFactors :: ![Int]
    , tgMaximalSupergroups :: ![Int]
    }

// All transitive subgroups of S_n (up to conjugacy) for degree n.
// Returns degree-5 hard-coded groups or runtime AGL(1,p) groups for prime p.
// Returns [] for unsupported composite degrees.
transGroupsOfDegree :: !Int -> [TransitiveGroup]

// Find transitive group(s) of given degree and order.
transGroupByOrder :: !Int !Int -> [TransitiveGroup]

// Test solvability.
isSolvable :: !TransitiveGroup -> Bool

// Composition series for a solvable group: list of generating sets
// descending from G to the trivial group {1}.
// Returns ?None for non-solvable groups.
compositionSeries :: !TransitiveGroup -> ?([[Perm]])
