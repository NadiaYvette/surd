definition module TransitiveGroup

// Database of transitive subgroups of S5 for Galois group identification.

from Permutation import :: Perm

:: TransitiveGroup = { tgName :: !{#Char}, tgDegree :: !Int, tgOrder :: !Int, tgGenerators :: ![Perm], tgSolvable :: !Bool, tgCompositionFactors :: ![Int] }

// All transitive subgroups of S_n (up to conjugacy) for degree n.
transGroupsOfDegree :: !Int -> [TransitiveGroup]

// Find transitive group(s) of given degree and order.
transGroupByOrder :: !Int !Int -> [TransitiveGroup]

// Test solvability.
isSolvable :: !TransitiveGroup -> Bool
