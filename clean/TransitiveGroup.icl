implementation module TransitiveGroup

import StdEnv
import Permutation
import PrimeFactors
import Positive

isSolvable :: !TransitiveGroup -> Bool
isSolvable tg = tg.tgSolvable

transGroupsOfDegree :: !Int -> [TransitiveGroup]
transGroupsOfDegree 5 = degree5Groups
transGroupsOfDegree n
    | n >= 3 && isPrime n = transGroupsOfPrimeRT n
    = []

transGroupByOrder :: !Int !Int -> [TransitiveGroup]
transGroupByOrder deg ord = [g \\ g <- transGroupsOfDegree deg | g.tgOrder == ord]

// ─── Degree-5 hard-coded groups ───

degree5Groups :: [TransitiveGroup]
degree5Groups =
    [ // T1: C5
      { tgName = "C5"
      , tgDegree = 5
      , tgOrder = 5
      , tgGenerators = [fromCycles 5 [[0, 1, 2, 3, 4]]]
      , tgSolvable = True
      , tgCompositionFactors = [5]
      , tgMaximalSupergroups = [1]  // D5
      }
    , // T2: D5
      { tgName = "D5"
      , tgDegree = 5
      , tgOrder = 10
      , tgGenerators =
          [ fromCycles 5 [[0, 1, 2, 3, 4]]
          , fromCycles 5 [[1, 4], [2, 3]]
          ]
      , tgSolvable = True
      , tgCompositionFactors = [5, 2]
      , tgMaximalSupergroups = [2]  // F20
      }
    , // T3: F20
      { tgName = "F20"
      , tgDegree = 5
      , tgOrder = 20
      , tgGenerators =
          [ fromCycles 5 [[0, 1, 2, 3, 4]]
          , fromCycles 5 [[1, 2, 4, 3]]
          ]
      , tgSolvable = True
      , tgCompositionFactors = [5, 2, 2]
      , tgMaximalSupergroups = [3, 4]  // A5, S5
      }
    , // T4: A5
      { tgName = "A5"
      , tgDegree = 5
      , tgOrder = 60
      , tgGenerators =
          [ fromCycles 5 [[0, 1, 2, 3, 4]]
          , fromCycles 5 [[0, 1, 2]]
          ]
      , tgSolvable = False
      , tgCompositionFactors = []
      , tgMaximalSupergroups = [4]  // S5
      }
    , // T5: S5
      { tgName = "S5"
      , tgDegree = 5
      , tgOrder = 120
      , tgGenerators =
          [ fromCycles 5 [[0, 1, 2, 3, 4]]
          , fromCycles 5 [[0, 1]]
          ]
      , tgSolvable = False
      , tgCompositionFactors = []
      , tgMaximalSupergroups = []
      }
    ]

// ─── Runtime prime-degree computation ───

// Modular exponentiation: b^e mod m
modExpRT :: !Int !Int !Int -> Int
modExpRT _ 0 _ = 1
modExpRT b e m
    | isEven e
        # half = modExpRT b (e / 2) m
        = (half * half) rem m
    = (b * modExpRT b (e - 1) m) rem m

// Primitive root modulo a prime p.
primitiveRootRT :: !Int -> Int
primitiveRootRT p = hd [g \\ g <- [2 .. p - 1] | isPrimRoot g]
where
    phi = p - 1
    factors = primeFactors (unsafePositive phi)
    isPrimRoot g = all (\q -> modExpRT g (phi / q) p <> 1) factors

// Sorted positive divisors of n.
divisorsRT :: !Int -> [Int]
divisorsRT n = sort (removeDups (flatten [if (d*d == n) [d] [d, n / d] \\ d <- [1 .. isqrt n] | n rem d == 0]))
where
    isqrt x = toInt (sqrt (toReal x))

// Prime factors with multiplicity as a flat list.
primeFactorsWithMultRT :: !Int -> [Int]
primeFactorsWithMultRT 1 = []
primeFactorsWithMultRT n = flatten [repeatn e p \\ (p, e) <- factorise (unsafePositive n)]

// All transitive subgroups of S_p for prime p.
// Solvable groups: Z/p ⋊ H for each divisor d of p-1.
// Non-solvable: A_p and S_p.
transGroupsOfPrimeRT :: !Int -> [TransitiveGroup]
transGroupsOfPrimeRT p
    # g = primitiveRootRT p
    # ds = divisorsRT (p - 1)
    # solvableGroups = [mkAffine p g d \\ d <- ds]
    # ap = { tgName = "A" +++ toString p
           , tgDegree = p
           , tgOrder = factorial p / 2
           , tgGenerators = [ fromCycles p [[0 .. p - 1]]
                            , fromCycles p [[0, 1, 2]]
                            ]
           , tgSolvable = p < 5
           , tgCompositionFactors = []
           , tgMaximalSupergroups = []
           }
    # sp = { tgName = "S" +++ toString p
           , tgDegree = p
           , tgOrder = factorial p
           , tgGenerators = [ fromCycles p [[0 .. p - 1]]
                            , fromCycles p [[0, 1]]
                            ]
           , tgSolvable = p < 4
           , tgCompositionFactors = []
           , tgMaximalSupergroups = []
           }
    # allGroups = sortBy (\a b -> a.tgOrder < b.tgOrder) (solvableGroups ++ [ap, sp])
    = assignMaximalSupergroups allGroups

mkAffine :: !Int !Int !Int -> TransitiveGroup
mkAffine p g d
    # trans = fromMapping [(i + 1) rem p \\ i <- [0 .. p - 1]]
    # scaleFactor = modExpRT g ((p - 1) / d) p
    # scale = fromMapping [(scaleFactor * i) rem p \\ i <- [0 .. p - 1]]
    # gens = if (d == 1) [trans] [trans, scale]
    # gName = if (d == 1) ("Z" +++ toString p)
              (if (d == 2) ("D" +++ toString p)
              (if (d == p - 1) ("AGL(1," +++ toString p +++ ")")
              ("Z" +++ toString p +++ ":Z" +++ toString d)))
    # cFactors = primeFactorsWithMultRT d ++ [p]
    = { tgName = gName
      , tgDegree = p
      , tgOrder = p * d
      , tgGenerators = gens
      , tgSolvable = True
      , tgCompositionFactors = cFactors
      , tgMaximalSupergroups = []
      }

// Assign maximal supergroups based on divisibility of orders.
assignMaximalSupergroups :: ![TransitiveGroup] -> [TransitiveGroup]
assignMaximalSupergroups groups
    # indexed = zip2 [0..] groups
    = [assignOne i tg indexed \\ (i, tg) <- indexed]

assignOne :: !Int !TransitiveGroup ![(Int, TransitiveGroup)] -> TransitiveGroup
assignOne myIdx tg indexed
    # myOrd = tg.tgOrder
    # cands = [(i, g.tgOrder) \\ (i, g) <- indexed | i <> myIdx && g.tgOrder > myOrd && g.tgOrder rem myOrd == 0]
    # maxSupers = [i \\ (i, superOrd) <- cands | isMaximal myOrd superOrd cands]
    = {tg & tgMaximalSupergroups = maxSupers}

isMaximal :: !Int !Int ![(Int, Int)] -> Bool
isMaximal myOrd superOrd cands
    = not (any (\(_, midOrd) -> midOrd > myOrd && midOrd < superOrd
                                && superOrd rem midOrd == 0
                                && midOrd rem myOrd == 0) cands)

factorial :: !Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n - 1)

// ─── Composition series ───

compositionSeries :: !TransitiveGroup -> ?([[Perm]])
compositionSeries tg
    | not tg.tgSolvable = ?None
    // Fast path: degree-5 hard-coded
    | tg.tgName == "C5" = ?Just [tg.tgGenerators, []]
    | tg.tgName == "D5" = ?Just [ tg.tgGenerators
                                , [fromCycles 5 [[0, 1, 2, 3, 4]]]
                                , []
                                ]
    | tg.tgName == "F20" = ?Just [ tg.tgGenerators
                                 , [ fromCycles 5 [[0, 1, 2, 3, 4]]
                                   , fromCycles 5 [[1, 4], [2, 3]]
                                   ]
                                 , [fromCycles 5 [[0, 1, 2, 3, 4]]]
                                 , []
                                 ]
    // General prime-degree affine groups
    | isPrime tg.tgDegree = compositionSeriesPrime tg
    = ?None

// Composition series for Z/p ⋊ H with |H| = d.
// Descend through subgroups of H by removing one prime factor at a time.
compositionSeriesPrime :: !TransitiveGroup -> ?([[Perm]])
compositionSeriesPrime tg
    # p = tg.tgDegree
    # g = primitiveRootRT p
    # d = tg.tgOrder / p
    # dFactors = primeFactorsWithMultRT d
    # dChain = scanl (\acc q -> acc / q) d dFactors
    # trans = fromCycles p [[0 .. p - 1]]
    # chain = [mkGens p g d` trans \\ d` <- dChain] ++ [[]]
    = ?Just chain

mkGens :: !Int !Int !Int !Perm -> [Perm]
mkGens p g d` trans
    | d` <= 1 = [trans]
    # sf = modExpRT g ((p - 1) / d`) p
    # scale = fromMapping [(sf * i) rem p \\ i <- [0 .. p - 1]]
    = [trans, scale]

// scanl for Int
scanl :: (Int Int -> Int) !Int ![Int] -> [Int]
scanl _ acc [] = [acc]
scanl f acc [x:xs] = [acc : scanl f (f acc x) xs]
