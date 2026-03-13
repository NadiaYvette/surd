implementation module TransitiveGroup

import StdEnv
import Permutation

isSolvable :: !TransitiveGroup -> Bool
isSolvable tg = tg.tgSolvable

transGroupsOfDegree :: !Int -> [TransitiveGroup]
transGroupsOfDegree 5 = degree5Groups
transGroupsOfDegree _ = []

transGroupByOrder :: !Int !Int -> [TransitiveGroup]
transGroupByOrder deg ord = [g \\ g <- transGroupsOfDegree deg | g.tgOrder == ord]

degree5Groups :: [TransitiveGroup]
degree5Groups =
    [ // T1: C5
      { tgName = "C5"
      , tgDegree = 5
      , tgOrder = 5
      , tgGenerators = [fromCycles 5 [[0, 1, 2, 3, 4]]]
      , tgSolvable = True
      , tgCompositionFactors = [5]
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
      }
    ]
