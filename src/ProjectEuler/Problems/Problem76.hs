-- 190569291 - Completed 4.6.2013.
module ProjectEuler.Problems.Problem76
    ( problem76
    ) where

import ProjectEuler.Utils.Number (partitions)

problem76 = print $ partitions 100 - 1
