-- 1366 - Completed 2.6.2013.
module ProjectEuler.Problems.Problem16
    ( problem16 -- :: IO ()
    ) where

import ProjectEuler.Utils.List (intToList)

problem16 :: IO ()
problem16 = print $ sum $ intToList (2^1000)
