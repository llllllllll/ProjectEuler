-- 21124 - Completed 22.9.2013
module ProjectEuler.Problems.Problem17
    ( problem17 -- :: IO ()
    ) where

import ProjectEuler.Utils.Misc (showEngl)

problem17 :: IO ()
problem17 = print $ sum $ map (length . filter (/= ' ') . showEngl) [1..1000]
