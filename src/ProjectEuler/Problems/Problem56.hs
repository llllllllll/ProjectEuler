-- 972 - Completed 10.5.2013.
module ProjectEuler.Problems.Problem56
    ( problem56 -- :: IO ()
    ) where

import ProjectEuler.Utils.List (intToList)

problem56 = print $ maximum $ [(sum . intToList) (a^b) | a <- [1..99]
                              ,  b <- [1..99]]
