-- 249 - Completed 15.5.2013 -Learned about Lychrel Numbers.
module ProjectEuler.Problems.Problem55
    ( problem55 -- :: IO ()
    ) where

import ProjectEuler.Utils.Misc (isLychrel)

problem55 :: IO ()
problem55 = print $ length [n | n <- [1..9999], isLychrel n]
