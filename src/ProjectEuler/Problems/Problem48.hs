-- 9110846700 - Completed 5.5.2013 - Wrote my lastN for this.
module ProjectEuler.Problems.Problem48
    ( problem48 -- :: IO ()
    ) where

import ProjectEuler.Utils.List (lastN)

problem48 :: IO ()
problem48 = print $ lastN 10 (sum (map (\x -> x^x) [1..1000]))
