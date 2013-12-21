-- 232792560 - Completed 29.4.2013 - Learned about folds.
module ProjectEuler.Problems.Problem5
    ( problem5
    ) where

import Data.List (foldl)

problem5 :: IO ()
problem5 = print $ foldl (lcm) 1 [1..20]
