-- 2783915460 - Completed 5.5.2013.
module ProjectEuler.Problems.Problem24
    ( problem24 -- :: IO ()
    ) where

import Data.List (sort,permutations)

problem24 :: IO ()
problem24 =  print $ (sort . permutations) ['0'..'9'] !! 999999
