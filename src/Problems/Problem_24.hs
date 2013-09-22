-- 2783915460 - Completed 5.5.2013.
module Problems.Problem_24
    ( problem_24
    ) where

import Data.List (sort,permutations)

problem_24 =  (sort . permutations) ['0'..'9'] !! 999999
