-- 232792560 - Completed 29.4.2013 - Learned about folds.
module Problems.Problem_5 
    ( problem_5
    ) where

import Data.List (foldl)

problem_5 = foldl (lcm) 1 [1..20]
