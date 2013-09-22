-- 233168 - Completed 29.4.2013
module Problems.Problem_1 
    ( problem_1 
    , problem_1'
    ) where

import Data.List (union)

problem_1 = sum [x | x <- [1..999], x `rem` 3 == 0 || x `rem` 5 == 0]

problem_1' = sum $ union [3,6..999] [5,10..995]
