-- 9183 - Completed 3.5.2013.
module Problems.Problem_29
    ( problem_29
    ) where

import Data.List (nub)

problem_29 = length $ nub [a^b | a <- [2..100], b <- [2..100]] 
