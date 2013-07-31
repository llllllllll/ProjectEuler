-- 4075 - Completed 8.5.2013 - Wrote nCr and learned about defining functions 
module Problems.Problem_53
    ( problem_53
    ) where

import Data.List
import Utils.Misc

problem_53 = genericLength [1 | n <- [1..100], r <- [1..n], 
                            n `nCr` r > 1000000]