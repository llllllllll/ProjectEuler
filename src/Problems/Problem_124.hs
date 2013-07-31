-- 21417 - Completed 2.6.2013.
module Problems.Problem_124
    ( problem_124
    ) where

import Data.List
import Utils.Prime
import Data.Function

problem_124 = fst $ (sortBy (compare `on` snd) [(n, rad n) 
                                                | n <- [1..100000]])!!9999
    where
	rad n = product $ nub $ prime_factors n