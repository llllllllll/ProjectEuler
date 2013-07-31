-- 134043 - Completed 17.5.2013.
module Problems.Problem_47
    ( problem_47
    ) where

import Data.List
import Utils.Prime

problem_47 = head [n | n <- [1..], is_valid n]
    where
	is_valid n = all (==4) $ 
                     map (length . nub . prime_factors) [n..n+3]