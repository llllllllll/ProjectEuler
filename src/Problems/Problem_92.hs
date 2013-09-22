-- 8581146 - Completed 22.5.2013 - Learned about forcing strictness.
module Problems.Problem_92
    ( problem_92
    ) where

import Utils.List (int_to_list)

problem_92 = length $ [n | n <- [1..100000], sq_chain n]
    where
	sq_chain n
	    | n == 1 = False
	    | n == 89 = True
	    | otherwise = sq_chain $! 
                          (sum (map (^2) (int_to_list n)))
