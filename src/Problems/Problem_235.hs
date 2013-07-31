-- 1.002322108633 (Paper/Pencil)- Completed 11.5.2013 - 
module Problems.
module Problem_235
    ( problem_235
    ) where

import Utils.Misc

problem_235 = bisection_search f (0-600000000000) (1,1.5) 0.000000000001
    where
	f r = sum [(900-3*k)*r**(k-1) | k <- [1..5000]] 