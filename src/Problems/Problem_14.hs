-- 837799 - Completed 29.4.2013 - Revised 25.5.2013.
module Problems.Problem_14
    ( problem_14
    ) where

import Utils.Sequence (collatz_mem,collatz)

problem_14 = collatz_mem (10^6) (0,0)
    where
	collatz_mem n (m,x)
	    | n == 1 = x
	    | (length . collatz) n <= m = 
                collatz_mem (n-1) (((length . collatz) n),n)
	    | (length . collatz) n > m = collatz_mem (n-1) (m,x)
