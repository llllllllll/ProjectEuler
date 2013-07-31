-- NOT COMPLETE.
module Problems.Problem_23
    ( problem_23
    ) where

import Utils.Misc

problem_23 = [a + b | a <- abundants, b <- takeWhile (<=a) abundants, 
              a + b < 28123]
    where
	abundants = filter is_abundant [12..28111]
	is_abundant n = is_abundant' n (divisors n) 0
	is_abundant' k (n:ns) c
	    | null ns = n + c > k
	    | n + c > k = True
	    | otherwise = is_abundant' k ns (c+n)