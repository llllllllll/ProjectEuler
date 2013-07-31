-- 55 - Completed 23.5.2013.
module Problems.Problem_35
    ( problem_35
    ) where

import Data.List
import Utils.Prime
import Utils.List

problem_35 = length [n | n <- takeWhile (<1000000) primes, is_valid n]
    where
	circulate ns = init (zipWith (++) (tails ns) (inits ns))
	is_valid n = all (is_prime . list_to_int) $ 
                     circulate (int_to_list n)