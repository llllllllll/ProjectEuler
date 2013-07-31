-- NOT COMPLETED.
module Problems.Problem_95
    ( problem_95
    ) where

import Data.List
import Data.Function
import Utils.Misc

problem_95 = last $ sortBy (compare `on` (length . snd)) 
             [(n, a_chain n [n]) | n <- [1..10^6]]
    where
	sum_divisors = map (sum . divisors) [1..]
	a_chain n ns
	    | n == head ns && length ns > 1 = ns
	    | n `elem` tail ns && length ns > 1 = []
	    | otherwise = a_chain (sum_divisors!!n) (n:ns)