-- NOT COMPLETED.
module Problems.Problem95
    ( problem95
    ) where

import Data.List (sortBy)
import Data.Function (on)
import Utils.Number (divisors)

problem95 = last $ sortBy (compare `on` (length . snd)) 
             [(n, aChain n [n]) | n <- [1..10^6]]
    where
	sumDivisors = map (sum . divisors) [1..]
	aChain n ns
	    | n == head ns && length ns > 1 = ns
	    | n `elem` tail ns && length ns > 1 = []
	    | otherwise = aChain (sumDivisors!!n) (n:ns)
 