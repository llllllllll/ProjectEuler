-- NOT COMPLETE.
module Problems.Problem_32
    ( problem_32
    ) where

import Utils.List
import Utils.Misc

problem_32 = [(a,b,a*b)| a <- [2..10000], b <- [1..a-1], is_valid a b]
    where
	is_valid a b = is_pandigital (1,9) (list_to_int [a*b,a,b])