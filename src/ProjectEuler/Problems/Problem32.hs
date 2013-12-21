-- NOT COMPLETE.
module Problems.Problem32
    ( problem32
    ) where

import Utils.List
import Utils.Misc

problem32 = [(a,b,a*b)| a <- [2..10000], b <- [1..a-1], isValid a b]
    where
	isValid a b = isPandigital (1,9) (listToInt [a*b,a,b]) 