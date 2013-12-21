-- NOT COMPLETED.
module Problems.Problem62
    ( problem62
    ) where

import Data.List (permutations,nub)
import Utils.Number (isCube)
import Utils.List (intToList,listToInt)

problem62 = [n | n <- cubes, isValid n]
    where
	cubes = map (^3) [1..]
	isValid n = 3 == (length . nub) 
                     [m | m <- permutations (intToList n), 
                      head m /= 0 && isCube (listToInt m)]
 