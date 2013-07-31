-- NOT COMPLETED.
module Problems.Problem_62
    ( problem_62
    ) where

import Data.List
import Utils.Misc
import Utils.List

problem_62 = [n | n <- cubes, is_valid n]
    where
	cubes = map (^3) [1..]
	is_valid n = 3 == (length . nub) 
                     [m | m <- permutations (int_to_list n), 
                      head m /= 0 && is_cube (list_to_int m)]