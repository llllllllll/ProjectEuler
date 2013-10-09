-- 40730 - Completed 5.5.2013.
module Problems.Problem_34
    ( problem_34
    ) where

import Utils.List (int_to_list)
import Utils.Number (factorial)

problem_34 = sum [x | x <- [3..99999], is_curious x]
    where
	is_curious n = (sum . map factorial) (int_to_list n) == n
