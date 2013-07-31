-- NOT COMPLETED.
module Problems.Problem_171
    ( problem_171
    ) where

import Utils.List
import Utils.Misc

problem_171 = [n | n <- [1..10^20-1], is_square (f n)]
    where
	f n = sum $ map (^2) (int_to_list n)