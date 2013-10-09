-- NOT COMPLETED.
module Problems.Problem_171
    ( problem_171
    ) where

import Utils.List (int_to_list)
import Utils.Number (is_square)

problem_171 = [n | n <- [1..10^20-1], is_square (f n)]
    where
	f n = sum $ map (^2) (int_to_list n)
