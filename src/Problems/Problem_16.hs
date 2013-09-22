-- 1366 - Completed 2.6.2013.
module Problems.Problem_16
    ( problem_16
    ) where

import Utils.List (int_to_list)

problem_16 = sum $ int_to_list (2^1000)
