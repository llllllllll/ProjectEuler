-- 648 - Completed 11.5.2013.
module Problems.Problem_20
    ( problem_20
    ) where

import Utils.List (int_to_list)
import Utils.Number (factorial)

problem_20 = (sum . int_to_list) $ factorial 100
