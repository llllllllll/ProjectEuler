-- NOT COMPLETED.
module Problems.Problem_188
    ( problem_188
    ) where

import Utils.List (last_n)
import Utils.Misc (hyper_exp)

problem_188 = last_n 8 (hyper_exp 1777 1855)
