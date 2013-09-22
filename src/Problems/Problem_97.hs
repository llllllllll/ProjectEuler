-- 8739992577 - Completed 8.5.2013.
module Problems.Problem_97
    ( problem_97
    ) where

import Utils.List (last_n)

problem_97 = last_n 10 $ 28433*2^(7830457)+1
