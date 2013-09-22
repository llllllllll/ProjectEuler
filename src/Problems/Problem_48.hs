-- 9110846700 - Completed 5.5.2013 - Wrote my last_n for this.
module Problems.Problem_48
    ( problem_48
    ) where

import Utils.List (last_n)

problem_48 = last_n 10 (sum (map (\x -> x^x) [1..1000]))
