-- 249 - Completed 15.5.2013 -Learned about Lychrel Numbers.
module Problems.Problem_55
    ( problem_55
    ) where

import Utils.Misc

problem_55 = length [n | n <- [1..9999], is_lychrel n]