-- NOT COMPLETED.
module Problems.Problem_179
    ( problem_179
    ) where

import Utils.Misc

problem_179 = [n | n <- [1..10^7-1], num_divisors n == num_divisors (n+1)]