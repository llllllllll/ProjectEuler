-- NOT COMPLETED.
module Problems.Problem_216
    ( problem_216
    ) where

import Utils.Prime

problem_216 = [2*n^2-1 | n <- [2..50000000], is_prime (n*n^2-1)]