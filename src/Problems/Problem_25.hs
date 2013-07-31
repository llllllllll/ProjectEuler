-- NOT COMPLETE.
module Problems.Problem_25
    ( problem_25
    ) where

import Utils.Sequence

problem_25 = head [x | x <- [1..], (length . show . fib) x == 1000]