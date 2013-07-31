-- 4613732 - Completed 29.4.2013 - Learned list comprehensions.
module Problems.Problem_2
    ( problem_2
    ) where

import Utils.Sequence    

problem_2 = sum [x | x <- takeWhile (<4000000) (map fib [2..]), even x]