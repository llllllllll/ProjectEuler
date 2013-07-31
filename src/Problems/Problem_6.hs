-- 25164150 - Completed 29.4.2013 - Learned about map.
module Problems.Problem_6
    ( problem_6
    ) where

problem_6 = (sum [1..100]) ^ 2 - sum (map (^2) [1..100])