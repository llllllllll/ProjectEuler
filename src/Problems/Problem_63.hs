-- 49 - Completed 25.5.2013.
module Problems.Problem_63
    ( problem_63
    ) where

problem_63 = length [x^n | x <- [1..100], n <- [1..100], 
                                (length . show) (x^n) == n]