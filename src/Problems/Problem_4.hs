-- 906609 - Completed 29.4.2013 - Learned how to use multiple generators.
module Problems.Problem_4 
    ( problem_4
    ) where

problem_4 = maximum [x*y | x <- [100..999], y <- [100..999], 
                     reverse (show (x*y)) == show (x*y)]