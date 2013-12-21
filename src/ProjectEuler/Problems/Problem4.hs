-- 906609 - Completed 29.4.2013 - Learned how to use multiple generators.
module ProjectEuler.Problems.Problem4
    ( problem4 -- IO ()
    ) where

problem4 :: IO ()
problem4 = print $ maximum [x * y | x <- [100..999], y <- [100..999]
                           , reverse (show (x * y)) == show (x * y)]
