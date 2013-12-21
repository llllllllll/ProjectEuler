-- 31875000 - Completed 3.6.2013.
module ProjectEuler.Problems.Problem9
    ( problem9 -- IO ()
    ) where

problem9 :: IO ()
problem9 = print $ [let c = sqrt (a^2 + b^2) in a * b * c | a <- [3..1000]
                   , b <- [1..a - 1], a + b + (sqrt (a^2 + b^2)) == 1000]
