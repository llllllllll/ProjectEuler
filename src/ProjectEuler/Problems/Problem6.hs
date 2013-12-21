-- 25164150 - Completed 29.4.2013 - Learned about map.
module ProjectEuler.Problems.Problem6
    ( problem6 -- :: IO ()
    ) where

problem6 :: IO ()
problem6 = print $ (sum [1..100]) ^ 2 - sum (map (^2) [1..100])
