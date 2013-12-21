-- 49 - Completed 25.5.2013.
module ProjectEuler.Problems.Problem63
    ( problem63 -- :: IO ()
    ) where

problem63 :: IO ()
problem63 = print $ length [x^n | x <- [1..100], n <- [1..100]
                           , (length . show) (x^n) == n]
