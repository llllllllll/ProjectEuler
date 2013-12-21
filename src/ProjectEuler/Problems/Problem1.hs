-- 233168 - Completed 29.4.2013
module ProjectEuler.Problems.Problem1
    ( problem1  -- :: IO ()
    , problem1' -- :: IO ()
    ) where

import Data.List (union)

problem1 :: IO ()
problem1 = print $ sum [x | x <- [1..999], x `rem` 3 == 0 || x `rem` 5 == 0]

problem1' :: IO ()
problem1' = print $ sum $ union [3,6..999] [5,10..995]
