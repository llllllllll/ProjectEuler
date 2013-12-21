-- NOT COMPLETE.
module ProjectEuler.Problems.Problem25
    ( problem25 -- :: IO ()
    ) where

import ProjectEuler.Utils.Sequence (fib)

problem25 :: IO ()
problem25 = print $ head [x | x <- [1..], (length . show . fib) x == 1000]
