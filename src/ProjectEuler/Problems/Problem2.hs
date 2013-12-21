-- 4613732 - Completed 29.4.2013 - Learned list comprehensions.
module ProjectEuler.Problems.Problem2
    ( problem2 -- :: IO ()
    ) where

import ProjectEuler.Utils.Sequence (fibs)

problem2 :: IO ()
problem2 = print $ sum [x | x <- takeWhile (< 4000000) fibs, even x]
