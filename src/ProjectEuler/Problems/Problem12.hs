-- 76576500 - Completed 26.5.12.
module ProjectEuler.Problems.Problem12
    ( problem12 -- :: IO ()
    ) where

import ProjectEuler.Utils.Number (numDivisors)

problem12 :: IO ()
problem12 = print $ head [x | x <- triNums, numDivisors x > 500]
    where
	triNums = tail $ scanl (+) 0 [1..]
