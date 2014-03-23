-- NOT COMPLETE.
module ProjectEuler.Problems.Problem23
    ( problem23 -- :: IO ()
    ) where

import ProjectEuler.Utils.Number (pDivisors)

problem23 :: IO ()
problem23 = print $ sequence [abundants,abundants]
    where
	abundants = filter isAbundant [12..28111]
	isAbundant n = (sum . pDivisors) n > n
