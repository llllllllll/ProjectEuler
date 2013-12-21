-- 142913828922 - Completed 29.4.2013.
module ProjectEuler.Problems.Problem10
    ( problem10 -- IO ()
    ) where

import ProjectEuler.Utils.Prime (primes)

problem10 :: IO ()
problem10 = print $ sum $ takeWhile (< 2000000) primes
