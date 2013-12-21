-- 6857 - Completed 17.5.2013 - Forgot to fix after new primeFactors was done.
module ProjectEuler.Problems.Problem3
    ( problem3 -- :: IO ()
    ) where

import ProjectEuler.Utils.Prime (primeFactors)

problem3 :: IO ()
problem3 = print $ last $ primeFactors 600851475143
