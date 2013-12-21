-- 104743 - Completed 29.4.2013 - Learned how to index a list.
module ProjectEuler.Problems.Problem7
    ( problem7 -- :: IO ()
    ) where

import ProjectEuler.Utils.Prime (primes)

problem7 :: IO ()
problem7 = print $ primes !! 10001
