-- 7652413 - Completed 24.5.2013 - WARNING: Does not terminate!
module ProjectEuler.Problems.Problem41
    ( problem41 -- :: IO ()
    ) where

import ProjectEuler.Utils.Prime (primes)
import ProjectEuler.Utils.Misc  (isPandigitalr)

problem41 :: IO ()
problem41 = print [n | n <- primes, isPandigitalr (1,(length . show) n) n]
