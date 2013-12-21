-- 837799 - Completed 29.4.2013 - Revised 25.5.2013.
module ProjectEuler.Problems.Problem14
    ( problem14 -- IO ()
    ) where

import ProjectEuler.Utils.Sequence (collatzMem,collatz)

problem14 :: IO ()
problem14 = print $ collatzMem (10^6) (0,0)
  where
      collatzMem n (m,x)
          | n == 1 = x
          | (length . collatz) n <= m = collatzMem (n - 1)
                                        (((length . collatz) n),n)
          | otherwise                 = collatzMem (n - 1) (m,x)
