-- 134043 - Completed 17.5.2013.
module ProjectEuler.Problems.Problem47
    ( problem47 -- :: IO ()
    ) where

import Data.List                (nub)
import ProjectEuler.Utils.Prime (primeFactors)

problem47 :: IO ()
problem47 = print $ head [n | n <- [1..], isValid n]
  where
      isValid n = all (== 4) $ map (length . nub . primeFactors) [n..n + 3]
