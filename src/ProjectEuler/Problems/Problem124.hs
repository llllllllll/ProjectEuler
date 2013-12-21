-- 21417 - Completed 2.6.2013.
module ProjectEuler.Problems.Problem124
    ( problem124 -- :: IO ()
    ) where

import Data.List (sortBy,nub)
import Data.Function (on)
import ProjectEuler.Utils.Prime (primeFactors)

problem124 :: IO ()
problem124 = print $ fst
             $ (sortBy (compare `on` snd) [(n, rad n) | n <- [1..100000]])!!9999
  where
      rad n = product $ nub $ primeFactors n
