-- 4075 - Completed 8.5.2013 - Wrote nCr and learned about defining functions
module ProjectEuler.Problems.Problem53
    ( problem53 -- :: IO ()
    ) where

import Data.List                 (genericLength)
import ProjectEuler.Utils.Number (nCr)

problem53 :: IO ()
problem53 = print $ genericLength [1 | n <- [1..100], r <- [1..n]
                                  , n `nCr` r > 1000000]
