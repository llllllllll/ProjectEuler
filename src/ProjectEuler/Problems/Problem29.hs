-- 9183 - Completed 3.5.2013.
module ProjectEuler.Problems.Problem29
    ( problem29 -- :: IO ()
    ) where

import Data.List (nub)

problem29 :: IO ()
problem29 = print $ length $ nub [a^b | a <- [2..100], b <- [2..100]]
