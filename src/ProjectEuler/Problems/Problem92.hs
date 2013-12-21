-- 8581146 - Completed 22.5.2013 - Learned about forcing strictness.
module ProjectEuler.Problems.Problem92
    ( problem92 -- :: IO ()
    ) where

import ProjectEuler.Utils.List (intToList)

problem92 :: IO ()
problem92 = print $ length $ [n | n <- [1..100000], sqChain n]
  where
      sqChain n
          | n == 1 = False
          | n == 89 = True
          | otherwise = sqChain $! (sum (map (^2) (intToList n)))
