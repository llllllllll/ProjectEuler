-- 1587000 - Completed 22.5.2013.
module ProjectEuler.Problems.Problem112
    ( problem112 -- :: IO ()
    ) where

import Data.Ratio ((%))
import ProjectEuler.Utils.Misc (isBouncy)

problem112 = p 21780 19602
  where
      p n a
          | a % n >= (99 % 100) = n
          | isBouncy n = p (n + 1) (a + 1)
          | otherwise = p (n + 1) a
