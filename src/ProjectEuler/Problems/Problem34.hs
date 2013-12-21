-- 40730 - Completed 5.5.2013.
module ProjectEuler.Problems.Problem34
    ( problem34 -- :: IO ()
    ) where

import ProjectEuler.Utils.List   (intToList)
import ProjectEuler.Utils.Number (factorial)

problem34 :: IO ()
problem34 = print $ sum [x | x <- [3..99999], isCurious x]
  where
      isCurious n = (sum . map factorial) (intToList n) == n
