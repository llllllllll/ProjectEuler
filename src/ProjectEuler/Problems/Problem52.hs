-- 142857 - Completed 5.5.2013.
module ProjectEuler.Problems.Problem52
    ( problem52 -- :: IO ()
    ) where

import Data.List               (nub,sort)
import ProjectEuler.Utils.List (intToList)

problem52 :: IO ()
problem52 = print $ head [n | n <- [1..], isValid n]
  where
      isValid n = (sort . intToList) n == ((nub . sort . concat . map
                                            (\x -> intToList (x*n))) [2..6])
