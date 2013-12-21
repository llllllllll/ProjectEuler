-- 210 - Completed 15.5.2013 - Leaned about Champernowne's Constant.
module ProjectEuler.Problems.Problem40
    ( problem40 -- :: IO ()
    ) where

import Data.Char                   (digitToInt)
import ProjectEuler.Utils.Constant (champernowne)

problem40 :: IO ()
problem40 = print $ product $ map (digitToInt . (!!) champernowne) indecies
  where
      indecies = map (10^) [1..6]
