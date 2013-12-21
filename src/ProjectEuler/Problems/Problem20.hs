-- 648 - Completed 11.5.2013.
module ProjectEuler.Problems.Problem20
    ( problem20 -- IO ()
    ) where

import ProjectEuler.Utils.List (intToList)
import ProjectEuler.Utils.Number (factorial)

problem20 :: IO ()
problem20 = print $ (sum . intToList) $ factorial 100
