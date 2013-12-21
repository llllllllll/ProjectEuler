-- 840 - Completed 24.5.2013.
module ProjectEuler.Problems.Problem39
    ( problem39 -- :: IO ()
    ) where

import Data.List                 (sortBy,group,sort)
import Data.Function             (on)
import ProjectEuler.Utils.Number (isInt)

problem39 :: IO ()
problem39 = print $ round $ head $ last $ sortBy (compare `on` length) $ group
            $ sort [let c = sqrt (a^2 + b^2) in a+b+c | a <- [1..998]
                   , b <- [1..a]
                   , let c = sqrt (a^2 + b^2)
                     in isInt c && (a+b+c) <= 1000 && (a^2 + b^2) == c^2]
