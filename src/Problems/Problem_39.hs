-- 840 - Completed 24.5.2013.
module Problems.Problem_39
    ( problem_39
    ) where

import Data.List (sortBy,group,sort)
import Data.Function (on)
import Utils.Misc (is_int)

problem_39 = round $ head $ last $ sortBy (compare `on` length) $ group $ 
             sort [let c = sqrt (a^2 + b^2) in a+b+c | a <- [1..998], 
                       b <- [1..a], let c = sqrt (a^2 + b^2) 
                                    in is_int c && (a+b+c) <= 1000 
                                           && (a^2 + b^2) == c^2]
