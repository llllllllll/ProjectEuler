-- NOT YET COMPLETED.
module Problems.Problem_19
    ( problem_19
    , is_leap_year
    ) where

import Data.List
import Data.Maybe

months :: [(String,Int)]
months = [ ("Jan",31)
         , ("Feb",28)
         , ("Mar",31) 
         , ("Apr",30) 
         , ("May",31)
         , ("Jun",30)
         , ("Jul",31)
         , ("Aug",31)
         , ("Sep",30)
         , ("Oct",31)
         , ("Nov",30)
         , ("Dec",31)
         ]

is_leap_year :: Int -> Bool
is_leap_year y = y `rem` 4 == 0 && (not (y `rem` 100 == 0) || y `rem` 400 == 0)



problem_19 = 0
