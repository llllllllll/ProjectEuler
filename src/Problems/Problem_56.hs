-- 972 - Completed 10.5.2013.
module Problems.Problem_56
    ( problem_56
    ) where

import Utils.List (int_to_list)

problem_56 = maximum $ [(sum . int_to_list) (a^b) | a <- [1..99], 
                        b <- [1..99]]
