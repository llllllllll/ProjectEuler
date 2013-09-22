-- 21124 - Completed 22.9.2013
module Problems.Problem_17
    ( problem_17
    ) where

import Utils.Misc (show_engl)

problem_17 = sum $ map (length . filter (/= ' ') . show_engl) [1..1000]
