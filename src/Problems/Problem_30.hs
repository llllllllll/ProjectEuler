-- 443839 - Completed 5.5.2013.
module Problems.Problem_30
    ( problem_30
    ) where

import Utils.List

problem_30 = sum [s | s <- [2..999999], ((sum . map (^5)) (int_to_list s)) == s]