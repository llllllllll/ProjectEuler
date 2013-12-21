-- 443839 - Completed 5.5.2013.
module ProjectEuler.Problems.Problem30
    ( problem30 -- :: IO ()
    ) where

import ProjectEuler.Utils.List (intToList)

problem30 :: IO ()
problem30 = print
            $ sum [s | s <- [2..999999], ((sum . map (^5)) (intToList s)) == s]
