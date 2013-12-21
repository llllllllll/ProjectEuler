-- 428572 - Completed 17.5.2013 - Used Farey Sequences learned in problem 72.
module ProjectEuler.Problems.Problem71
    ( problem71 -- :: IO ()
    ) where

import Data.Ratio                  ((%))
import ProjectEuler.Utils.Sequence (fareySeqTerm)

problem71 :: IO ()
problem71 = print $ fareySeqTerm (0 % 1) (3 % 7) (10^6)
