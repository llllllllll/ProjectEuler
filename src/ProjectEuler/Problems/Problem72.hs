-- 303963552391 - Completed 17.5.2013 - Learned Farey Sequences.
module ProjectEuler.Problems.Problem72
    ( problem72 -- :: IO ()
    ) where

import ProjectEuler.Utils.Sequence (fareySeqLength)

problem72 :: IO ()
problem72 = print $ fareySeqLength 1000000
