-- NOT COMPLETED.
module Problems.Problem73 
    ( problem73
    ) where

import Data.Ratio
import Utils.Sequence

problem73 = fSeq (1%3) (1%2) 12000 []

fSeq a b p ns
    | fareySeqTerm (0%1) b p == a = ns
    | otherwise = fSeq a (fareySeqTerm (0%1) b p) p 
                  ((fareySeqTerm (0%1) b p):ns) 