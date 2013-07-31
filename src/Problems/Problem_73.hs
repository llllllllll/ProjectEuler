-- NOT COMPLETED.
module Problems.Problem_73 
    ( problem_73
    ) where

import Data.Ratio
import Utils.Sequence

problem_73 = f_seq (1%3) (1%2) 12000 []

f_seq a b p ns
    | farey_seq_term (0%1) b p == a = ns
    | otherwise = f_seq a (farey_seq_term (0%1) b p) p 
                  ((farey_seq_term (0%1) b p):ns)