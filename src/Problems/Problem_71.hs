-- 428572 - Completed 17.5.2013 - Used Farey Sequences learned in problem 72.
module Problems.Problem_71
    ( problem_71
    ) where

import Data.Ratio
import Utils.Sequence

problem_71 = farey_seq_term (0%1) (3%7) (10^6)