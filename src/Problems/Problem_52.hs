-- 142857 - Completed 5.5.2013.
module Problems.Problem_52
    ( problem_52
    ) where

import Data.List
import Utils.List

problem_52 = head [n | n <- [1..], is_valid n]
    where
	is_valid n = (sort . int_to_list) n 
                     == ((nub . sort . concat . map 
                          (\x -> int_to_list (x*n))) [2..6])