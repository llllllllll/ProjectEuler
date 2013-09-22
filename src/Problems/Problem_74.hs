-- 402 - Completed 17.5.2013 - Learned about Factorial Chains.
module Problems.Problem_74
    ( problem_74
    ) where

import Utils.List (int_to_list)
import Utils.Misc (factorial)

problem_74 = length [f_chain n| n <- [1..999999], is_valid n]
    where
	is_valid n = f_chain n == 60
	f_chain n = f_chain' n []
	f_chain' n ns
           | n `elem` ns = length ns
	    | otherwise = f_chain' ((sum . map factorial) 
                                    (int_to_list n)) (n:ns)
