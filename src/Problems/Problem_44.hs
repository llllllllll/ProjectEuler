
module Problems.Problem_44
    ( problem_44
    ) where

import Utils.Misc

problem_44 = [pent_num a - pent_num b 
             | a <- [1..5000], b <- [1..a], 
               is_pent (pent_num a - pent_num b) 
               && is_pent (pent_num a + pent_num b)]
    where
	is_pent n = is_int ((1/6)*(1-sqrt (fromIntegral (24*n+1)))) 
                    || is_int ((1/6)*(1+sqrt (fromIntegral (24*n+1))))
	pent_num = ((map (\n -> n*(3*n-1)`div`2) [1..])!!)