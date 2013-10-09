-- NOT COMPLETED.
module Problems.Problem_70
    ( problem_70
    ) where

import Data.List (sort)
import Data.Ratio
import Utils.Number (euler_totient)

problem_70 = phi_perm 2 1000
    where
	phi_perm n m
	    | n >= 10^7 = m
	    | (sort . show) n == (sort . show . euler_totient) n 
              && n%euler_totient n < m%euler_totient m = 
                  phi_perm (n+1) n
	    | (sort . show) n == (sort . show . euler_totient) n 
              && n%euler_totient n > m% euler_totient m = 
                  phi_perm (n+1) m
	    | otherwise = phi_perm (n+1) m
