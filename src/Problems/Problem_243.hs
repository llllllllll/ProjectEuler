-- NOT COMPLETED.
module Problems.Problem_243
    ( problem_243
    ) where

import Data.Ratio
import Utils.Number (euler_totient)

problem_243 = head nums
    where
	nums = [n+1 | n <- [94744, 2*94744..], 
                (euler_totient (n+1))%n < 15499%94744]
