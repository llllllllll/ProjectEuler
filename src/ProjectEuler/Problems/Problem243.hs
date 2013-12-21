-- NOT COMPLETED.
module Problems.Problem243
    ( problem243
    ) where

import Data.Ratio
import Utils.Number (eulerTotient)

problem243 = head nums
    where
	nums = [n+1 | n <- [94744, 2*94744..], 
                (eulerTotient (n+1))%n < 15499%94744]
 