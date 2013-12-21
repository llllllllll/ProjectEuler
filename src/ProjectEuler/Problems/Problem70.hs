-- NOT COMPLETED.
module Problems.Problem70
    ( problem70
    ) where

import Data.List (sort)
import Data.Ratio
import Utils.Number (eulerTotient)

problem70 = phiPerm 2 1000
    where
	phiPerm n m
	    | n >= 10^7 = m
	    | (sort . show) n == (sort . show . eulerTotient) n 
              && n%eulerTotient n < m%eulerTotient m = 
                  phiPerm (n+1) n
	    | (sort . show) n == (sort . show . eulerTotient) n 
              && n%eulerTotient n > m% eulerTotient m = 
                  phiPerm (n+1) m
	    | otherwise = phiPerm (n+1) m
 