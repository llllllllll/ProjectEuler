-- 1.002322108633 (Paper/Pencil)- Completed 11.5.2013
module ProjectEuler.Problems.Problem235
    ( problem235 -- :: IO ()
    ) where

import ProjectEuler.Utils.Misc (bisectionSearch)

problem235 = print $ bisectionSearch f (0-600000000000) (1,1.5) 0.000000000001
    where
	f r = sum [(900-3*k)*r**(k-1) | k <- [1..5000]]
