-- NOT COMPLETED.
module Problems.Problem277
    ( problem277
    ) where

import Data.List

problem277 = nums  {-[(n, modCol (n,"")) | n <- nums, 
                    "UDDDUdddDDUDDddDdDddDDUDDdUUDd" `isPrefixOf` 
                    (modCol (n, "")]-}
    where
	nums = head [n | n <- [10000..], isValid n 0]
        isValid n c
            | c == 30 = True
            | c `elem` [0,4,10,22,26,27] 
              && ((4*n+2)`div`3) `rem` 3 == 1 = 
                  isValid ((4*n+2)`div`3) (c+1)
            | c `elem` [1,2,3,8,9,11,12,15,17,20,21,23,24,28] 
              && (n`div`3) `rem` 3 == 0 = isValid (n`div`3) (c+1)
            | c `elem` [5,6,7,13,14,16,18,19,25,29] 
              && ((2*n-1)`div`3) `rem` 3 == 2 = 
                  isValid ((2*n-1)`div`3) (c+1)
            | otherwise = False 