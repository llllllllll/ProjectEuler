-- NOT COMPLETED.
module Problems.Problem171
    ( problem171
    ) where

import Utils.List (intToList)
import Utils.Number (isSquare)

problem171 = [n | n <- [1..10^20-1], isSquare (f n)]
    where
	f n = sum $ map (^2) (intToList n)
 