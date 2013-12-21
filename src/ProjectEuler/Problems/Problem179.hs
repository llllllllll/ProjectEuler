-- NOT COMPLETED.
module Problems.Problem179
    ( problem179
    ) where

import Utils.Misc

problem179 = [n | n <- [1..10^7-1], numDivisors n == numDivisors (n+1)] 