-- NOT COMPLETED.
module Problems.Problem216
    ( problem216
    ) where

import Utils.Prime

problem216 = [2*n^2-1 | n <- [2..50000000], isPrime (n*n^2-1)] 