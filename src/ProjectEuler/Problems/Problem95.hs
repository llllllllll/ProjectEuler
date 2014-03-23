-- NOT COMPLETED.
module ProjectEuler.Problems.Problem95
    ( problem95
    , aChain
    ) where

import Data.List (sortBy)
import Data.Function (on)
import Data.Set                  (empty,insert,member,size)
import ProjectEuler.Utils.Number (pDivisors)

problem95 = map aChain [100..1000000]

aChain n = let n' = sum . pDivisors $ n
           in aChain' (sum . pDivisors $ n') n [n',n]
aChain' n s rs@(r:_)
    | n > 1000000 = []
    | n == s      = rs
    | n == r      = []
    | otherwise   = aChain' (sum . pDivisors $ n) s (n:rs)
