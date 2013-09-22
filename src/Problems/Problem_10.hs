-- 142913828922 - Completed 29.4.2013.
module Problems.Problem_10
    ( problem_10
    ) where

import Utils.Prime (primes)

problem_10 = sum $ takeWhile (<2000000) primes
