-- 6857 - Completed 17.5.2013 - Forgot to fix after new prime_factors was done.
module Problems.Problem_3
    ( problem_3
    ) where

import Utils.Prime (prime_factors)

problem_3 = last $ prime_factors 600851475143
