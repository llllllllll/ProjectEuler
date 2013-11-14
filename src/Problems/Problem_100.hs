-- NOT YET COMPLETED.
module Problems.Problem_100
    ( problem_100
    ) where

import Data.Ratio

problem_100 = [n % d | d <- [1000000000000..], n <- [d `rem` 2..d]
              , (n % d) * ((n - 1) % (d - 1)) == 1 % 2]
