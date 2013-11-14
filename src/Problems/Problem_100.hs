-- NOT YET COMPLETED.
module Problems.Problem_100
    ( problem_100
    ) where

import Data.Ratio
import Utils.Number

problem_100 =  head [(n,d,round (n^2 - n) % round (d^2 - d)) | d <- [10^12..]
                   , let n = (1 + sqrt (4*((d^2 - d) / 2) + 1)) / 2, is_int n]
                             -- && (n^2 - n) / (d^2 - d) == 1 / 2]
