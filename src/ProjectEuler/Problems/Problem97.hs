-- 8739992577 - Completed 8.5.2013.
module ProjectEuler.Problems.Problem97
    ( problem97 -- :: IO ()
    ) where

import ProjectEuler.Utils.List (lastN)

problem97 :: IO ()
problem97 = print $ lastN 10 $ 28433 * 2^(7830457) + 1
