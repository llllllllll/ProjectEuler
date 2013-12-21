-- 137846528820 - Completed 17.5.2013 - Learned about binomial coefficients
module ProjectEuler.Problems.Problem15
    ( problem15 -- :: IO ()
    ) where

import ProjectEuler.Utils.Number (nCr)

problem15 :: IO ()
problem15 = print $ 40 `nCr` 20
