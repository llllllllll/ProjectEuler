-- 510510 - Completed 16.5.2013 - Learned Euler Totient (phi(n)).
module ProjectEuler.Problems.Problem69
    ( problem69 -- :: IO ()
    ) where

import Data.Function             (on)
import Data.List                 (sortBy)
import ProjectEuler.Utils.Number (eulerTotient)

problem69 :: IO ()
problem69 =  print $ last $ sortBy (compare `on` snd)
             [(n, (fromIntegral n) / (fromIntegral . eulerTotient) n)
                  | n <- [2..1000000]]
