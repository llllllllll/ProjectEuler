-- 510510 - Completed 16.5.2013 - Learned Euler Totient (phi(n)).
module Problems.Problem_69
    ( problem_69
    ) where

import Data.Function
import Data.List
import Utils.Misc

problem_69 =  last $ sortBy (compare `on` snd) 
              [(n, (fromIntegral n) / (fromIntegral . euler_totient) n) | 
               n <- [2..1000000]]