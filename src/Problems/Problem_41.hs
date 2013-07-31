-- 7652413 - Completed 24.5.2013 - WARNING: Does not terminate!
module Problems.Problem_41
    ( problem_41
    ) where

import Utils.Prime
import Utils.Misc

problem_41 = [n | n <- primes, is_pandigitalr (1,(length . show) n) n]