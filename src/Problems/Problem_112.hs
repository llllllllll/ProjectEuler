-- 1587000 - Completed 22.5.2013.
module Problems.Problem_112
    ( problem_112
    ) where

import Data.Ratio ((%))
import Utils.Misc (is_bouncy)

problem_112 = p 21780 19602
    where
	p n a
            | a%n >= (99%100) = n
            | is_bouncy n = p (n+1) (a+1)
            | otherwise = p (n+1) a
