-- 31725 - Completed 12.5.2013 - Started working on typing.
module ProjectEuler.Problems.Problem21
    ( problem21 -- IO ()
    ) where

import ProjectEuler.Utils.Number (divisors)

problem21 :: IO ()
problem21 = print $ sum [a | a <- [1..9999], isAmicable a]
    where
	isAmicable :: Integral a => a -> Bool
	isAmicable a = (sum . divisors) ((sum . divisors) a) == a
                       && a /= (sum . divisors) a
