-- 31725 - Completed 12.5.2013 - Started working on typing.
module Problems.Problem_21
    ( problem_21
    ) where

import Utils.Misc

problem_21 = sum [a | a <- [1..9999], is_amicable a]
    where
	is_amicable :: Integral a => a -> Bool
	is_amicable a = (sum . divisors) ((sum . divisors) a) == a 
                        && a /= (sum . divisors) a