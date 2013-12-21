-- NOT COMPLETE.
module ProjectEuler.Problems.Problem23
    ( problem23 -- :: IO ()
    ) where

import ProjectEuler.Utils.Number (divisors)

problem23 :: IO ()
problem23 = print $ [a + b | a <- abundants, b <- takeWhile (<= a) abundants
                    , a + b < 28123]
    where
	abundants = filter isAbundant [12..28111]
	isAbundant n = isAbundant' n (divisors n) 0
	isAbundant' k (n:ns) c
	    | null ns = n + c > k
	    | n + c > k = True
	    | otherwise = isAbundant' k ns (c + n)
