-- 55 - Completed 23.5.2013.
module ProjectEuler.Problems.Problem35
    ( problem35 -- :: IO ()
    ) where

import Data.List                (takeWhile,zipWith,inits,tails)
import ProjectEuler.Utils.Prime (isPrime,primes)
import ProjectEuler.Utils.List  (listToInt,intToList)

problem35 :: IO ()
problem35 = print $ length [n | n <- takeWhile (<1000000) primes, isValid n]
    where
	circulate ns = init (zipWith (++) (tails ns) (inits ns))
	isValid n    = all (isPrime . listToInt) $ circulate (intToList n)
