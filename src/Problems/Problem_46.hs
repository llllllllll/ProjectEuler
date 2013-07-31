-- 5777 - Completed 2.6.2013.
module Problems.Problem_46
    ( problem_46
    ) where

import Utils.Prime

problem_46 = (fst . head) [(n,[(a,b)| a <- takeWhile (<=n) primes, 
                               b <- takeWhile (<=(n-a)`div`2) squares, 
                               a+2*b == n]) | n <- odd_composites, 
                           length [(a,b)| a <- takeWhile (<=n) primes, 
                                   b <- takeWhile (<=(n-a)`div`2) squares, 
                                   a+2*b == n] == 0 && (not . is_prime) n]
    where
	squares = map (^2) [1..]
	odd_composites = [n | n <- [3,5..]]