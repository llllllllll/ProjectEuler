-- 5777 - Completed 2.6.2013.
module ProjectEuler.Problems.Problem46
    ( problem46 -- :: IO ()
    ) where

import ProjectEuler.Utils.Prime (primes,isPrime)

problem46 :: IO ()
problem46 = print $ (fst . head)
            [(n,[(a,b)| a <- takeWhile (<=n) primes
                , b <- takeWhile (<=(n - a) `div` 2) squares
                , a + 2 * b == n]) | n <- oddComposites
            , length [(a,b)| a <- takeWhile (<= n) primes
                     , b <- takeWhile (<= (n - a) `div` 2) squares
                     , a + 2 * b == n] == 0 && (not . isPrime) n]
  where
      squares = map (^2) [1..]
      oddComposites = [n | n <- [3,5..]]
