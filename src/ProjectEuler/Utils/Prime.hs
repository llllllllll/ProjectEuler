-- |
-- Module      : ProjectEuler.Utils.Prime
-- Copyright   : Joe Jevnik 2013
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- Operations of primes and checking primality..

module ProjectEuler.Utils.Prime
    ( primes           -- :: [Integer]
    , primeFactors     -- :: Integer -> [Integer]
    , primeFactorsMult -- :: Integer -> [(Integer, Int)]
    , isPrime          -- :: Integral a => a -> Bool
    , isPrime'         -- :: Integral a => a -> Bool
    ) where

import Data.Bits (Bits(..),shift)
import Data.List (group)

-- | An infinite list of prime numbers.
primes :: [Integer]
primes = 2: 3: sieve (tail primes) [5,7..]
    where
        sieve (p:ps) xs = n ++ sieve ps [x | x <- ns, x `rem` p /= 0]
            where (n,~(_:ns)) = span (< p*p) xs

-- | The prime factors of n.
primeFactors :: Integer -> [Integer]
primeFactors n = factor primes n
    where
        factor ps@(p:pt) n
            | p * p > n    = [n]
            | rem n p == 0 = p : factor ps (quot n p)
            | otherwise    =     factor pt n

primeFactorsMult :: Integer -> [(Integer, Int)]
primeFactorsMult = map f . group . primeFactors
    where f xs = (head xs, length xs)

-- | A Bool representing the primality of n.
isPrime :: Integral a => a -> Bool
isPrime n = isPrime' n (floor $ sqrt $ fromIntegral n)
    where
        isPrime' n i
            | i == 1 && n > 1    = True
            | n == i*(n `div` i) = False
            | otherwise = isPrime' n (i - 1)

-- | An attempt at fermats little theorem.
isPrime' :: Integral a => a -> Bool
isPrime' p = gcd (sum [a^(p-1) | a <- [1..p-1]]) p == 1

-- | NOT EXPORTED: Added to alleviate recursive imports.
modExp :: (Integral a, Bits a) => a -> a -> a -> a
modExp a b c = expMod' a b c 1
  where
      expMod' a b c s
          | b == 0    = s
          | odd b     = expMod' (a^2 `rem` c) (shift b (0-1)) c ((s * a) `mod` c)
          | otherwise = expMod' (a^2 `rem` c) (shift b (0-1)) c s

-- | NOT EXPORTED: Added to alleviate recursive imports.
eulerTotient :: Integer -> Integer
eulerTotient 1 = 1
eulerTotient m = product [(p - 1) * p ^ (c - 1) | (p, c) <- primeFactorsMult m]
