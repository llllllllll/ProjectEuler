module Utils.Prime 
    ( primes
    , prime_factors
    , prime_factors_mult
    , is_prime 
    ) where

import Data.List    

-- An infinite list of prime numbers.
primes :: [Integer]
primes = 2: 3: sieve (tail primes) [5,7..]
    where 
        sieve (p:ps) xs = n ++ sieve ps [x | x <- ns, 
                                                  x `rem` p /= 0]  
            where (n,~(_:ns)) = span (< p*p) xs
                          
-- The prime factors of n.
prime_factors n = factor primes n
    where 
        factor ps@(p:pt) n 
            | p*p > n      = [n]               
            | rem n p == 0 = p : factor ps (quot n p) 
            | otherwise    =     factor pt n
                                 
prime_factors_mult = map encode . group . prime_factors
    where encode xs = (head xs, length xs)
                    
-- A Bool representing the primality of n.
is_prime :: Integral a => a -> Bool
is_prime n = is_prime' n (floor $ sqrt $ fromIntegral n)
    where 
        is_prime' n i
            | i == 1 && n > 1 = True
            | n == i*(n `div` i) = False
            | otherwise = is_prime' n (i-1)
                          