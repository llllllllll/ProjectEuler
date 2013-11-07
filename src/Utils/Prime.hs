module Utils.Prime
    ( primes
    , prime_factors
    , prime_factors_mult
    , is_prime
    , is_prime'
    ) where

import Data.List (group)
import Data.Bits (Bits(..),shift)

-- An infinite list of prime numbers.
primes :: [Integer]
primes = 2: 3: sieve (tail primes) [5,7..]
    where
        sieve (p:ps) xs = n ++ sieve ps [x | x <- ns,
                                                  x `rem` p /= 0]
            where (n,~(_:ns)) = span (< p*p) xs

-- The prime factors of n.
prime_factors :: Integer -> [Integer]
prime_factors n = factor primes n
    where
        factor ps@(p:pt) n
            | p * p > n    = [n]
            | rem n p == 0 = p : factor ps (quot n p)
            | otherwise    =     factor pt n

prime_factors_mult :: Integer -> [(Integer, Int)]
prime_factors_mult = map f . group . prime_factors
    where f xs = (head xs, length xs)

-- A Bool representing the primality of n.
is_prime :: Integral a => a -> Bool
is_prime n = is_prime' n (floor $ sqrt $ fromIntegral n)
    where
        is_prime' n i
            | i == 1 && n > 1    = True
            | n == i*(n `div` i) = False
            | otherwise = is_prime' n (i - 1)

is_prime' p = gcd (sum [a^(p-1) | a <- [1..p-1]]) p == 1

mod_exp :: (Integral a, Bits a) => a -> a -> a -> a
mod_exp a b c = exp_mod' a b c 1
  where
      exp_mod' a b c s
          | b == 0    = s
          | odd b     = exp_mod' (a^2 `rem` c) (shift b (0-1)) c ((s * a) `mod` c)
          | otherwise = exp_mod' (a^2 `rem` c) (shift b (0-1)) c s

euler_totient m = product
                  [(p - 1) * p ^ (c - 1) | (p, c) <- prime_factors_mult m]
