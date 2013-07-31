module Utils.Sequence
    ( collatz
    , collatz_mem
    , fib
    , slow_fib
    , farey_seq_term
    , farey_seq_length
    ) where

import Data.Ratio
import Utils.Misc

-- Collatz Sequence starting at n.
collatz 1 = []
collatz n
    | even n = n:collatz (n`div`2)
    | odd n = n:collatz (n*3+1)
              
-- Memoized Collatz Sequence starting at n.
collatz_mem = ((map c [0..])!!)
    where
	c 1 = [1]
	c n
            | even n = n:collatz_mem (n`div`2)
            | odd n = n:collatz_mem (n*3+1)

-- nth term of the Fibonacci sequence.
fib = ((map f [0..])!!)
    where
	f 0 = 0
	f 1 = 1
	f n = fib (n - 2) + fib (n - 1)
       
-- A Fibbonacci term without memoization.
slow_fib :: Int -> Int     
slow_fib 0 = 0
slow_fib 1 = 1
slow_fib n = slow_fib (n-2) + slow_fib (n-1)

{-Farey Sequence term to the left of b with a power of p-}
farey_seq_term a b p
    |da2 <= p = farey_seq_term a1 b p
    |otherwise = na % nb
    where
    	na = numerator a
    	nb = numerator b
    	da = denominator a
        db = denominator b
    	a1 = (na + nb) % (da + db)
    	da2 = denominator a1

-- The length of the farey Sequence of order n.
farey_seq_length n = (sum . map euler_totient) [1..n]