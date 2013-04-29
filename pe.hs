module ProjectEuler where 

-- Generic Functions

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

primes :: [Integer]
primes = 2: 3: sieve (tail primes) [5,7..]
	where 
  		sieve (p:ps) xs = h ++ sieve ps [x | x <- t, x `rem` p /= 0]  
  			where (h,~(_:t)) = span (< p*p) xs

-- Problems 

{- 233168 - Completed 29.4.2013 -}
problem_1 = sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]

{- 4613732 - Completed 29.4.2013 -}
problem_2 = sum [x | x <- takeWhile (<4000000) (map fib [2..]), even x]


problem_3 = error "Not Completed In Haskell"

{- 906609 - Completed 29.4.2013 -}
problem_4 = maximum [x*y | x <- [100..999], y <- [100..999], reverse (show (x*y)) == show (x*y)]

{- 232792560 - Completed 29.4.2013 -}
problem_5 = foldl (lcm) 1 [1..20]

{- 25164150 - Completed 29.4.2013 -}
problem_6 = (sum [1..100]) ^ 2 - sum (map (^2) [1..100])

{- 104743 - Completed 29.4.2013 -}
problem_7 = primes !! 10001

problem_8 = error "Not Completed In Haskell"

problem_9 = error "Not Completed In Haskell"

{- 142913828922 - Completed 29.4.2013 -}
problem_10 = sum $ takeWhile (<2000000) primes

problem_11 = error "Not Completed In Haskell"

problem_12 = head [triNum x | x <- [1..], numDivisiors x > 500]
	where
		triNum t = sum [1..t]
		numDivisiors n = length [1 | x <- [1..n], x `mod` n == 0]