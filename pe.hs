module ProjectEuler where 
import Data.List
import Data.Bits
import Data.Char

--Lists--

{- An infinite list of prime numbers -}
primes :: [Integer]
primes = 2: 3: sieve (tail primes) [5,7..]
	where 
  		sieve (p:ps) xs = h ++ sieve ps [x | x <- t, x `rem` p /= 0]  
  			where (h,~(_:t)) = span (< p*p) xs

{- A list of all divisors of n -}
divisors :: Integral a => a -> [a]
divisors n = [x | x <- takeWhile (<=ceiling ((fromIntegral n)/2)) [1..], n `mod` x == 0]

{- A list of all divisors of n in reverse order -}
r_divisors :: Integral a => a -> [a]
r_divisors n = [x | x <- [n, n-1..1], n `mod` x == 0]

--Sequences--

{- Fibonacci Sequence starting at n -}
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

{- Collatz Sequence starting at n -}
collatz :: Integral a => a -> [a]
collatz 1 = [1]
collatz n
	| even n = n:collatz (n `div` 2)
	| odd n = n:collatz (n * 3 + 1)

--Functions--

{- factorial n = n! -}
factorial n = product [1..n]

{- The number of divisors of n -}
num_divisors n = (length . divisors) n

{- Bool whether n is a prime number or not -}
is_prime n = n == (head $ primes \\ (takeWhile (<n) primes))

{- convert an Integral to a list of its digits. eg: int_to_list 123 = [1,2,3] -}
int_to_list n = map digitToInt $ show n

{- Inverse of int_to_list. Convert a list into an Integral where each element is a digit. eg: list_to_int [1,2,3] = 123 -}
list_to_int ns = read (foldl (++) "" (map (show) ns)) :: Integer

{- Returns a Bool whether a number is neither increasing or decreasing -}
is_bouncy n = not (is_increasing (int_to_list n) || is_decreasing (int_to_list n))
{- Returns a Bool as to whether a number is increasing. eg: is_increasing 1234 = True, is_increasing 4321 = False -}
is_increasing n = (sort . show) n == (show n)
{- Returns a Bool as to whether a number is decreasing. eg: is_increasing 1234 = False, is_increasing 4321 = True -}
is_decreasing n = (reverse . sort . show) n == (show n)

{- Returns if n is pandigital with bounds a..b -}
is_pandigital a b n = (nub . sort . int_to_list) n == (sort [a..b])

{- Returns the first d digits of n -}
head_n d n = list_to_int $ take d (int_to_list n)
{- Returns the last d digits of n -}
end_n d n = reverse $ take d $ (reverse . show) n

{- Number of combinations of n choose r -}
n `nCr` r = factorial n / (factorial r * factorial (n-r))

{- returns the number of times n occurs in list ns -}
elem_count n ns =  (length . nubBy (/=)) (n:ns) - 1


--Problems--

{- 233168 - Completed 29.4.2013 -}
problem_1 = sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]
problem_1' = (sum . nub) $ union [3,6..999] [5,10..995]

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

problem_12 = head [tri_num x | x <- [1..], (num_divisors . tri_num) x > 500]
	where
		tri_num t = sum [1..t]

{- 837799 - Completed 29.4.2013 -}
problem_14 =  head (head [collatz x | x <- [999999,999998..1], (length (collatz x))  == maximum [length (collatz x) | x <- [1..999999]]])

{- 648 - Completed 11.5.2013 -}
problem_20 = (sum . int_to_list) $ factorial 100

{- 31725 - Completed 12.5.2013 -}
problem_21 = sum [a | a <- [1..9999], is_amicable a]
	where
		is_amicable :: Integral a => a -> Bool
		is_amicable a = (sum . divisors) ((sum . divisors) a) == a && a /= (sum . divisors) a


{- 871198282 - Completed 5.5.2013 -}
problem_22 = sum $ map (\n -> raw_score n * pos_mod n) names
	where
		names = [""] --Names.txt can be found on the website, I just ommited to clean it up.
		raw_score n = (sum . map char_pos) n
		char_pos c = (fromEnum c) - (fromEnum 'A') + 1
		pos_mod n = length (takeWhile (/=n) names) + 1

{- 2783915460 - Completed 5.5.2013 -}
problem_24 =  (sort . permutations) ['0'..'9'] !! 999999

problem_25 = head [x | x <- [1..], (length . show . fib) x == 1000]

{- 9183 - Completed 3.5.2013 -}
problem_29 = length $ nub [a^b | a <- [2..100], b <- [2..100]] 

{- 443839 - Completed 5.5.2013 -}
problem_30 = sum [s | s <- [2..999999], ((sum . map (^5)) (int_to_list s)) == s]

{- 40730 - Completed 5.5.2013 -}
problem_34 = sum [x | x <- [3..99999], is_curious x]
	where
		is_curious n = (sum . map factorial) (int_to_list n) == n

problem_35 = [1 | n <- takeWhile (<1000000) primes, is_valid n]
	where
		is_valid n = foldl (&&) (True) $ map (is_prime . list_to_int) ((circulate . int_to_list) n)
		circulate ns = init (zipWith (++) (tails ns) (inits ns))

problem_40 = length $ show (list_to_int [1..10000])

{- 9110846700 - Completed 5.5.2013 -}
problem_48 = end_n 10 (sum (map (\x -> x^x) [1..1000]))

{- 142857 - Completed 5.5.2013 -}
problem_52 = head [n | n <- [1..], is_valid n]
	where
		is_valid n = (sort . int_to_list) n == ((nub . sort . concat . map (\x -> int_to_list (x*n))) [2..6])

{- 4075 - Completed 8.5.2013 -}
problem_53 = genericLength [1 | n <- [1..100], r <- [1..n], n `nCr` r > 1000000]

{- 972 - Completed 10.5.2013 -}
problem_56 = maximum $ [(sum . int_to_list) (a^b) | a <- [1..99], b <- [1..99]]

{- 8739992577 - Completed 8.5.2013 -}
problem_97 = end_n 10 $ 28433*2^(7830457)+1

problem_104 = head $ filter (\x -> is_pandigital 1 9 (head_n 9 x) && is_pandigital 1 9 (end_n 9 x)) (map (fib) [1..])

problem_112 = takeWhile (\x -> p_bouncy x < 0.99) [21700..]
	where
		p_bouncy n = fromIntegral (length (filter (is_bouncy) [1..n])) / (fromIntegral n)

{- 1.002322108633 (Paper/Pencil)- Completed 11.5.2013 -}
problem_235 = error "Not Done In Haskell"

problem_371 =  [(n-1)*p | n <- [1..10]]
	where
		p = 999/1000000