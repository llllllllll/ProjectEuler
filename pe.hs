module ProjectEuler where 
import Data.List
import Data.Bits
import Data.Char
import Data.Function
import Data.Array
import Data.Ratio

--Lists--

{-An infinite list of prime numbers-}
primes :: [Integer]
primes = 2: 3: sieve (tail primes) [5,7..]
	where 
  		sieve (p:ps) xs = h ++ sieve ps [x | x <- t, x `rem` p /= 0]  
  			where (h,~(_:t)) = span (< p*p) xs

{-A list of all divisors of n-}
divisors :: Integral a => a -> [a]
divisors n = [x | x <- takeWhile (<=ceiling ((fromIntegral n)/2)) [1..], n `mod` x == 0]

{-A list of all divisors of n in reverse order-}
r_divisors :: Integral a => a -> [a]
r_divisors n = [x | x <- [n, n-1..1], n `mod` x == 0]

{-The primes factors of n-}
prime_factors n = factor primes n
  where 
    factor ps@(p:pt) n | p*p > n      = [n]               
                       | rem n p == 0 = p : factor ps (quot n p) 
                       | otherwise    =     factor pt n
prime_factors_mult = map encode . group . prime_factors
    where encode xs = (head xs, length xs)

{-Euler's number-}
e = exp 1

{-Champernown's constant-}
champernowne = concatMap show [0..]

--Sequences--

{-Fibonacci Sequence starting at n-}
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

{-Collatz Sequence starting at n-}
collatz :: Integral a => a -> [a]
collatz 1 = [1]
collatz n
	| even n = n:collatz (n `div` 2)
	| odd n = n:collatz (n * 3 + 1)

--Functions--

{-factorial n = n!-}
factorial n = product [1..n]

{-The number of divisors of n-}
num_divisors n = (length . divisors) n

{-Bool whether n is a prime number or not-}
is_prime n = n == (head $ primes \\ (takeWhile (<n) primes))

{-convert an Integral to a list of its digits. eg: int_to_list 123 = [1,2,3]-}
int_to_list n = map digitToInt $ show n

{-Inverse of int_to_list. Convert a list into an Integral where each element is a digit. eg: list_to_int [1,2,3] = 123-}
list_to_int ns = read $ concatMap show ns :: Integer

{-Returns a Bool whether a number is neither increasing or decreasing-}
is_bouncy n = not (is_increasing (int_to_list n) || is_decreasing (int_to_list n))
{-Returns a Bool as to whether a number is increasing. eg: is_increasing 1234 = True, is_increasing 4321 = False-}
is_increasing n = (sort . show) n == (show n)
{-Returns a Bool as to whether a number is decreasing. eg: is_increasing 1234 = False, is_increasing 4321 = True-}
is_decreasing n = (reverse . sort . show) n == (show n)

{-Returns if n is pandigital with bounds a..b-}
is_pandigital a b n = (nub . sort . int_to_list) n == [a..b]
is_pandigital' a b n = (sort . int_to_list) n == [a..b]

{-Returns the first d digits of n-}
head_n d n = list_to_int $ take d (int_to_list n)
{-Returns the last d digits of n-}
last_n d n = reverse $ take d $ (reverse . show) n

{-Number of combinations of n choose r-}
n `nCr` r = factorial n / (factorial r * factorial (n-r))
n `nPr` r = factorial n / factorial (n-r)

{-returns the number of times n occurs in list ns-}
elem_count n ns =  elem_count' n ns 0
	where
	elem_count' n ns c
		| null ns = c
		| head ns == n = elem_count' n (tail ns) (c+1)
		| otherwise = elem_count' n (tail ns) c

{-Returns if n is an Integral to 7 decimal places-}
is_int :: RealFrac a => a -> Bool
is_int n = (round $ 10^(fromIntegral 7)*(n-(fromIntegral $ round n))) == 0

{-Returns True if n is a perfect square-}
is_square n = is_int (fromIntegral (n) **(0.5))

{-The size of the list of numbers coprime to n-}
euler_totient m = product [(p - 1) * p ^ (c - 1) | (p, c) <- prime_factors_mult m]

{-Farey Sequence term to the left of b with a power of p-}
farey_seq_term a b p
    |da2<=p=farey_seq_term a1 b p
    |otherwise=na%nb
    where
    	na=numerator a
    	nb=numerator b
    	da=denominator a
    	db=denominator b
    	a1=(na+nb)%(da+db)
    	da2=denominator a1

{-The length of the farey Sequence of order n.-}
farey_seq_length n = (sum . map euler_totient) [1..n]

{-Detirmines if a number is a lychrel number, a number that does not converge to a palindrome-}
is_lychrel n = is_lychrel' n 0
is_lychrel' n c
	| (reverse . show) n == show n && c /= 0 = False
	| c == 50 = True
	| otherwise = is_lychrel' (list_to_int ((reverse . int_to_list) n) + n) (c+1)

{-A Binary Search for arrays-}
binary_search n arr = binary_search' n arr 0 (a_length arr)
binary_search' n arr a b
	| arr ! c == n = c
	| arr ! c > n = binary_search' n arr a (c-1)
	| arr ! c < n = binary_search' n arr (c+1) b
	| otherwise = 0-1
	where c = floor (fromIntegral (a + b) / 2)

bisection_search f n (a,b) eps
	| f a > f b = b_search_dec f n (a,b) eps
	|otherwise = b_search_inc f n (a,b) eps
b_search_inc f n (a,b) eps
	| f c == n || (b-a)/2 < eps = c
	| f c > n = b_search_inc f n (a, c) eps
	| otherwise = b_search_inc f n (c, b) eps
	where c = (a+b)/2.0
b_search_dec f n (a,b) eps
	| f c == n || (b-a)/2 < eps = c
	| f c < n = b_search_dec f n (a, c) eps
	| otherwise = b_search_dec f n (c, b) eps
	where c = (a+b)/2.0

a_length arr = (snd . bounds) arr - (fst . bounds) arr

split_on     :: (Char -> Bool) -> String -> [String]
split_on p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : split_on p s''
                            where (w, s'') = break p s'

--Problems--

{-233168 - Completed 29.4.2013-}
problem_1 = sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]
problem_1' = (sum . nub) $ union [3,6..999] [5,10..995]

{-4613732 - Completed 29.4.2013 - Learned list comprehensions-}
problem_2 = sum [x | x <- takeWhile (<4000000) (map fib [2..]), even x]

{-6857 - Completed 17.5.2013 - Forgot to fix after new prime_factors was written.-}
problem_3 = last $ prime_factors 600851475143

{-906609 - Completed 29.4.2013 - Learned how to use multiple generators.-}
problem_4 = maximum [x*y | x <- [100..999], y <- [100..999], reverse (show (x*y)) == show (x*y)]

{-232792560 - Completed 29.4.2013 - Learned about folds-}
problem_5 = foldl (lcm) 1 [1..20]

{-25164150 - Completed 29.4.2013 - Learned about map-}
problem_6 = (sum [1..100]) ^ 2 - sum (map (^2) [1..100])

{-104743 - Completed 29.4.2013 - Learned how to index a list-}
problem_7 = primes !! 10001

problem_8 = error "Not Completed In Haskell"

problem_9 = error "Not Completed In Haskell"

{-142913828922 - Completed 29.4.2013-}
problem_10 = sum $ takeWhile (<2000000) primes

{-70600674 - Completed 16.5.2013 - Learned arrays and reasoned through 2d arrays-}
problem_11 = maximum [maximum [horizontal r n | r <- [0..19], n <- [0..15]], maximum [vertical r n | r <- [0..15], n <- [0..19]], maximum [d_right r n | r <- [0..15], n <- [0..15]], maximum [d_left r n | r <- [3..15], n <- [3..15]]]
	where 
		horizontal r n = grid!r!n * grid!r!(n+1) * grid!r!(n+2) * grid!r!(n+3)
		vertical r n = grid!r!n * grid!(r+1)!n * grid!(r+2)!n * grid!(r+3)!n
		d_right r n = grid!r!n * grid!(r+1)!(n+1) * grid!(r+2)!(n+2) * grid!(r+3)!(n+3)
		d_left r n = grid!r!n * grid!(r-1)!(n+1) * grid!(r-2)!(n+2) * grid!(r-3)!(n+3)
		grid = listArray (0,19) 
			[listArray (0,19) [08, 02, 22, 97, 38, 15, 00, 40, 00, 75, 04, 05, 07, 78, 52, 12, 50, 77, 91, 08],
			listArray (0,19) [49, 49, 99, 40, 17, 81, 18, 57, 60, 87, 17, 40, 98, 43, 69, 48, 04, 56, 62, 00],
			listArray (0,19) [81, 49, 31, 73, 55, 79, 14, 29, 93, 71, 40, 67, 53, 88, 30, 03, 49, 13, 36, 65],
			listArray (0,19) [52, 70, 95, 23, 04, 60, 11, 42, 69, 24, 68, 56, 01, 32, 56, 71, 37, 02, 36, 91],
			listArray (0,19) [22, 31, 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, 40, 28, 66, 33, 13, 80],
			listArray (0,19) [24, 47, 32, 60, 99, 03, 45, 02, 44, 75, 33, 53, 78, 36, 84, 20, 35, 17, 12, 50],
			listArray (0,19) [32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 66, 18, 38, 64, 70],
			listArray (0,19) [67, 26, 20, 68, 02, 62, 12, 20, 95, 63, 94, 39, 63, 08, 40, 91, 66, 49, 94, 21],
			listArray (0,19) [24, 55, 58, 05, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, 14, 88, 34, 89, 63, 72],
			listArray (0,19) [21, 36, 23, 09, 75, 00, 76, 44, 20, 45, 35, 14, 00, 61, 33, 97, 34, 31, 33, 95],
			listArray (0,19) [78, 17, 53, 28, 22, 75, 31, 67, 15, 94, 03, 80, 04, 62, 16, 14, 09, 53, 56, 92],
			listArray (0,19) [16, 39, 05, 42, 96, 35, 31, 47, 55, 58, 88, 24, 00, 17, 54, 24, 36, 29, 85, 57],
			listArray (0,19) [86, 56, 00, 48, 35, 71, 89, 07, 05, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58],
			listArray (0,19) [19, 80, 81, 68, 05, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77, 04, 89, 55, 40],
			listArray (0,19) [04, 52, 08, 83, 97, 35, 99, 16, 07, 97, 57, 32, 16, 26, 26, 79, 33, 27, 98, 66],
			listArray (0,19) [88, 36, 68, 87, 57, 62, 20, 72, 03, 46, 33, 67, 46, 55, 12, 32, 63, 93, 53, 69],
			listArray (0,19) [04, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18, 08, 46, 29, 32, 40, 62, 76, 36],
			listArray (0,19) [20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, 59, 85, 74, 04, 36, 16],
			listArray (0,19) [20, 73, 35, 29, 78, 31, 90, 01, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57, 05, 54],
			listArray (0,19) [01, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, 52, 01, 89, 19, 67, 48]]

problem_12 = head [tri_num x | x <- [1..], (num_divisors . tri_num) x > 500]
	where
		tri_num t = sum [1..t]

{-5537376230 - Completed 15.5.2013-}
problem_13 = head_n 10 a
	where a = 0 -- Sum all the numbers on the site.

{-837799 - Completed 29.4.2013-}
problem_14 =  head (head [collatz x | x <- [999999,999998..1], (length (collatz x))  == maximum [length (collatz x) | x <- [1..999999]]])

{-137846528820 - Completed 17.5.2013 - Learned about binomial coefficients for solving lattice paths.-}
problem_15 = 40 `nCr` 20 

{-648 - Completed 11.5.2013,-}
problem_20 = (sum . int_to_list) $ factorial 100

{-31725 - Completed 12.5.2013 - Started working on typing-}
problem_21 = sum [a | a <- [1..9999], is_amicable a]
	where
		is_amicable :: Integral a => a -> Bool
		is_amicable a = (sum . divisors) ((sum . divisors) a) == a && a /= (sum . divisors) a


{-871198282 - Completed 5.5.2013 - Baby's first lambda-}
problem_22 = sum $ map (\n -> raw_score n * pos_mod n) names
	where
		names = [""] --Names.txt can be found on the website, I just ommited to clean it up.
		raw_score n = (sum . map char_pos) n
		char_pos c = (fromEnum c) - (fromEnum 'A') + 1
		pos_mod n = length (takeWhile (/=n) names) + 1

{-2783915460 - Completed 5.5.2013-}
problem_24 =  (sort . permutations) ['0'..'9'] !! 999999

problem_25 = head [x | x <- [1..], (length . show . fib) x == 1000]

{-9183 - Completed 3.5.2013-}
problem_29 = length $ nub [a^b | a <- [2..100], b <- [2..100]] 

{-443839 - Completed 5.5.2013-}
problem_30 = sum [s | s <- [2..999999], ((sum . map (^5)) (int_to_list s)) == s]

problem_32 = sum $ nub [a*b | a <- [1..10000], b <- [1..a], is_valid a b]
	where
		is_valid a b = is_pandigital' 1 9 (list_to_int [a*b,a,b])

{-40730 - Completed 5.5.2013-}
problem_34 = sum [x | x <- [3..99999], is_curious x]
	where
		is_curious n = (sum . map factorial) (int_to_list n) == n

problem_35 = [1 | n <- takeWhile (<1000000) primes, is_valid n]
	where
		is_valid n = foldl (&&) (True) $ map (is_prime . list_to_int) ((circulate . int_to_list) n)
		circulate ns = init (zipWith (++) (tails ns) (inits ns))

{-210 - Completed 15.5.2013 - Leaned about Champernowne's Constant.-}
problem_40 = product $ map (digitToInt . (!!) champernowne) indecies
	where
		indecies = map (10^) [1..6]

{-16695334890 - Completed 21.5.2013 -}
problem_43 = sum $ (map (read) [x | x <- permutations ['0'..'9'], is_valid x] :: [Integer])
	where
		is_valid x = (even . read) [x!!1,x!!2,x!!3] && read [x!!2,x!!3,x!!4] `mod` 3 == 0 && read [x!!3,x!!4,x!!5] `mod` 5 == 0
				&& read [x!!4,x!!5,x!!6] `mod` 7 == 0 && read [x!!5,x!!6,x!!7] `mod` 11 == 0 && read [x!!6,x!!7,x!!8] `mod` 13 == 0
				&& read [x!!7,x!!8,x!!9] `mod` 17 == 0 

{-134043 - Completed 17.5.2013-}
problem_47 = head [n | n <- [1..], is_valid n]
	where
		is_valid n = all (==4) $ map (length . nub . prime_factors) [n..n+3]

{-9110846700 - Completed 5.5.2013 - Wrote my last_n for this.-}
problem_48 = last_n 10 (sum (map (\x -> x^x) [1..1000]))

{-142857 - Completed 5.5.2013-}
problem_52 = head [n | n <- [1..], is_valid n]
	where
		is_valid n = (sort . int_to_list) n == ((nub . sort . concat . map (\x -> int_to_list (x*n))) [2..6])

{-4075 - Completed 8.5.2013 - Wrote nCr and learned about defining functions with infix.-}
problem_53 = genericLength [1 | n <- [1..100], r <- [1..n], n `nCr` r > 1000000]

{-249 - Completed 15.5.2013 -Learned about Lychrel Numbers.-}
problem_55 = length [n | n <- [1..9999], is_lychrel n]

{-972 - Completed 10.5.2013-}
problem_56 = maximum $ [(sum . int_to_list) (a^b) | a <- [1..99], b <- [1..99]]

{-510510 - Completed 16.5.2013 - Learned Euler Totient (phi(n)).-}
problem_69 =  last $ sortBy (compare `on` snd) [(n, (fromIntegral n) / (fromIntegral . euler_totient) n) | n <- [2..1000000]]

problem_70 = phi_perm 2 1000
	where
		phi_perm n m
			| n >= 10^7 = m
			| (sort . show) n == (sort . show . euler_totient) n && n%euler_totient n < m%euler_totient m = phi_perm (n+1) n
			| (sort . show) n == (sort . show . euler_totient) n && n%euler_totient n > m% euler_totient m = phi_perm (n+1) m
			| otherwise = phi_perm (n+1) m

{-428572 - Completed 17.5.2013 - Used Farey Sequences learned in problem 72.-}
problem_71 = farey_seq_term (0%1) (3%7) (10^6)

{-303963552391 - Completed 17.5.2013 - Learned Farey Sequences-}
problem_72 = farey_seq_length 1000000 

problem_73 = f_seq (1%3) (1%2) 12000 []
f_seq a b p ns
	| farey_seq_term (0%1) b p == a = ns
	| otherwise = f_seq a (farey_seq_term (0%1) b p) p ((farey_seq_term (0%1) b p):ns)

{-402 - Completed 17.5.2013 - Learned about Factorial Chains.-}
problem_74 = length [f_chain n| n <- [1..999999], is_valid n]
	where
		is_valid n = f_chain n == 60
		f_chain n = f_chain' n []
		f_chain' n ns
			| n `elem` ns = length ns
			| otherwise = f_chain' ((sum . map factorial) (int_to_list n)) (n:ns)

{-8581146 - Completed 22.5.2013 - Learned about forcing strictness -}
problem_92 = length $ [n | n <- [1..100000], sq_chain n]
	where
		sq_chain n
			| n == 1 = False
			| n == 89 = True
			| otherwise = sq_chain $! (sum (map (^2) (int_to_list n)))

{-8739992577 - Completed 8.5.2013-}
problem_97 = last_n 10 $ 28433*2^(7830457)+1

{-709 - Completed 22.5.2013 - Learned how to work with IO -}
problem_99 = do
	file <- readFile "base_exp.txt"
	putStrLn "Add 1 to index to get answer\nFormat: (index, value)"
	return $ maximumBy (compare `on` snd) $ map (\(l, a, b) -> (l, (read b)*log (read a)) ) [(str `elemIndex` (lines file), head (split_on (==',') str), (last (split_on (==',') str))) | str <- lines file]


problem_104 = head $ filter (\x -> is_pandigital 1 9 (head_n 9 x) && is_pandigital 1 9 (last_n 9 x)) (map (fib) [1..])

problem_112 = takeWhile (\x -> p_bouncy x < 0.99) [21700..]
	where
		p_bouncy n = fromIntegral (length (filter (is_bouncy) [1..n])) / (fromIntegral n)

problem_142 = head [x+y+z | x <- [1..1000], y <- [1..(x-1)], z <- [1..(y-1)], is_valid x y z]
	where
		is_valid x y z = foldl (&&) True (map is_square [(x+y),(x-y),(x+z),(x-z),(y+z),(y-z)])

problem_211 = sum [n | n <- [1..64000000], is_valid n]
	where
		is_valid :: Integral a => a -> Bool
		is_valid n = is_square $ sum (map (^2) (divisors n))

{-1.002322108633 (Paper/Pencil)- Completed 11.5.2013 - Learned how to bisection search by hand.-}
problem_235 = bisection_search f (0-600000000000) (1,1.5) 0.000000000001
	where
		f r = sum [(900-3*k)*r**(k-1) | k <- [1..5000]]

problem_371 =  [(n-1)*p | n <- [1..10]]
	where
		p = 999/1000000