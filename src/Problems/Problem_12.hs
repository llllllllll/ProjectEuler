-- 76576500 - Completed 26.5.12.
module Problems.Problem_12
    ( problem_12
    ) where

import Utils.Misc (num_divisors)

problem_12 = head [x | x <- tri_nums, num_divisors x > 500]
    where
	tri_nums = map (\x -> sum [1..x]) [1..]
