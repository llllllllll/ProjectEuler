-- 1533776805 - Completed 29.5.2013.
module Problems.Problem_45
    ( problem_45
    ) where

problem_45 = [h | h <- hex_nums, h == head (dropWhile (<h) pent_nums)]!! 1
    where
	pent_nums = map (\n -> n*(3*n-1)`div`2) [2..]
	hex_nums = map (\n -> n*(2*n-1)) [2..]