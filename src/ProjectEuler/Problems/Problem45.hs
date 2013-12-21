-- 1533776805 - Completed 29.5.2013.
module ProjectEuler.Problems.Problem45
    ( problem45 -- :: IO ()
    ) where

problem45 :: IO ()
problem45 = print $ [h | h <- hexNums, h == head (dropWhile (<h) pentNums)] !! 1
    where
	pentNums = map (\n -> n * (3 * n - 1) `div` 2) [2..]
	hexNums  = map (\n -> n * (2 * n - 1)) [2..]
