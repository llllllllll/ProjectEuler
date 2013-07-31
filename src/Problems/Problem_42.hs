-- 162 - Completed 23.5.2013.
module Problems.Problem_42
    ( problem_42
    ) where

import System.IO
import Data.List
import Utils.List

problem_42 = do
    file <- readFile "txt/words.txt"
    let 
	words = split_on (==',') file
	char_pos c = (fromEnum c) - (fromEnum 'A') + 1
	is_tri str = is_tri_num $ sum $ map (char_pos) str
	tri_nums = [fromIntegral (n*(n+1) `div` 2) | n <- [1..]]
	is_tri_num n = n == head (dropWhile (<n) tri_nums)
    return $ length [filter (/='\"') str 
                     | str <- words, is_tri (filter (/='\"') str)]