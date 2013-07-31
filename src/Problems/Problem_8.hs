-- 40824 - Completed 3.6.2013.
module Problems.Problem_8
    ( problem_8
    ) where

import System.IO
import Data.Char
import Data.Array

problem_8 = do
    file <- readFile "txt/problem_8.txt"
    let
	num = listArray (1,1000) (map digitToInt $ filter (/='\n') 
                                      file)
	ps = [num!n * num!(n+1) * num!(n+2) * num!(n+3) * num!(n+4) 
                  |  n <- [1..995]]
    print $ maximum ps