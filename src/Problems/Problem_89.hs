module Problems.Problem_89 (
    ) where

import System.IO
import Utils.Misc

{-problem_89 = do
    file <- readFile "txt/roman.txt"
    let 
	start = length $ concat $ lines file
	nums = lines file
	fixed = map to_roman [from_roman str | str <- nums]
    print $ start-}