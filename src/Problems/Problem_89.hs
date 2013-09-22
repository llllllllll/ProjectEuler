-- 743 - Completed 8.26.2013
module Problems.Problem_89 
    ( 
     problem_89 
    ) where

import System.IO (readFile)
import Utils.Misc (from_roman,to_roman)

problem_89 = do
    file <- readFile "txt/roman.txt"
    let len = length file
        len' = length $ unlines $ map (to_roman . from_roman) $ lines file
    print $ len - len' + 1
