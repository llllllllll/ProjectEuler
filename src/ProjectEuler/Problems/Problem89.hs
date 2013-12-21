-- 743 - Completed 8.26.2013
module ProjectEuler.Problems.Problem89
    ( problem89 -- IO ()
    ) where

import System.IO               (readFile)
import ProjectEuler.Utils.Misc (fromRoman,toRoman)

problem89 :: IO ()
problem89 = do
    file <- readFile "txt/roman.txt"
    let len  = length file
        len' = length $ unlines $ map (toRoman . fromRoman) $ lines file
    print $ len - len' + 1
