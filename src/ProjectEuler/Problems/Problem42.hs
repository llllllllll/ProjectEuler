-- 162 - Completed 23.5.2013.
module ProjectEuler.Problems.Problem42
    ( problem42 -- :: IO ()
    ) where

import Control.Applicative     ((<$>))
import System.IO               (readFile)
import Data.List               (dropWhile)
import ProjectEuler.Utils.List (splitOn)

problem42 :: IO ()
problem42 = do
    words <- splitOn (== ',') <$> readFile "txt/words.txt"
    let charPos c = (fromEnum c) - (fromEnum 'A') + 1
	isTri str = isTriNum $ sum $ map (charPos) str
	triNums = [fromIntegral (n*(n+1) `div` 2) | n <- [1..]]
	isTriNum n = n == head (dropWhile (<n) triNums)
    print $ length [filter (/='\"') str | str <- words
                   , isTri (filter (/='\"') str)]
