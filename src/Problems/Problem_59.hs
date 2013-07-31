-- 107359 - Completed 26.5.2013 - Most fun of any problem I have done.
module Problems.Problem_59
    ( problem_59
    ) where

import System.IO
import Data.List
import Data.Char
import Data.Bits
import Utils.List

problem_59 = do
    file <- readFile "txt/cipher1.txt"
    let ascii = map (read) (split_on (==',') (filter (/='\n') file))
    print $ sum $ zipWith (xor) (map ord (cycle "god")) ascii