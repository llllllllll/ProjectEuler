-- 107359 - Completed 26.5.2013 - Most fun of any problem I have done.
module ProjectEuler.Problems.Problem59
    ( problem59 -- :: IO ()
    ) where

import System.IO               (readFile,print)
import Data.List               (cycle)
import Data.Char               (ord)
import Data.Bits               (xor)
import ProjectEuler.Utils.List (splitOn)

problem59 :: IO ()
problem59 = do
    file <- readFile "txt/cipher1.txt"
    let ascii = map (read) (splitOn (==',') (filter (/='\n') file))
    print $ sum $ zipWith (xor) (map ord (cycle "god")) ascii
