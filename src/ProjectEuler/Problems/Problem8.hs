-- 40824 - Completed 3.6.2013.
module ProjectEuler.Problems.Problem8
    ( problem8 -- IO ()
    ) where

import Control.Applicative ((<$>))
import Data.Array          (listArray,(!))
import Data.Char           (digitToInt)
import System.IO           (readFile,print)

problem8 :: IO ()
problem8 = do
    num <- listArray (1,1000) . map digitToInt . filter (/='\n')
           <$> readFile "txt/problem8.txt"
    let ps = [num!n * num!(n + 1) * num!(n + 2) * num!(n + 3) * num!(n + 4)
                  | n <- [1..995]]
    print $ maximum ps
