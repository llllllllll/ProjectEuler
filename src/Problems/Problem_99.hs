-- 709 - Completed 22.5.2013 - Learned how to work with IO.
module Problems.Problem_99 
    ( problem_99
    ) where

import System.IO (readFile)
import Control.Applicative ((<$>))
import Data.Function (on)
import Data.List (maximumBy,elemIndex)
import Utils.List (split_on)

problem_99 = do
    file <- readFile "txt/base_exp.txt"
    putStrLn "Format: (index, log (value))"
    return $ (\(a,b) -> ((+1) <$> a,b)) $ maximumBy (compare `on` snd) $ 
           map (\(l, a, b) -> (l, (read b)*log (read a)) ) 
             [(str `elemIndex` (lines file), head (split_on (==',') str), 
               (last (split_on (==',') str))) | str <- lines file]
