-- 709 - Completed 22.5.2013 - Learned how to work with IO.
module ProjectEuler.Problems.Problem99
    ( problem99 -- :: IO ()
    ) where

import System.IO               (readFile)
import Control.Applicative     ((<$>))
import Data.Function           (on)
import Data.List               (maximumBy,elemIndex)
import ProjectEuler.Utils.List (splitOn)

problem99 :: IO ()
problem99 = do
    file <- readFile "txt/baseExp.txt"
    putStrLn "Format: (index, log (value))"
    print $ (\(a,b) -> ((+1) <$> a,b)) $ maximumBy (compare `on` snd)
              $ map (\(l, a, b) -> (l, (read b)*log (read a)) )
                    [(str `elemIndex` (lines file), head (splitOn (==',') str),
                              (last (splitOn (==',') str))) | str <- lines file]
