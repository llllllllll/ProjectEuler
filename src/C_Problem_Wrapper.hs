-- Joe Jevnik
-- Date: 20.8.2013
-- A neat wrapper module for all problems completed in c.
module C_Problem_Wrapper
    ( problem_1
    , problem_2
    , problem_3
    , problem_5
    , problem_15
    , problem_6
    , problem_4
    , problem_20
    , problem_13
    , problem_11
    , problem_18
    , problem_67
    , problem_19
    , problem_81
    ) where

import Control.Applicative ((<$>))
import Data.List (intersperse,isSuffixOf)
import System.Directory (removeFile,getDirectoryContents)
import System.Process (system,readProcess)

-- |Compiles the c problem_p, prints the output then deletes the binary.
eval :: Int -> IO ()
eval p = do
    us <- concat . intersperse " " . map ("C_Utils/" ++)
          . filter (\p -> p `notElem` [".",".."] && ".c" `isSuffixOf` p)
                <$> getDirectoryContents "C_Utils"
    system $ "gcc -o C_Problems/temp_proc -std=c11 C_Problems/Problem_"
               ++ show p ++ ".c " ++ us ++ " -lm -lgmp -Wall"
    readProcess "C_Problems/temp_proc" [] [] >>= putStr
    removeFile "C_Problems/temp_proc"

-- Automatically generated functions.

problem_1 :: IO ()
problem_1 = eval 1

problem_2 :: IO ()
problem_2 = eval 2

problem_3 :: IO ()
problem_3 = eval 3

problem_5 :: IO ()
problem_5 = eval 5

problem_15 :: IO ()
problem_15 = eval 15

problem_6 :: IO ()
problem_6 = eval 6

problem_4 :: IO ()
problem_4 = eval 4

problem_20 :: IO ()
problem_20 = eval 20

problem_13 :: IO ()
problem_13 = eval 13

problem_11 :: IO ()
problem_11 = eval 11

problem_18 :: IO ()
problem_18 = eval 18

problem_67 :: IO ()
problem_67 = eval 67

problem_19 :: IO ()
problem_19 = eval 19

problem_81 :: IO ()
problem_81 = eval 81
