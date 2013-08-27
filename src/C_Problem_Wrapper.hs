-- Joe Jevnik
-- Date: 20.8.2013
-- A neat wrapper module for all problems completed in c/c++.
module C_Problem_Wrapper
    ( c_problem_1
    , c_problem_2
    , c_problem_3
    , c_problem_5
    , c_problem_15
    , c_problem_6
    , c_problem_4
    , c_problem_20
    , c_problem_13
    , c_problem_11
    ) where

import System.Process
import System.Directory

-- Compiles the c/c++ problem_p, prints the output then delets the binary.
eval :: Int -> IO ()
eval p = do
    system $ "g++ -o C_Problems/temp_proc C_Problems/Problem_"
               ++ show p ++ ".c -lm -lgmp"
    readProcess "C_Problems/temp_proc" [] [] >>= putStrLn
    removeFile "C_Problems/temp_proc"

-- Automatically generated functions.

c_problem_1 :: IO ()
c_problem_1 = eval 1

c_problem_2 :: IO ()
c_problem_2 = eval 2

c_problem_3 :: IO ()
c_problem_3 = eval 3

c_problem_5 :: IO ()
c_problem_5 = eval 5

c_problem_15 :: IO ()
c_problem_15 = eval 15

c_problem_6 :: IO ()
c_problem_6 = eval 6

c_problem_4 :: IO ()
c_problem_4 = eval 4

c_problem_20 :: IO ()
c_problem_20 = eval 20

c_problem_13 :: IO ()
c_problem_13 = eval 13

c_problem_11 :: IO ()
c_problem_11 = eval 11