-- Joe Jevnik
-- Date: 20.8.2013
-- A neat wrapper module for all problems completed in c/c++.
module C_Problem_Wrapper
    ( c_problem_1
    , c_problem_2
    , c_problem_3
    ) where

import System.Process
import System.Directory

-- Compiles the c/c++ problem_p, prints the output then delets the binary.
eval :: Int -> IO ()
eval p = do
    system $ "g++ -o C_Problems/temp_proc C_Problems/Problem_"
               ++ show p ++ ".c"
    readProcess "C_Problems/temp_proc" [] [] >>= putStrLn
    removeFile "C_Problems/temp_proc"
    
c_problem_1 :: IO ()
c_problem_1 = eval 1

c_problem_2 :: IO ()
c_problem_2 = eval 2

c_problem_3 :: IO ()
c_problem_3 = eval 3
