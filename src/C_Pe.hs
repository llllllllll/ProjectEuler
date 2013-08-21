-- Joe Jevnik
-- Date: 8.20.2013
-- Utilities for working on Project Euler problems in c/c++.
module C_Pe
    ( check_status
    , open_problem
    , mark_complete
    , mark_incomplete
    , wrap_import
    , unwrap_import
    ) where

import System.IO
import System.Process
import System.Directory
import Data.List
import Data.Maybe
import Control.Monad
import Control.Applicative
import Problem_Wrapper

-- The file used to neatly wrap all problems to be imported by the Main module.
problem_wrapper :: FilePath                                                                 
problem_wrapper = "/home/joejev/compsci/ProjectEuler/src/C_Problem_Wrapper.hs"

check_status :: Int -> IO String
check_status p = do
    cs <- lines <$> readFile "C_Problems/.complete"
    ws <- lines <$> readFile "C_Problems/.incomplete"
    return $ check_status' p cs ws
    where
        check_status' p cs ws
            | show p `elem` cs = "Complete"
            | show p `elem` ws = "Incomplete"
            | otherwise = "Not yet started"

-- open_problem but for c/c++ problems.
open_problem :: Int -> IO ()
open_problem p = do
    s <- check_status p
    if s `elem` ["Complete","Incomplete"] 
    then (system $ "emacs C_Problems/Problem_" ++ show p ++ ".c &") >> return ()
    else do
        putStr "Problem has not been started, Would you like to start it (Y/n)"
        inp <- getLine
        unless (inp `elem` ["n","N"]) $ (system ("echo \"" ++ problem_template p 
                                       ++ "\" > C_Problems/Problem_" 
                                       ++ show p ++ ".c")) 
                   >> (system $ "emacs C_Problems/Problem_" 
                                  ++ show p ++ ".c &") 
                   >> appendFile "C_Problems/.incomplete" (show p)
    where
        problem_template n = "// NOT YET COMPLETED.\n#include <stdlib.h>\n"
                             ++ "#include <iostream>\n\nint main(){\n    "

-- Marks problem_p as complete.
mark_complete :: Int -> IO ()
mark_complete p = do
    ws <- lines <$> readFile "C_Problems/.incomplete"
    let ws' = filter (/=show p) ws
    removeFile "C_Problems/.incomplete"
    appendFile "C_Problems/.incomplete" $ unlines ws'
    appendFile "C_Problems/.complete" $ '\n':show p
    wrap_import p

-- Marks problem_p as incomplete
mark_incomplete :: Int -> IO ()
mark_incomplete p = do
    cs <- lines <$> readFile "C_Problems/.complete"
    let cs' = filter (/=show p) cs
    removeFile "C_Problems/.complete"
    appendFile "C_Problems/.complete" $ unlines cs'
    appendFile "C_Problems/.incomplete" $ '\n':show p
    unwrap_import p

-- Adds a problem to the C_Problem_Wrapper list.
wrap_import :: Int -> IO ()
wrap_import p = do
    ls <- fmap (break (=="    ) where") . lines) $ readFile problem_wrapper
    let edits = (unlines . fst) ls ++ "    , c_problem_"
                ++ show p ++ "\n" ++ (unlines . snd) ls ++ "\n"
                ++ "c_problem_" ++ show p ++ " :: IO ()\n"
                ++ "c_problem_" ++ show p ++ " = eval " ++ show p
    removeFile problem_wrapper
    appendFile problem_wrapper edits

-- Removes a problem from the C_Problem_Wrapper import list.
unwrap_import :: Int -> IO ()
unwrap_import p = do
    ls <- lines <$> readFile problem_wrapper
    let edits = unlines $ filter 
                (\l -> "c_problem_" ++ show p `notElem` words l) ls
    removeFile problem_wrapper
    appendFile problem_wrapper edits

