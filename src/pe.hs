-- Joe Jevnik
-- Date: 31.7.2013
-- Utilities for working on Project Euler problems in Haskell.
module Main where

import System.IO
import System.Process
import System.Directory
import Data.List
import Data.Maybe
import Control.Monad
import Control.Applicative
import Problem_Wrapper
import C_Problem_Wrapper
import qualified C_Pe as C

-- The file used to neatly wrap all problems to be imported by the Main module.
problem_wrapper :: FilePath                                                                 
problem_wrapper = "/home/joejev/compsci/ProjectEuler/src/Problem_Wrapper.hs"

-- Displays some basic information about my work.
main :: IO ()
main = putStrLn "Project Euler Work by Joe Jevnik"

-- Checks the completion status of problem_9
check_status :: Int -> IO String
check_status p = do
    cs <- lines <$> readFile "Problems/.complete"
    ws <- lines <$> readFile "Problems/.incomplete"
    return $ check_status' p cs ws
    where
        check_status' p cs ws
            | show p `elem` cs = "Complete"
            | show p `elem` ws = "Incomplete"
            | otherwise = "Not yet started"

-- Opens the given problem in a new emacs window or
-- starts a new problem with the template if it does not exist.
open_problem :: Int -> IO ()
open_problem p = do
    s <- check_status p
    if s `elem` ["Complete","Incomplete"] 
    then (system $ "emacs Problems/Problem_" ++ show p ++ ".hs &") >> return ()
    else do
        putStr "Problem has not been started, Would you like to start it (Y/n): "
        inp <- getLine
        unless (inp `elem` ["n","N"]) $ (system ("echo \"" ++ problem_template p 
                                                 ++ "\" > Problems/Problem_" 
                                                 ++ show p ++ ".hs")) 
                   >> (system $ "emacs Problems/Problem_" 
                                  ++ show p ++ ".hs &") 
                   >> appendFile "Problems/.incomplete" (show p)
    where
        problem_template n = "-- NOT YET COMPLETED.\nmodule Problems.Problem_" 
                             ++ show n ++ "\n    ( problem_" ++ show n 
                             ++ "\n    ) where\n\nproblem_" ++ show n 
                             ++ " = "
              
-- Marks problem_p as complete.
mark_complete :: Int -> IO ()
mark_complete p = do
    ws <- lines <$> readFile "Problems/.incomplete"
    let ws' = filter (/=show p) ws
    removeFile "Problems/.incomplete"
    appendFile "Problems/.incomplete" $ unlines ws'
    appendFile "Problems/.complete" $ '\n':show p
    wrap_import p

-- Marks problem_p as incomplete
mark_incomplete :: Int -> IO ()
mark_incomplete p = do
    cs <- lines <$> readFile "Problems/.complete"
    let cs' = filter (/=show p) cs
    removeFile "Problems/.complete"
    appendFile "Problems/.complete" $ unlines cs'
    appendFile "Problems/.incomplete" $ '\n':show p
    unwrap_import p

-- Adds a problem to the Problem_Wrapper list.
wrap_import :: Int -> IO ()
wrap_import p = do
    ls <- fmap (break (=="    ) where") . lines) $ readFile problem_wrapper
    let edits = (unlines . fst) ls ++ "    , module Problems.Problem_"
                ++ show p ++ "\n" ++ (unlines . snd) ls
                ++ "import Problems.Problem_" ++ show p
    removeFile problem_wrapper
    appendFile problem_wrapper edits

-- Removes a problem from the Problem_Wrapper import list.
unwrap_import :: Int -> IO ()
unwrap_import p = do
    ls <- lines <$> readFile problem_wrapper
    let edits = unlines $ filter 
                (\l -> "Problems.Problem_" ++ show p `notElem` words l) ls
    removeFile problem_wrapper
    appendFile problem_wrapper edits

