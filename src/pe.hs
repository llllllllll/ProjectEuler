-- Date: 31.7.2013
module Main where

import System.IO
import System.Process
import System.Directory
import Data.List
import Data.Maybe
import Control.Monad
import Control.Applicative
import Problem_Wrapper

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
        putStrLn "Problem has not been started, Would you like to start it (y/n)"
        inp <- getLine
        when (inp == "y") $ (system ("echo \"" ++ problem_template p 
                                    ++ "\" > Problems/Problem_" 
                                    ++ show p ++ ".hs")) 
                 >> (system $ "emacs Problems/Problem_" 
                                ++ show p ++ ".hs &") 
                 >> appendFile "Problems/.incomplete" (show p)
    where
        problem_template n = "-- NOT YET COMPLETED.\nmodule Problem_" ++ show n
                             ++ "\n    ( problem_" ++ show n 
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

-- Marks problem_p as incomplete
mark_incomplete :: Int -> IO ()
mark_incomplete p = do
    cs <- lines <$> readFile "Problems/.complete"
    let cs' = filter (/=show p) cs
    removeFile "Problems/.complete"
    appendFile "Problems/.complete" $ unlines cs'
    appendFile "Problems/.incomplete" $ '\n':show p