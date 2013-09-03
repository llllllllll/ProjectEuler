-- Joe Jevnik
-- Date: 8.20.2013
-- Utilities for working on Project Euler problems in c/c++.
module C_Pe
    ( check_status
    , open_problem
    , mark_complete
    , mark_incomplete
    , mark_not_started
    , wrap_import
    , unwrap_import
    , ls_complete
    , ls_incomplete
    , count_complete
    , count_incomplete
    ) where

import System.IO
import System.Process
import System.Directory
import Data.List
import Data.Maybe
import Control.Monad
import Control.Applicative

-- The file used to neatly wrap all problems to be imported by the Main module.
problem_wrapper :: FilePath 
problem_wrapper = "/home/joejev/compsci/ProjectEuler/src/C_Problem_Wrapper.hs"

-- The file containing the list of completed problems.
dot_complete :: FilePath
dot_complete = "/home/joejev/compsci/ProjectEuler/src/C_Problems/.complete"

-- The file containing the list of incomplete problems.
dot_incomplete :: FilePath
dot_incomplete = "/home/joejev/compsci/ProjectEuler/src/C_Problems/.incomplete"

-- Returns the completion status of c_problem_p
check_status :: Int -> IO String
check_status p = do
    cs <- lines <$> readFile dot_complete
    ws <- lines <$> readFile dot_incomplete
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
      then (system $ "emacs C_Problems/Problem_" ++ show p ++ ".c &") 
                 >> return ()
      else do
          putStr $ "Problem " ++ show p ++
                     " has not been started, Would you like to start it (Y/n):"
          inp <- getLine
          unless (inp `elem` ["n","N"]) 
                     $ (system ("echo \"" ++ problem_template p 
                                ++ "\" > C_Problems/Problem_" 
                                ++ show p ++ ".c")) 
                     >> (system $ "emacs C_Problems/Problem_" 
                                    ++ show p ++ ".c &") 
                     >> appendFile dot_incomplete (show p) 
                     >> wrap_import p >> mark_incomplete p
  where
      problem_template n = "// NOT YET COMPLETED.\n#include <stdlib.h>\n"
                           ++ "#include <iostream>\n#include <gmp.h>\n\n"
                           ++ "int main(){\n    \n}"

-- Marks problem_p as complete.
mark_complete :: Int -> IO ()
mark_complete p = do
    cs <- lines <$> readFile dot_complete
    ws <- lines <$> readFile dot_incomplete
    unless (show p `elem` cs) $ do
                      let ws' = filter (/=show p) ws
                      removeFile dot_incomplete
                      appendFile dot_incomplete $ unlines ws'
                      appendFile dot_complete $ '\n':show p

-- Marks problem_p as incomplete
mark_incomplete :: Int -> IO ()
mark_incomplete p = do
    cs <- lines <$> readFile dot_complete
    ws <- lines <$> readFile dot_incomplete
    unless (show p `elem` ws) $ do
                        let cs' = filter (/=show p) cs
                        removeFile dot_complete
                        appendFile dot_complete $ unlines cs'
                        appendFile dot_incomplete $ '\n':show p

-- Marks a problem as not yet started.
mark_not_started :: Int -> IO ()
mark_not_started p = do
    cs <- lines <$> readFile dot_complete
    ws <- lines <$> readFile dot_incomplete
    let cs' = filter (/=show p) cs
        ws' = filter (/=show p) ws
    mapM_ removeFile [dot_incomplete,dot_complete]
    appendFile dot_complete $ unlines cs'
    appendFile dot_incomplete $ unlines ws'

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

-- Returns a list of problems that are completed.
ls_complete :: IO [Int]
ls_complete = sort . (map read) . filter (/="") . lines 
              <$> readFile dot_complete

-- Returns a list of problems that are incomplete.
ls_incomplete :: IO [Int]
ls_incomplete = sort . (map read) . filter (/="") . lines
                <$> readFile dot_incomplete

-- Returns the number of problems that are marked complete.
count_complete :: IO Int
count_complete = length <$> ls_complete

-- Returns the number of problems that are marked incomplete.
count_incomplete :: IO Int
count_incomplete = length <$> ls_incomplete
