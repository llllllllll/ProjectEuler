-- Joe Jevnik
-- Date: 31.7.2013
-- Utilities for working on Project Euler problems in Haskell.
module Main where

import System.IO
import System.Process
import System.Directory
import Data.List
import Data.Maybe
import Data.Time
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
-- WARNING: Sending interupts to ghci will close the window.
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
                     >> wrap_import p >> mark_incomplete p
  where
      problem_template n = "-- NOT YET COMPLETED.\nmodule Problems.Problem_" 
                           ++ show n ++ "\n    ( problem_" ++ show n 
                           ++ "\n    ) where\n\nproblem_" ++ show n 
                           ++ " = "
              
-- Marks problem_p as complete.
mark_complete :: Int -> IO ()
mark_complete p = do
    cs <- lines <$> readFile "Problems/.complete"
    ws <- lines <$> readFile "Problems/.incomplete"
    unless (show p `elem` cs) $ do
                      let ws' = filter (/=show p) ws
                      removeFile "Problems/.incomplete"
                      appendFile "Problems/.incomplete" $ unlines ws'
                      appendFile "Problems/.complete" $ '\n':show p

-- Marks problem_p as incomplete
mark_incomplete :: Int -> IO ()
mark_incomplete p = do
    cs <- lines <$> readFile "Problems/.complete"
    ws <- lines <$> readFile "Problems/.incomplete"
    unless (show p `elem` ws) $ do
                        let cs' = filter (/=show p) cs
                        removeFile "Problems/.complete"
                        appendFile "Problems/.complete" $ unlines cs'
                        appendFile "Problems/.incomplete" $ '\n':show p

-- Marks a problem as not yet started.
mark_not_started :: Int -> IO ()
mark_not_started p = do
    cs <- lines <$> readFile "Problems/.complete"
    ws <- lines <$> readFile "Problems/.incomplete"
    let cs' = filter (/=show p) cs
        ws' = filter (/=show p) ws
    mapM_ removeFile ["Problems/.incomplete","Problems/.complete"]
    appendFile "Problems/.complete" $ unlines cs'
    appendFile "Problems/.incomplete" $ unlines ws'

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

-- Returns a list of problems that are completed.
ls_complete :: IO [Int]
ls_complete = sort . (map read) . filter (/="") . lines 
              <$> readFile "Problems/.complete"

-- Returns a list of problems that are incomplete.
ls_incomplete :: IO [Int]
ls_incomplete = sort . (map read) . filter (/="") . lines
                <$> readFile "Problems/.incomplete"

-- Returns the number of problems that are marked complete.
count_complete :: IO Int
count_complete = length <$> ls_complete

-- Returns the number of problems that are marked incomplete.
count_incomplete :: IO Int
count_incomplete = length <$> ls_incomplete

-- Creates an version of my current progress and exports the source/binary
-- that accept all current problems marked complete in both Problems/.complete
-- and C_Problems/.complete.
-- Arguments: problem <p>          : Calls the problem_p
--            ls <(in)complete>    : Calls ls_(in)complete
--            count <(in)complete> : Calls count_(in)complete
--            open <p>             : Calls open_problem p
--            -c                   : prefix to above to call C_Pe equivelent.
mk_binary :: IO ()
mk_binary = do
    time <- getCurrentTime
    fs   <- map mk_fun <$> ls_complete
    cfs  <- map c_mk_fun <$> C.ls_complete
    let h   = head bin_template
        c_  = bin_template!!1
        ca  = bin_template!!2
        gs  = mk_guards fs
        cgs = mk_guards cfs
        vrs = "\"Joe Jevnik's work on Project Euler as of " ++ show time ++ "\""
        out = concat [h,vrs ++ "\n\n",c_,cgs,ca,gs]
        fil = "../bin/" ++ take 10 (show time) ++ ".hs"
    appendFile fil out
    system $ "ghc --make ../bin/" ++ fil
    return ()
  where
      mk_fun :: Int -> String
      mk_fun p = "problem_" ++ show p
      c_mk_fun :: Int -> String
      c_mk_fun p = "c_problem_" ++ show p
      mk_guards :: [String] -> String
      mk_guards ps = (concatMap (\p -> "    | p == " 
                                 ++ fromMaybe (drop 10 p) (stripPrefix "problem_" p)  
                                 ++ " = print " ++ p ++ "\n") ps)
                     ++ "    | otherwise = error \"Problem not complete.\"\n\n"

bin_template :: [String]
bin_template = [ "module Main where\n\nimport System.IO\nimport System.Process\nimport System.Directory\nimport System.Environment\nimport Data.List\nimport Data.Maybe\nimport Data.Time\nimport Control.Monad\nimport Control.Applicative\nimport Problem_Wrapper\nimport C_Problem_Wrapper\nimport qualified C_Pe as C\n\nmain :: IO ()\nmain = do\n    args <- getArgs\n    check_args args\n  where\n      check_args args\n          | head args == \"--help\"    = help\n          | head args == \"--version\" = version\n          | head args == \"-c\"        = parse_c (tail args)\n          | otherwise                = parse_args args\n\nhelp :: IO ()\nhelp = putStrLn \"Arguments: problem <p>          : Calls the problem_p\\nls <(in)complete>    : C\\nalls ls_(in)complete\\ncount <(in)complete> : Calls count_(in)complete\\nopen <p>             : Calls open_problem p\\n-c                   : prefix to above to call C_Pe equivelent.\"\n\nparse_args :: [String] -> IO ()\nparse_args args\n    | head args == \"problem\" = call_problem (read (args!!1) :: Int)\n    | head args == \"ls\"      = list (args!!1)\n    | head args == \"count\"   = count (args!!1)\n    | head args == \"open\"    = open_problem (read (args!!1) :: Int)\n    | otherwise              = error $ \"Unexpected parameter \" ++ head args\n                               ++ \". Expected [-c,problem <p>,\"\n                               ++ \"ls <(in)complete>,count <(in)complete>,\"\n                               ++ \"open <p>]\"\n\nparse_c :: [String] -> IO ()\nparse_c args\n    | head args == \"problem\" = c_call_problem (read (args!!1) :: Int)\n    | head args == \"ls\"      = c_list (args!!1)\n    | head args == \"count\"   = c_count (args!!1)\n    | head args == \"open\"    = C.open_problem (read (args!!1) :: Int)\n    | otherwise              = error $ \"Unexpected parameter \" ++ head args\n                               ++ \". Expected [problem <p>,ls <(in)complete>,\"\n                               ++ \" count <(in)complete>,open <p>]\"\n\n\nlist :: String -> IO ()\nlist str\n    | str == \"complete\"   = ls_complete\n    | str == \"incomplete\" = ls_incomplete\n    | otherwise           = error $ \"Unexpected parameter \" ++ str \n                            ++ \". Expected complete or incomplete\"\n\nc_list :: String -> IO ()\nc_list str\n    | str == \"complete\"   = C.ls_complete\n    | str == \"incomplete\" = C.ls_incomplete\n    | otherwise           = error $ \"Unexpected parameter \" ++ str \n                            ++ \". Expected complete or incomplete\"\ncount :: String -> IO Int\ncount str\n    | str == \"complete\"   = count_complete\n    | str == \"incomplete\" = count_incomplete\n    | otherwise           = error $ \"Unexpected parameter \" ++ str \n                            ++ \". Expected complete or incomplete\"\n\nc_count :: String -> IO Int\nc_count str\n    | str == \"complete\"   = C.count_complete\n    | str == \"incomplete\" = C.count_incomplete\n    | otherwise           = error $ \"Unexpected parameter \" ++ str \n                            ++ \". Expected complete or incomplete\"\n\n\nls_complete :: IO [Int]\nls_complete = (map read) . filter (/=\"\") . lines\n              <$> readFile \"Problems/.complete\"\n\nls_incomplete :: IO [Int]\nls_incomplete = (map read) . filter (/=\"\") . lines\n                <$> readFile \"Problems/.incomplete\"\n\ncount_complete :: IO Int\ncount_complete = length <$> ls_complete\n\ncount_incomplete :: IO Int\ncount_incomplete = length <$> ls_incomplete\n\nopen_problem :: Int -> IO ()\nopen_problem p = do\n    s <- check_status p\n    if s `elem` [\"Complete\",\"Incomplete\"]\n      then (system $ \"emacs Problems/Problem_\" ++ show p ++ \".hs &\") >> return ()\n      else do\n          putStr \"Problem has not been started, Would you like to start it (Y/n): \"\n          inp <- getLine\n          unless (inp `elem` [\"n\",\"N\"]) $ (system (\"echo \\\"\" ++ problem_template p\n                                                   ++ \"\\\" > Problems/Problem_\" \n                                                   ++ show p ++ \".hs\"))\n                     >> (system $ \"emacs Problems/Problem_\"\n                                    ++ show p ++ \".hs &\")\n                     >> appendFile \"Problems/.incomplete\" (show p)\n                     >> wrap_import p >> mark_incomplete p\n  where\n      problem_template n = \"-- NOT YET COMPLETED.\\nmodule Problems.Problem_\" \n                           ++ show n ++ \"\\n    ( problem_\" ++ show n \n                           ++ \"\\n    ) where\\n\\nproblem_\" ++ show n \n                           ++ \" = \"\n\ncheck_status :: Int -> IO String\ncheck_status p = do\n    cs <- lines <$> readFile \"Problems/.complete\"\n    ws <- lines <$> readFile \"Problems/.incomplete\"\n    return $ check_status' p cs ws\n  where\n      check_status' p cs ws\n          | show p `elem` cs = \"Complete\"\n          | show p `elem` ws = \"Incomplete\"\n          | otherwise = \"Not yet started\"\n\n-- GENERATED AUTOMATICALLY PAST THIS POINT\n\nwrap_import :: Int -> IO ()\nwrap_import _ = return ()\n\nmark_incomplete :: Int -> IO ()\nmark_incomplete _ = return ()\n\nversion :: IO ()\nversion = putStrLn "
               , "c_call_problem p\n"
               , "call_problem p\n" ]
    
