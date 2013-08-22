module Main where

import System.IO
import System.Process
import System.Directory
import System.Environment
import Data.List
import Data.Maybe
import Data.Time
import Control.Monad
import Control.Applicative
import Problem_Wrapper
import C_Problem_Wrapper
import qualified C_Pe as C

main :: IO ()
main = do
    args <- getArgs
    check_args args
  where
      check_args args
          | head args == "--help"    = help
          | head args == "--version" = version
          | head args == "-c"        = parse_c (tail args)
          | otherwise                = parse_args args

help :: IO ()
help = putStrLn "Arguments: problem <p>          : Calls the problem_p\nls <(in)complete>    : C\nalls ls_(in)complete\ncount <(in)complete> : Calls count_(in)complete\nopen <p>             : Calls open_problem p\n-c                   : prefix to above to call C_Pe equivelent."

parse_args :: [String] -> IO ()
parse_args args
    | head args == "problem" = call_problem (read (args!!1) :: Int)
    | head args == "ls"      = list (args!!1)
    | head args == "count"   = count (args!!1)
    | head args == "open"    = open_problem (read (args!!1) :: Int)
    | otherwise              = error $ "Unexpected parameter " ++ head args
                               ++ ". Expected [-c,problem <p>,"
                               ++ "ls <(in)complete>,count <(in)complete>,"
                               ++ "open <p>]"

parse_c :: [String] -> IO ()
parse_c args
    | head args == "problem" = c_call_problem (read (args!!1) :: Int)
    | head args == "ls"      = c_list (args!!1)
    | head args == "count"   = c_count (args!!1)
    | head args == "open"    = C.open_problem (read (args!!1) :: Int)
    | otherwise              = error $ "Unexpected parameter " ++ head args
                               ++ ". Expected [problem <p>,ls <(in)complete>,"
                               ++ " count <(in)complete>,open <p>]"


list :: String -> IO ()
list str
    | str == "complete"   = ls_complete
    | str == "incomplete" = ls_incomplete
    | otherwise           = error $ "Unexpected parameter " ++ str 
                            ++ ". Expected complete or incomplete"

c_list :: String -> IO ()
c_list str
    | str == "complete"   = C.ls_complete
    | str == "incomplete" = C.ls_incomplete
    | otherwise           = error $ "Unexpected parameter " ++ str 
                            ++ ". Expected complete or incomplete"
count :: String -> IO Int
count str
    | str == "complete"   = count_complete
    | str == "incomplete" = count_incomplete
    | otherwise           = error $ "Unexpected parameter " ++ str 
                            ++ ". Expected complete or incomplete"

c_count :: String -> IO Int
c_count str
    | str == "complete"   = C.count_complete
    | str == "incomplete" = C.count_incomplete
    | otherwise           = error $ "Unexpected parameter " ++ str 
                            ++ ". Expected complete or incomplete"


ls_complete :: IO [Int]
ls_complete = (map read) . filter (/="") . lines
              <$> readFile "Problems/.complete"

ls_incomplete :: IO [Int]
ls_incomplete = (map read) . filter (/="") . lines
                <$> readFile "Problems/.incomplete"

count_complete :: IO Int
count_complete = length <$> ls_complete

count_incomplete :: IO Int
count_incomplete = length <$> ls_incomplete

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

-- GENERATED AUTOMATICALLY PAST THIS POINT

wrap_import :: Int -> IO ()
wrap_import _ = return ()

mark_incomplete :: Int -> IO ()
mark_incomplete _ = return ()

version :: IO ()
version = putStrLn "Joe Jevnik's work on Project Euler as of 2013-08-22 12:18:11.293955 UTC"

c_call_problem p
    | p == 1 = print c_problem_1
    | p == 2 = print c_problem_2
    | p == 5 = print c_problem_5
    | p == 15 = print c_problem_15
    | otherwise = error "Problem not complete."

call_problem p
    | p == 1 = print problem_1
    | p == 2 = print problem_2
    | p == 3 = print problem_3
    | p == 4 = print problem_4
    | p == 5 = print problem_5
    | p == 6 = print problem_6
    | p == 7 = print problem_7
    | p == 8 = print problem_8
    | p == 9 = print problem_9
    | p == 10 = print problem_10
    | p == 11 = print problem_11
    | p == 12 = print problem_12
    | p == 13 = print problem_13
    | p == 14 = print problem_14
    | p == 15 = print problem_15
    | p == 16 = print problem_16
    | p == 20 = print problem_20
    | p == 21 = print problem_21
    | p == 22 = print problem_22
    | p == 23 = print problem_23
    | p == 24 = print problem_24
    | p == 25 = print problem_25
    | p == 29 = print problem_29
    | p == 30 = print problem_30
    | p == 32 = print problem_32
    | p == 33 = print problem_33
    | p == 34 = print problem_34
    | p == 35 = print problem_35
    | p == 36 = print problem_36
    | p == 39 = print problem_39
    | p == 40 = print problem_40
    | p == 41 = print problem_41
    | p == 42 = print problem_42
    | p == 43 = print problem_43
    | p == 44 = print problem_44
    | p == 45 = print problem_45
    | p == 46 = print problem_46
    | p == 47 = print problem_47
    | p == 48 = print problem_48
    | p == 52 = print problem_52
    | p == 53 = print problem_53
    | p == 55 = print problem_55
    | p == 56 = print problem_56
    | p == 59 = print problem_59
    | p == 62 = print problem_62
    | p == 63 = print problem_63
    | p == 69 = print problem_69
    | p == 70 = print problem_70
    | p == 71 = print problem_71
    | p == 72 = print problem_72
    | p == 73 = print problem_73
    | p == 74 = print problem_74
    | p == 76 = print problem_76
    | p == 79 = print problem_79
    | p == 92 = print problem_92
    | p == 95 = print problem_95
    | p == 97 = print problem_97
    | p == 99 = print problem_99
    | p == 108 = print problem_108
    | p == 112 = print problem_112
    | p == 124 = print problem_124
    | p == 142 = print problem_142
    | p == 171 = print problem_171
    | p == 188 = print problem_188
    | p == 206 = print problem_206
    | p == 216 = print problem_216
    | p == 243 = print problem_243
    | p == 277 = print problem_277
    | p == 99 = print problem_99
    | otherwise = error "Problem not complete."

