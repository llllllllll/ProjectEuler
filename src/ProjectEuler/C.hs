-- |
-- Module      : ProjectEuler.C
-- Copyright   : Joe Jevnik 2013
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- An interface for working on project euler in C.

module ProjectEuler.C
    ( module ProjectEuler.C.Problems
    , checkStatus
    , openProblem
    , markComplete
    , markIncomplete
    , markNotStarted
    , wrapImport
    , unwrapImport
    , lsComplete
    , lsIncomplete
    , countComplete
    , countIncomplete
    ) where

import Control.Applicative ((<$>))
import Control.Monad       (unless,void)
import Data.List           (sort)
import System.Directory    (removeFile)
import System.Process      (system)

import ProjectEuler.C.Problems

-- | The file used to neatly wrap all problems to be imported by the C module.
problemWrapper :: FilePath
problemWrapper = "ProjectEuler/C/Problems.hs"

-- | The file containing the list of completed problems.
dotComplete :: FilePath
dotComplete = "ProjectEuler/C/Problems/.complete"

-- | The file containing the list of incomplete problems.
dotIncomplete :: FilePath
dotIncomplete = "ProjectEuler/C/Problems/.incomplete"

-- | Returns the completion status of problemP
checkStatus :: Int -> IO String
checkStatus p = do
    cs <- lines <$> readFile dotComplete
    ws <- lines <$> readFile dotIncomplete
    return $ checkStatus' p cs ws
  where
      checkStatus' p cs ws
          | show p `elem` cs = "Complete"
          | show p `elem` ws = "Incomplete"
          | otherwise        = "Not yet started"

-- | openProblem but for c/c++ problems.
openProblem :: Int -> IO ()
openProblem p = do
    s <- checkStatus p
    if s `elem` ["Complete","Incomplete"]
      then void (system $ "emacs " ++ prob p ++ ".c")
      else do
          putStr $ "Problem " ++ show p
           ++ " has not been started, Would you like to start it (Y/n):"
          inp <- getLine
          unless (inp `elem` ["n","N"])
                     $ (system ("echo \"" ++ problemTemplate p
                                ++ "\" > " ++ prob p ++ ".c"))
                     >> (system $ "emacs " ++ prob p ++ ".c")
                     >> appendFile dotIncomplete (show p)
                     >> wrapImport p >> markIncomplete p
  where
      prob = (++) "ProjectEuler/C/Problems/Problem" . show
      problemTemplate n = "// NOT YET COMPLETED.\n#include <stdlib.h>\n"
                          ++ "#include <stdio.h>\n\nint main(){\n    \n}"

-- | Marks problemP as complete.
markComplete :: Int -> IO ()
markComplete p = do
    cs <- lines <$> readFile dotComplete
    ws <- lines <$> readFile dotIncomplete
    unless (show p `elem` cs) $ do
                      let ws' = filter (/=show p) ws
                      removeFile dotIncomplete
                      appendFile dotIncomplete $ unlines ws'
                      appendFile dotComplete $ '\n':show p

-- | Marks problemP as incomplete
markIncomplete :: Int -> IO ()
markIncomplete p = do
    cs <- lines <$> readFile dotComplete
    ws <- lines <$> readFile dotIncomplete
    unless (show p `elem` ws) $ do
                        let cs' = filter (/=show p) cs
                        removeFile dotComplete
                        appendFile dotComplete   $ unlines cs'
                        appendFile dotIncomplete $ '\n':show p

-- | Marks a problem as not yet started.
markNotStarted :: Int -> IO ()
markNotStarted p = do
    cs <- lines <$> readFile dotComplete
    ws <- lines <$> readFile dotIncomplete
    let cs' = filter (/=show p) cs
        ws' = filter (/=show p) ws
    mapM removeFile [dotIncomplete,dotComplete]
    appendFile dotComplete $ unlines cs'
    appendFile dotIncomplete $ unlines ws'

-- | Adds a problem to the C problem wrapper import list.
wrapImport :: Int -> IO ()
wrapImport p = do
    ls <- fmap (break (=="    ) where") . lines) $ readFile problemWrapper
    let edits = (unlines . fst) ls ++ "    , problem"
                ++ show p ++ "\n" ++ (unlines . snd) ls ++ "\n"
                ++ "problem" ++ show p ++ " :: IO ()\n"
                ++ "problem" ++ show p ++ " = eval " ++ show p
    removeFile problemWrapper
    appendFile problemWrapper edits

-- | Removes a problem from the C problem wrapper import list.
unwrapImport :: Int -> IO ()
unwrapImport p = do
    ls <- lines <$> readFile problemWrapper
    let edits = unlines $ filter
                (\l -> "problem" ++ show p `notElem` words l) ls
    removeFile problemWrapper
    appendFile problemWrapper edits

-- | Returns a list of problems that are completed.
lsComplete :: IO [Int]
lsComplete = sort . map read . filter (/= "") . lines
             <$> readFile dotComplete

-- | Returns a list of problems that are incomplete.
lsIncomplete :: IO [Int]
lsIncomplete = sort . map read . filter (/= "") . lines
               <$> readFile dotIncomplete

-- | Returns the number of problems that are marked complete.
countComplete :: IO Int
countComplete = length <$> lsComplete

-- | Returns the number of problems that are marked incomplete.
countIncomplete :: IO Int
countIncomplete = length <$> lsIncomplete
