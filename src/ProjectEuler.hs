-- |
-- Module      : ProjectEuler
-- Copyright   : Joe Jevnik 2013
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- An interface for working on project euler in Haskell.

module ProjectEuler where

import Control.Applicative ((<$>))
import Control.Monad       (unless,void)
import Data.List           (sort,isInfixOf,stripPrefix)
import Data.Maybe          (fromMaybe)
import System.Directory    (removeFile)
import System.Process      (system)

import ProjectEuler.Problems
import ProjectEuler.Utils
import qualified ProjectEuler.C as C

-- | The file used to neatly wrap all problems to be imported by the main
-- ProjectEuler module.
problemWrapper :: FilePath
problemWrapper = "ProjectEuler/Problems.hs"

-- | The file containing the list of completed problems.
dotComplete :: FilePath
dotComplete = "ProjectEuler/Problems/.complete"

-- | The file containing the list of incomplete problems.
dotIncomplete :: FilePath
dotIncomplete = "ProjectEuler/Problems/.incomplete"

-- | Displays some basic information about my work.
main :: IO ()
main = putStrLn "Project Euler Work by Joe Jevnik"

-- | Checks the completion status of problemP
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

-- | Opens the given problem in a new emacs window or
-- starts a new problem with the template if it does not exist.
-- WARNING: Sending interupts to ghci will close the emacs window.
openProblem :: Int -> IO ()
openProblem p = do
    s <- checkStatus p
    if s `elem` ["Complete","Incomplete"]
      then void (system $ "emacs " ++ prob p ++ ".hs")
      else do
          putStr $ "Problem " ++ show p
                ++ " has not been started, Would you like to start it (Y/n):"
          inp <- getLine
          unless (inp `elem` ["n","N"])
                     $ (system ("echo \"" ++ problemTemplate p
                                ++ "\" > " ++ prob p ++ ".hs"))
                     >> (system $ "emacs " ++ prob p ++ ".hs")
                     >> appendFile dotIncomplete (show p)
                     >> wrapImport p >> markIncomplete p
  where
      prob = (++) "ProjectEuler/Problems/Problem" . show
      problemTemplate n =
          "-- NOT YET COMPLETED.\nmodule ProjectEuler.Problems.Problem"
          ++ show n ++ "\n    ( problem" ++ show n
                 ++ " -- :: IO ()\n    ) where\n\nproblem" ++ show n
                        ++ " :: IO ()\nproblem" ++ show n ++ " = return ()"

-- | Marks problemP as complete.
markComplete :: Int -> IO ()
markComplete p = do
    cs <- lines <$> readFile dotComplete
    ws <- lines <$> readFile dotIncomplete
    unless (show p `elem` cs) $ do
                     let ws' = filter (/= show p) ws
                     removeFile dotIncomplete
                     appendFile dotIncomplete $ unlines ws'
                     appendFile dotComplete   $ '\n':show p

-- | Marks problemP as incomplete
markIncomplete :: Int -> IO ()
markIncomplete p = do
    cs <- lines <$> readFile dotComplete
    ws <- lines <$> readFile dotIncomplete
    unless (show p `elem` ws) $ do
                       let cs' = filter (/= show p) cs
                       removeFile dotComplete
                       appendFile dotComplete   $ unlines cs'
                       appendFile dotIncomplete $ '\n':show p

-- | Marks a problem as not yet started.
markNotStarted :: Int -> IO ()
markNotStarted p = do
    cs <- lines <$> readFile dotComplete
    ws <- lines <$> readFile dotIncomplete
    let cs' = filter (/= show p) cs
        ws' = filter (/= show p) ws
    mapM removeFile [dotIncomplete,dotComplete]
    appendFile dotComplete   $ unlines cs'
    appendFile dotIncomplete $ unlines ws'

-- | Adds a problem to the ProblemWrapper list.
wrapImport :: Int -> IO ()
wrapImport p = do
    appendFile problemWrapper $ "\nimport ProjectEuler.Problems.Problem"
                   ++ show p ++ " as M"

-- | Removes a problem from the ProblemWrapper import list.
unwrapImport :: Int -> IO ()
unwrapImport p = do
    ls <- lines <$> readFile problemWrapper
    let edits = unlines $ filter
                 (\l -> "ProjectEuler.Problems.Problem" ++ show p
                       `notElem` words l) ls
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
