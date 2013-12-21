-- |
-- Module      : ProjectEuler.C.Problems
-- Copyright   : Joe Jevnik 2013
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- A neat wrapper module for all problems completed in c.

module ProjectEuler.C.Problems
    ( problem1
    , problem2
    , problem3
    , problem5
    , problem15
    , problem6
    , problem4
    , problem20
    , problem13
    , problem11
    , problem18
    , problem67
    , problem19
    , problem81
    ) where

import Control.Applicative ((<$>))
import Data.List           (intersperse,isSuffixOf)
import System.Directory    (removeFile,getDirectoryContents)
import System.Process      (system,readProcess)

-- | Compiles the c problemP, prints the output then deletes the binary.
eval :: Int -> IO ()
eval p = do
    us <- concat . intersperse " " . map ("ProjectEuler/C/Utils/" ++)
          . filter (\p -> p `notElem` [".",".."] && ".c" `isSuffixOf` p)
                <$> getDirectoryContents "ProjectEuler/C/Utils/"
    system $ "gcc -o ProjectEuler/C/Problems/tempProc -std=c11"
               ++ " ProjectEuler/C/Problems/Problem" ++ show p ++ ".c " ++ us
               ++ " -lm -lgmp -Wall"
    readProcess "ProjectEuler/C/Problems/tempProc" [] [] >>= putStr
    removeFile "ProjectEuler/C/Problems/tempProc"

-- Automatically generated functions.

problem1 :: IO ()
problem1 = eval 1

problem2 :: IO ()
problem2 = eval 2

problem3 :: IO ()
problem3 = eval 3

problem5 :: IO ()
problem5 = eval 5

problem15 :: IO ()
problem15 = eval 15

problem6 :: IO ()
problem6 = eval 6

problem4 :: IO ()
problem4 = eval 4

problem20 :: IO ()
problem20 = eval 20

problem13 :: IO ()
problem13 = eval 13

problem11 :: IO ()
problem11 = eval 11

problem18 :: IO ()
problem18 = eval 18

problem67 :: IO ()
problem67 = eval 67

problem19 :: IO ()
problem19 = eval 19

problem81 :: IO ()
problem81 = eval 81
