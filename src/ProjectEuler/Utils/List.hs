-- |
-- Module      : ProjectEuler.Utils.List
-- Copyright   : Joe Jevnik 2013
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- Functions acting on lists.

module ProjectEuler.Utils.List
    ( intToList -- :: (Show a,Integral a) => a -> [Int]
    , listToInt -- :: (Show a,Integral a,Read b,Integral b) => [a] -> b
    , elemCount -- :: Eq a => a -> [a] -> Int
    , splitOn   -- :: (a -> Bool) -> [a] -> [[a]]
    , headN     -- :: (Integral b, Integral a, Read b, Show a) => Int -> a -> b
    , lastN     -- :: (Integral a, Integral b) => b -> a -> a
    ) where

import Data.Char (digitToInt)

-- | Convert an Integral to a list of its digits. eg: intToList 123 = [1,2,3].
intToList :: (Show a,Integral a) => a -> [Int]
intToList = map digitToInt . show

-- | Inverse of intToList. Convert a list into an Integral where each element
-- is a digit. eg: listToInt [1,2,3] = 123.
listToInt :: (Show a,Integral a,Read b,Integral b) => [a] -> b
listToInt = read . concatMap show

-- | The number of times n occurs in list ns.
elemCount :: Eq a => a -> [a] -> Int
elemCount e = length . filter (== e)

-- | Splits a string into a list of strings at a given condition (wordsBy).
splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn p s =  case dropWhile p s of
    		    [] -> []
                    s' -> w : splitOn p s''
                        where
                            (w, s'') = break p s'

-- | Returns the first d digits of n.
headN :: (Integral b, Integral a, Read b, Show a) => Int -> a -> b
headN d = listToInt . take d . intToList

-- | Returns the last d digits of n.
lastN :: (Integral a, Integral b) => b -> a -> a
lastN d n = n `rem` 10^d
