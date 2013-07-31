module Utils.List 
    ( int_to_list
    , list_to_int
    , elem_count
    , split_on
    ) where
    
import Data.Char
import Data.List
            
-- Convert an Integral to a list of its digits. eg: int_to_list 123 = [1,2,3].
int_to_list n = map digitToInt $ show n

-- Inverse of int_to_list. Convert a list into an Integral where each element 
-- is a digit. eg: list_to_int [1,2,3] = 123.
list_to_int ns = read $ concatMap show ns :: Integer

-- The number of times n occurs in list ns.
elem_count :: Eq a => a -> [a] -> Int 
elem_count n ns =  elem_count' n ns 0
    where
	elem_count' n ns c
	    | null ns = c
	    | head ns == n = elem_count' n (tail ns) (c+1)
	    | otherwise = elem_count' n (tail ns) c
                          
-- Splits a string into a list of strings at a given condition (wordsBy).
split_on :: (Char -> Bool) -> String -> [String]
split_on p s =  case dropWhile p s of
    		    "" -> []
                    s' -> w : split_on p s''
                        where (w, s'') = break p s'