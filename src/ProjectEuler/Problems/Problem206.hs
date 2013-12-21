-- NOT COMPLETED.
module Problems.Problem206
    ( problem206
    ) where

problem206 = [n | n <- [1020304050607080900..1929394959697989990], 
               isValid (show n) 0]
    where
	findX x
            | isValid (show (x^2)) 0 = x
            | otherwise = findX (x+1)
	isValid (n:ns) c
            | null (n:ns) = True
            | c == 0 && n == '1' = isValid ns (c+1)
            | c == 2 && n == '2' = isValid ns (c+1)
            | c == 4 && n == '3' = isValid ns (c+1)
            | c == 6 && n == '4' = isValid ns (c+1)
            | c == 8 && n == '5' = isValid ns (c+1)
            | c == 10 && n == '6' = isValid ns (c+1)
            | c == 12 && n == '7' = isValid ns (c+1)
            | c == 14 && n == '8' = isValid ns (c+1)
            | c == 16 && n == '9' = isValid ns (c+1)
            | c == 18 && n == '0' = isValid ns (c+1)
            | odd c = isValid ns (c+1)
            | otherwise = False 