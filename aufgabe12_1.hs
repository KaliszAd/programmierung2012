-- Aufgabe 12.1
-- | The 'f' function returns a reversed list of non-zero integers
--
f :: [Int] -> [Int]
f []			= []
f (x:xs)
	| x > 0 	= f xs ++ [x]
	| otherwise = f xs
