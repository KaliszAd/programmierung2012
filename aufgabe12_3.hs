-- Aufgabe 12.3
-- a)

-- The length of the longest list

--[[1,2,3], [23,32], [5,7,6,4]] = 4


-- b)

max_length :: [[Int]] -> Int
max_length [] = -1
max_length (x:xs) = maximum' (list_length x) (max_length xs)

list_length :: [Int] -> Int
list_length []		= 0
list_length (x:xs)  = 1 + list_length xs

maximum' :: Int -> Int -> Int
maximum' x y
	| x > y 	= x
	| otherwise = y
