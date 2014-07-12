-- Aufgabe 12.14
-- a)
-- `filter2 [-1,0,1,6666,-5555]`

filter2 :: [Int] -> [Int]
filter2 []		= []
filter2 (x:xs)
	| x < 0 	= filter2 xs
	| otherwise = [x] ++ (filter2 xs)

-- b)
-- `count [-1,0,1,6666,-5555]`

count :: [a] -> Int
count []		= 0
count (x:xs)	= 1 + (count xs)

-- c)
-- `check [-1,0,1,6666,-5555,2,3]`
-- this would be True

check :: [Int] -> Bool
check l
	| count (filter2 l) >= 5	= True
	| otherwise 				= False

-- d)

data Tree a = Node (Tree a) (Tree a) | Leaf a deriving Show

-- An example
-- (Node (Leaf [1,2]) (Node (Leaf [2,3]) (Leaf [3,4])))
-- The number of elements in the list can also vary

-- e)
-- `trans (Node (Leaf [1,2]) (Node (Leaf [2,3]) (Leaf [3,4,4,4,5])))`

trans :: Tree [Int] -> Tree Bool
trans (Leaf l)		= Leaf (check l)
trans (Node t1 t2)	= Node (trans t1) (trans t2)
