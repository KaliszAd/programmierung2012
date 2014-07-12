-- Aufgabe 12.15
-- a)
-- `trans [(1,2),(444,444),(1,-2)]`

trans :: [(Int,Int)] -> [Int]
trans []			= []
trans ((x,y):xys)
	| x > y 		= (x - y) : [] ++ (trans xys)
	| y > x 		= (y - x) : [] ++ (trans xys)
	| otherwise 	= [(x + y)] ++ (trans xys)

-- b)
-- This is not done!

liste :: Int -> [Int] -> [Int]
liste n []		= []
liste n (x:xs)
	| n < x 	= [n] ++ xs
	| n == x	= liste n xs
	| n > x		= [x] ++ liste n xs

-- c)

data Tree a = Node (Tree a) (Tree a) Bool | Leaf a deriving Show

transtree :: Tree Bool -> Tree Bool
transtree (Leaf a)			= Leaf a
transtree (Node t1 t2 b)
	| (number_leafs t1) == (number_leafs t2)	= Node t1 t2 True
	| otherwise									= Node t1 t2 False

number_leafs :: Tree a -> Int
number_leafs (Leaf a)		= 1
number_leafs (Node t1 t2 b)	= (number_leafs t1) + (number_leafs t2)
