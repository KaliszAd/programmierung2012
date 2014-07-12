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
-- This one is a bit tricky because of the last case, when there is no x 
-- higher than n. That is the 'liste n []' case.
-- `liste (a) [0,1,2,3,4,5,6]` for a, try lower number, than contained 
-- in the list, number contained in the list and number higher than 
-- contained in the list

liste :: Int -> [Int] -> [Int]
liste n []		= [n]
liste n (x:xs)
	| n < x 	= [n] ++ [x] ++ xs
	| n == x	= []  ++ xs
	| n > x		= [x] ++ (liste n xs) 

-- c)
-- `transtree (Node (Leaf 1) (Node (Leaf 2) (Leaf 3) False) False)`

data Tree a = Node (Tree a) (Tree a) Bool | Leaf a deriving Show

transtree :: Tree a -> Tree a
transtree (Leaf a)			= Leaf a
transtree (Node t1 t2 b)
	| (number_leafs t1) == (number_leafs t2)	= Node (transtree t1) (transtree t2) True
	| otherwise									= Node (transtree t1) (transtree t2) False

number_leafs :: Tree a -> Int
number_leafs (Leaf a)		= 1
number_leafs (Node t1 t2 b)	= (number_leafs t1) + (number_leafs t2)
