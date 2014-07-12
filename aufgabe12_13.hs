-- Aufgabe 12.13
-- a)
-- `check (Branch (Leaf True) (Branch (Leaf True) (Branch (Leaf True) 
-- (Branch (Leaf True) (Leaf False)))))`

data Tree a = Branch (Tree a) (Tree a) | Leaf a deriving Show

check :: Tree Bool -> Bool
check (Leaf False)		= True
check (Leaf True)		= False
check (Branch t1 t2)
	| (check t1) || (check t2) 	= True
	| otherwise 				= False

-- b)
-- `toList (Branch (Leaf 1) (Branch (Leaf 2) (Branch (Leaf 3) (Branch 
-- (Leaf 
-- 4) (Leaf 42)))))`

toList :: Tree Int -> [Int]
toList (Leaf n)			= [n]
toList (Branch t1 t2)	= (toList t2) ++ (toList t1)

-- c)
-- `toTree [1,2,3,4]`

toTree :: [Int] -> Tree Int
toTree []		= Leaf 42
toTree (x:xs)	= Branch (Leaf x) (toTree xs)

-- d)
-- `transform [False, True]`

transform :: [Bool] -> [Bool]
transform []		= []
transform (x:xs)
	| x == False	= transform xs
	| x == True		= True : True : [] ++ transform xs
