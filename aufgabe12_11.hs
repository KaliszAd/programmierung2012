-- Aufgabe 12.11
-- a)
-- `check (Node 9 (Node 4 (Node 2 Nil Nil) (Node 3 Nil Nil)) (Node 7 
-- (Node 5 Nil Nil) (Node 6 Nil Nil)))`

data Tree = Node Int Tree Tree | Nil deriving Show

collapse :: Tree -> [Int]
collapse Nil 			= []
collapse (Node x t1 t2)	= (collapse t1) ++ [x] ++ (collapse t2)

check :: Tree -> Bool
check t = isOrdered (collapse t)

isOrdered :: [Int] -> Bool
isOrdered []			= True
isOrdered (x:[])		= True
isOrdered (x:(y:ys))	= (x <= y) && (isOrdered ys)

-- b)

insert :: Int -> Tree -> Tree
insert x Nil 			= Node x Nil Nil
insert x (Node n t1 t2)
	| x <= n 	= Node n (insert x t1) t2
	| otherwise = Node n t1 (insert x t2)

-- c)
-- This is the official solution, mine was to obscure

merge :: Tree -> Tree -> Tree
merge Nil t				= t
merge (Node x t1 t2) t	= merge t2 (merge t1 (insert x t))

{-
merge (Node 9 (Node 4 (Node 2 Nil Nil) (Node 3 Nil Nil)) (Node 7 (Node 5 Nil Nil) (Node 6 Nil Nil))) (Node 11 (Node 7 (Node 0 Nil Nil) (Node 1 Nil Nil)) (Node 4 (Node 3 Nil Nil) (Node 6 Nil Nil)))
-}
