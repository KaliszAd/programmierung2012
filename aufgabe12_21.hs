-- Aufgabe 12.21
-- a)
-- `proglist (Node (1,'c') (Node (2,'a') Nil (Node (3,'b') Nil Nil)) 
-- (Node (4,'d') Nil Nil))`

data Tree a = Node (Int,a) (Tree a) (Tree a) | Nil deriving Show

proglist :: Tree a -> [a]
proglist Nil = []
proglist (Node (x,a) t1 t2) = (proglist t1) ++ [a] ++ (proglist t2)

-- b)
-- `insertone (2,'x') (Node (5,'c') (Node (1,'a') Nil (Node (3,'b') Nil 
-- Nil)) (Node (9,'d') Nil Nil))`

insertone :: (Int,a) -> Tree a -> Tree a
insertone d Nil = Node d Nil Nil
insertone (n,u) (Node (x,a) t1 t2)
	| n < x = Node (x,a) (insertone (n,u) t1) t2
	| n > x = Node (x,a) t1 (insertone (n,u) t2)
	| n == x = Nil -- this should signify an ilegal case

-- c)
-- `insert [(6,'x'),(7,'y')] (Node (5,'c') (Node (1,'a') Nil (Node 
-- (3,'b') Nil Nil)) (Node (9,'d') Nil Nil))`

insert :: [(Int,a)] -> Tree a -> Tree a 
insert [] t = t
insert (x:xs) t = insert xs (insertone x t)
