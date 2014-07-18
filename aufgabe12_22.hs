-- Aufgabe 12.22

data Tree = Node Int Tree Tree | Nil deriving Show

-- a)
-- `insert Nil [1,2,3]`
insert :: Tree -> [Int] -> Tree
insert t []	= t
insert Nil (x:xs) = insert (Node x Nil Nil) xs
insert (Node n t1 t2) (x:xs)
	| n > x = insert (Node n (insert t1 [x]) t2) xs
	| n < x = insert (Node n t1 (insert t2 [x])) xs
	| n == x = Nil -- this is a no-go case

-- b)
-- `test_tree (Node 1 Nil (Node 2 Nil (Node 3 Nil Nil))) (Node 1 Nil 
-- (Node 2 Nil (Node 3 Nil Nil)))`
test_tree :: Tree -> Tree -> Bool
test_tree Nil Nil = True
test_tree Nil _ = False
test_tree _ Nil = False
test_tree (Node a ta1 ta2) (Node b tb1 tb2)
	| a == b && (test_tree ta1 tb1) && (test_tree ta2 tb2) = True
	| otherwise = False
