-- Aufgabe 12.8

data Tree = Leaf Int | Branch Tree Tree

-- Tree with 5 Leafs
-- a)

{-
(Branch (Leaf 5) (Branch (Leaf 4) (Branch (Leaf 3) (Branch (Leaf 2)(Leaf 1)))))
-}

-- Graphical representation
{-
	Branch
	/	\
Leaf 5	Branch
		/	\
	Leaf 4	Branch
			/	\
		Leaf 3	Branch
				/	\
			Leaf 2	Leaf 1
-}
-- b)
-- 1. Write a function for counting the number of leafs
-- 2. Write a function which creates a list of the information stored on 
--    leafs (from left to right).

-- You can use the example tree to test these functions
leaf_count :: Tree -> Int
leaf_count (Leaf a) 		= 1
leaf_count (Branch t1 t2)	= (leaf_count t1) + (leaf_count t2)

leaf_content :: Tree -> [Int]
leaf_content (Leaf a) 		= a : []
leaf_content (Branch t1 t2)	= (leaf_content t1) ++ (leaf_content t2)
