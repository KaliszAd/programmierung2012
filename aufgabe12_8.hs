-- Aufgabe 12.8

data Tree = Leaf Int | Branch Tree Tree

-- Tree with 5 Leafs
-- a)

(Branch (Leaf 5) (Branch (Leaf 4) (Branch (Leaf 3) (Branch (Leaf 2) (Leaf 1)))))

-- Graphical representation

	Branch
	/	\
Leaf	Branch
		/	\
	Leaf	Branch
			/	\
		Leaf	Branch
				/	\
			Leaf	Leaf

-- b)
-- 1. Write a function for counting the number of leafs
-- 2. Write a function which creates a list of the information stored on 
--    leafs (from left to right).
