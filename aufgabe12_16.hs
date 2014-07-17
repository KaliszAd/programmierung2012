-- Aufgabe 12.16
-- a)

-- Note that expo work only on natural numbers (non-negative)
-- `expo 0 0`, `expo 0 1`, `expo 2 3`

expo :: Int -> Int -> Int
expo x 0 = 1
expo 0 y = 0
expo x y
	| y > 0 = x * (expo x (y-1))

{-
Computation example:

expo 2 2 = 2 * (expo 2 1)
		 = 2 * 2 * (expo 2 0)
		 = 2 * 2 * 1
		 = 4
-}

-- b)
-- `check (Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 6)) k` mit 0<=k<=4 

data Tree a = Branch (Tree a) (Tree a) | Leaf a deriving Show

check :: Tree a -> Int -> Bool
check (Leaf a) k
	| k == 1	= True
	| k <= 0	= False
	| k > 1		= False
check (Branch t1 t2) k
	| (check t1 (k-1)) || (check t2 (k-1)) = True
	| otherwise = False

-- c)

