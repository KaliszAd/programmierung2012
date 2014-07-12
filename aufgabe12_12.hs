-- Aufgabe 12.12

data Tree = Leaf Char | Branch2 Char Tree Tree | Branch3 Char Tree Tree Tree deriving Show

{- 
 - Test with `depth (Branch3 'a' (Branch2 'b' (Leaf 'c') (Leaf 'd')) 
 - (Branch2 'e' (Leaf 'f') (Leaf 'g')) (Leaf 'h'))` e.g.
 -}


depth :: Tree -> Int
depth (Leaf a)				= 1
depth (Branch2 a t1 t2)		= 1 + (max' (depth t1) (depth t2))
depth (Branch3 a t1 t2 t3)	= 1 + (max' (depth t1) (max' (depth t2) (depth t3)))

max' :: Int -> Int -> Int
max' a b 
	| a <= b	= b
	| otherwise = a
