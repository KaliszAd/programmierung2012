-- Aufgabe 12.10
-- a)

data TA = NilA | A Int TA TB deriving Show

data TB = NilB | B Int TB TA deriving Show

-- b)
-- Check with `trans (A 0 (A 1 NilA NilB) (B 1 NilB NilA))` e.g.
-- Zero should become 1 and 1 should become 0 in this instance

trans :: TA -> TA
trans NilA 			= NilA
trans (A n ta tb)	= A ((countB ta) + (countB' tb)) (trans ta) (trans' tb)

trans' :: TB -> TB
trans' NilB 		= NilB
trans' (B n tb ta)	= B ((countB' tb) + (countB ta)) (trans' tb) (trans ta)

countB :: TA -> Int
countB NilA			= 0
countB (A n ta tb)	= (countB ta) + (countB' tb)

countB' :: TB -> Int
countB' NilB		= 0
countB' (B n tb ta)	= 1 + (countB' tb) + (countB ta)

{-

The solution in the book is more efficient

trans :: TA -> TA
trans t = atrans t 0

atrans :: TA -> Int -> TA
atrans NilA _ 			= NilA
atrans (A _ ta tb) n 	= (A n (atrans ta n) (btrans tb n))

btrans :: TB -> Int -> TB
btrans NilB _ 			= NilB
btrans (B _ tb ta) n	= (B (n + 1) (btrans tb (n + 1)) (atrans ta (n + 1)))
-- Or without the change of the value stored on the B-Branch
btrans (B val tb ta) n	= (B val (btrans tb (n + 1)) (atrans ta (n + 1)))

-}

-- c)
-- These functions save all Integers in the Tree in a list

list_naive :: TA -> [Int]
list_naive NilA			= []
list_naive (A n ta tb)	= (list ta) ++ [n] ++ (list' tb)

list_naive' :: TB -> [Int]
list_naive' NilB		= []
list_naive' (B n tb ta)	= (list' tb) ++ [n] ++ (list ta)

-- This is the actual solution, it saves only the values stored in a 
-- trees

list :: TA -> [Int]
list NilA			= []
list (A n ta tb)	= [n] ++ (list ta) ++ (list' tb)
-- I think, here it should be (list ta) ++ [n] ++ (list' tb)
-- but that isn't in the solution

list' :: TB -> [Int]
list' NilB			= []
list' (B n tb ta)	= (list' tb) ++ (list ta)
