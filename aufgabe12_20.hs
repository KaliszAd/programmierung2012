-- Aufgabe 12.20
-- a)
-- `compare' a b` where a = [] or [1,2] and b = [] or [1,2]
-- try combinations

compare' :: [Int] -> [Int] -> Bool
compare' [] [] = True
compare' _ [] = False
compare' [] _ = False
compare' (a:as) (b:bs)
	| a == b && (compare' as bs) = True
	| otherwise = False

-- b)
-- `merge [1,2,3] [2,3,4]` mergin of sorted lists
merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge (a:as) [] = (a:as)
merge [] (b:bs) = (b:bs)
merge (a:as) (b:bs)
	| a <= b = a : b : (merge as bs)
	| a > b = b : a : (merge as bs)

-- c)
-- The Tree starts with TreeB (as advised in the solution)
data TreeI = LeafI Int | NodeI Int TreeB TreeB deriving Show
data TreeB = LeafB Int | NodeB Bool TreeI TreeI deriving Show
