-- Aufgabe 12.2
-- Liste f(1) = 1, f(2) = 1, f(3) = 1
-- f(i + 3) = f(i) + f(i + 1) + f(i + 2) für i >= 1

-- a)
-- f1 1 1 1 should generate the whished list
-- In ghci, it may be wise to write `take 10 (f1 1 1 1)`
-- as the list will continue to be printed in an endless loop
f1 :: Int ->  Int -> Int -> [Int]
f1 x y z = [x] ++ f1 y z (x+y+z) 

-- b)
-- The nth element out of the list `p n (f1 1 1 1)`
p :: Int -> [Int] -> Int
p 0 (x:xs)	= 0
p n (x:xs)
	| n == 1 = x
	| n >  1 = p (n - 1) xs

-- c)
-- Call by value
--
-- Call by name
--
-- Call by need
