-- Aufgabe 12.19
-- a)

addL  :: [Int] -> Int
addL [] = 0
addL (x:xs) = x + (addL xs)

-- b)
-- `addU (UI [(UL 1), (UI [(UL 2), (UL 5)]), (UL 7)])`
-- (x:xs) can be "list" as well in this instance

data UTree = UI [UTree] | UL Int deriving Show

addU :: UTree -> Int
addU (UI []) = 0
addU (UL i)	= i
addU (UI (x:xs)) = addL (map addU (x:xs))

-- c)

data Tree a = TI  (Tree a) (Tree a) (Tree a) | TL a deriving Show

mapTree :: (a -> b) -> Tree a -> Tree b
