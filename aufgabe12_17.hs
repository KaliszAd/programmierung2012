-- Aufgabe 12.17

data Person = PER Name Geschlecht Kinder deriving Show
type Name = String
type Kinder = [Person]

-- a)
benenne_um :: Person -> Name -> Person
benenne_um (PER _ geschlecht kinder) name = (PER name geschlecht kinder)

-- b)
data Geschlecht = Weiblich | Maennlich deriving Show

weiblich :: Person -> Bool
weiblich (PER _ Weiblich _) = True
weiblich (PER _ Maennlich _) = False

{- In the solution, it is written, the "==" operator can only be used, 
 - if "deriving (Show, Eq)" is written in the algebraic data type
 -}

-- c)
nehme :: (Person -> Bool) -> [Person] -> [Person]
nehme _ [] = []
nehme person (x:xs)
	| (person x) = [x] ++ (nehme person xs)
	| otherwise = nehme person xs

-- The solution uses an if-then-else construct

toechter :: Person -> [Person]
toechter (PER x y kinder) = nehme weiblich kinder

-- d)
addiere :: [Int] -> Int
addiere [] = 1
addiere (x:xs) = x + (addiere xs)

nachfahren :: Person -> Int
nachfahren (PER x y kinder) = addiere (map nachfahren kinder)
