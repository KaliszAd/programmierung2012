-- Aufgabe 12.5
-- a)

data Term = Lit Int | Add Term Term | Mult Term Term deriving Show

-- b)
-- This is the solution given in the text book all credit to the author

eval' :: Term -> Term
eval' t		= Lit (eval'' t)

eval'' :: Term -> Int
eval'' (Add t1 t2) 	= (eval'' t1) + (eval'' t2)
eval'' (Mult t1 t2)	= (eval'' t1) * (eval'' t2)
eval'' (Lit a) 		= a

-- c)

transform :: [Term] -> [Term]
transform []		= 	[]
transform (x:xs)	= 	if (eval'' x) < 0 
						then (transform xs)
						else ((eval' x) : (transform xs))
