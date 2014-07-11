-- Aufgabe 12.9
-- a)

data TA = Nil | A TA TB deriving Show

data TB = Nil | B TB TA

-- b)

zahlB :: TA -> Int
zahlB Nil 			= 0
zahlB (A ta tb)		= (zahlB ta) + (zahlB' tb)

zahlB' :: TB -> Int
zahlB' Nil			= 1
zahlB' (B tb ta)	= (zahlB' tb) + (zahlB ta) + 1
