-- Aufgabe 12.9
-- a)

data TA = NilA | A TA TB deriving Show

data TB = NilB | B TB TA deriving Show

-- b)
-- Check with `zahlB (A NilA (B NilB NilA))` e.g.

zahlB :: TA -> Int
zahlB NilA 			= 0
zahlB (A ta tb)		= (zahlB ta) + (zahlB' tb)

zahlB' :: TB -> Int
zahlB' NilB			= 1
zahlB' (B tb ta)	= (zahlB' tb) + (zahlB ta) + 1
