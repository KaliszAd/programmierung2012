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
