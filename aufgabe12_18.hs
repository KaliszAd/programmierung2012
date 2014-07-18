-- Aufgabe 12.18
data Tree t = Leaf t | Branch t (Tree t) (Tree t) deriving Show

-- a)
-- `liste (Branch 'b' (Leaf 'a') (Leaf 'c'))`
liste :: Tree t -> [t]
liste (Leaf t) = [t]
liste (Branch t t1 t2) = (liste t1) ++ [t] ++ (liste t2)

-- b)
-- x is of type Tree t
-- This is not done!
f :: t -> t

map f (liste x) = liste (g f x)

g :: (t -> r) -> Tree t -> Tree r
g f (Leaf t) = Leaf (f t)
g f (Branch t t1 t2) = Branch (f t) (f t1) (f t2)
