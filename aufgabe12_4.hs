-- Aufgabe 12.4
-- a)

data Expr = TermType Term | NTermType Term | Sum Expr Term | Dif Expr Term deriving Show

data Term = FactorType Factor | Prod Term Factor deriving Show

data Factor = Lit Int | ExprType Expr deriving Show

-- b)
-- Usage in GHCi? 
evalExpr :: Expr -> Int
evalExpr (TermType  a)	= evalTerm a
evalExpr (NTermType a) 	= - (evalTerm a)
evalExpr (Sum a b)		= (evalExpr a) + (evalTerm b)
evalExpr (Dif a b)		= (evalExpr a) - (evalTerm b)

evalFactor :: Factor -> Int
evalFactor (Lit a)		= a
evalFactor (ExprType a)	= evalExpr a

evalTerm :: Term -> Int
evalTerm (FactorType a)	= evalFactor a
evalTerm (Prod a b)		= (evalTerm a) * (evalFactor b)
