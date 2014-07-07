-- Aufgabe 12.4
-- a)

data Expr = TermType Term | NTermType Term | Sum Expr Term | Dif Expr Term deriving Show

data Term = FactorType Factor | Prod Term Factor deriving Show

data Factor = Lit Int | ExprType Expr deriving Show

-- b)
-- Usage in GHCi?
-- Simple example:
-- evalTerm (Prod (FactorType (Lit 4)) (Lit 1))
-- 
-- We can see, Prod needs some arguments, that are to be found in Term 
-- or Factor algebraic types respectivelly. So we need to give them to 
-- the function
--
-- Advanced:
-- evalTerm (Prod (FactorType (Lit 4)) (ExprType (TermType (FactorType 
-- (Lit 1)))))
--
-- That is actually the same, as the example before, we just do a 
-- round-trip trough other algebraic types
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
