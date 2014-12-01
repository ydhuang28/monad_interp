module CoreInterp where

type Name = String

data Term = Var Name
		  | Con Int 
		  | Add Term Term
		  | Lam Term Term
		  | App Term Term

data Value = Wrong
		   | Num Int
		   | Monad m => Fun (Value -> m Value)

type Environment = [(Name, Value)]

showval :: Value -> String
showval Wrong = "error"
showval (Num i) = show i
showval (Fun f) = "<function>"

interp :: (Monad m) => Term -> Environment -> m Value
interp (Var x) e = lookup x e
interp (Con i) e = return (Num i)
interp (Add u v) e = do
					 a <- interp u e
					 b <- interp v e
					 add a b
interp (Lam x v) e = return (Fun (\a -> interp v ((x, a):e)))
interp (App t u) e = do
					 f <- interp t e
					 a <- interp u e
					 apply f a
