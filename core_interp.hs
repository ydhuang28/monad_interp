module CoreInterp where

type Name = String

data Term = Var Name
		  | Con Int 
		  | Add Term Term
		  | Lam Term Term
		  | App Term Term

data Value = Wrong
		   | Num Int
		   | Fun (Value -> (Monad Value))

type Environment = [(Name, Value)]

showval :: Value -> String
showval Wrong = "error"
showval (Num i) = show i
showval (Fun f) = "<function>"

interp :: (Monad m) => Term -> Environment -> m Value
interp (Var x) e = look_up x e
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

look_up :: (Monad m) => Name -> Environment -> m Value
look_up x [] = return Wrong
look_up x ((y,b):e) = if x == y
	then return b
	else look_up x e

add :: (Monad m) => Value -> Value -> m Value
add (Num i) (Num j) = return (Num (i + j))
add _ _ = return Wrong

apply :: (Monad m) => Value -> Value -> m Value
apply (Fun k) a = k a
apply _ a = return Wrong



