module CoreInterp where

import ErrorMonad

type Name = String

data Term = Var Name
		  | ConI Int
		  | ConS String
		  | ConC Char
		  | ConL [Value]
		  | Add Term Term
		  | Cat Term Term
		  | Lam Name Term
		  | App Term Term

data Value = Num Int
		   | Str String
		   | Ch Char
		   | List [Value]
		   | Fun (Value -> E Value)

instance Show Value where
	show (Num n) = show n
	show (Str s) = show s
	show (Ch c) = show c
	show (List l) = show l
	show (Fun f) = "<function>"

type Environment = [(Name, Value)]

interp :: Term -> Environment -> E Value
interp (Var x) e = look_up x e
interp (ConI i) e = return (Num i)
interp (ConS s) e = return (Str s)
interp (ConC c) e = return (Ch c)
interp (ConL l) e = return (List l)
interp (Add u v) e = do
					 a <- interp u e
					 b <- interp v e
					 add a b
interp (Lam x v) e = return (Fun (\a -> interp v ((x,a):e)))
interp (App t u) e = do
					 f <- interp t e
					 a <- interp u e
					 apply f a
interp (Cat u v) e = do
					 s1 <- interp u e
					 s2 <- interp v e
					 cat s1 s2

look_up :: Name -> Environment -> E Value
look_up x [] = errorE ("variable not in scope: " ++ x)
look_up x ((y,b):e) = if x == y
					  then return b
					  else look_up x e

add :: Value -> Value -> E Value
add (Num i) (Num j) = return (Num (i + j))
add a b = errorE ("should be numbers: " ++ show a ++ ", " ++ show b)

apply :: Value -> Value -> E Value
apply (Fun k) a = k a
apply f a = errorE ("should be function: " ++ show f)

cat :: Value -> Value -> E Value
cat (Str s1) (Str s2) = return (Str (s1 ++ s2))
cat (List l1) (List l2) = return (List (l1 ++ l2))
cat a b = errorE ("should either be both strings or both lists: " ++ show a ++ ", " ++ show b)

t1 = do
	 x <- interp (Var "me") [("me", (Num 21))]
	 interp (Cat (ConL [x, (Num 3)]) (ConL [(Num 1), (Num 20)])) []
