module CoreInterp where

type Name = String

data Term = Var Name
		  | ConI Int
		  | ConS String
		  | Add Term Term
		  | Lam Name Term
		  | App Term Term

data Value = Wrong
		   | Num Int
		   | Str String
		   | Fun (Value -> Value)--should eventually be like V -> m V

type Environment = [(Name, Value)]

showval :: Value -> String
showval Wrong = "error"
showval (Num i) = show i
showval (Str s) = s
showval (Fun f) = "<function>"

interp :: (Monad m) => Term -> Environment -> m Value
interp (Var x) e = look_up x e
interp (ConI i) e = return (Num i)
interp (ConS s) e = return (Str s)
interp (Add u v) e = do
					 a <- interp u e
					 b <- interp v e
					 add a b
interp (Lam x v) e = return (Fun (\a -> interp v ((x,a):e)))
interp (App t u) e = do
					 f <- interp t e
					 a <- interp u e
					 apply f a
--I don't think we interp Cat, I think we apply cat

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

cat :: (Monad m) => Value -> Value -> m Value
cat (Str s1) (Str s2) = return (s1 ++ s2)
cat _ _ = return Wrong

