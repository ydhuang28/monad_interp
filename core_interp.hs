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
		   | Fun (Value -> E Value)--should eventually be like V -> m V

type Environment = [(Name, Value)]

data E a = Success a | Error String deriving Show

instance Monad E where
	return a = Success a
	Success a >>= f = f a
	Error s >>= f = Error s

errorE s = Error s

showval :: Value -> String
showval Wrong = "error"
showval (Num i) = show i
showval (Str s) = s
showval (Fun f) = "<function>"

interp :: Term -> Environment -> E Value
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
--I don't think we interp Cat, I think we do apply((Fun cat) a)

look_up :: Name -> Environment -> E Value
look_up x [] = errorE ("variable not in scope: " ++ x)
look_up x ((y,b):e) = if x == y
					  then return b
					  else look_up x e

add :: Value -> Value -> E Value
add (Num i) (Num j) = return (Num (i + j))
add a b = errorE ("should be numbers: " ++ showval a ++ ", " ++ showval b)

apply :: Value -> Value -> E Value
apply (Fun k) a = k a
apply f a = errorE ("should be function: " ++ showval f)

cat :: Value -> Value -> E Value
cat (Str s1) (Str s2) = return (Str (s1 ++ s2))
cat a b = errorE ("should be strings: " ++ showval a ++ ", " ++ showval b)

