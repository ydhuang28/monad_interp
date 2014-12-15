module ErrorMonad where

data E a = Success a | Error String deriving Show

instance Functor E where
	fmap f (Error s) = Error s
	fmap f (Success a) = Success (f a)

instance Monad E where
	return a = Success a
	Success a >>= f = f a
	Error s >>= f = Error s

errorE s = Error s

showE (Success a) = "Success: " ++ show a
showE (Error s) = "Error: " ++ s