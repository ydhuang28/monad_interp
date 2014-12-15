module PositionMonad where

type Position = Int

type P a = Position -> E a

instance Functor P where
	fmap f p = 

instance Monad P where
	return val = \p -> (return val)
	ma >>= f = \p -> ma p >>= (\x -> f x p)

showpos p = show p

showP m = showE (m pos0)

errorP s = \p -> errorE (showpos p ++ ": " ++ s)

resetP :: Position -> P x -> P x
resetP q m = \p -> m q

