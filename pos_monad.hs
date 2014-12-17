module PositionMonad where

import ErrorMonad

type Position = Int

data P a = P (Position -> E a)

instance Functor P where
	fmap f (P tr) = P (\p -> let ea = tr p in
							   ea >>= return.f)

applyPos p (P tr) = tr p

instance Monad P where
	return val = P (\p -> (return val))
	P tr >>= f = P (\p -> tr p >>= (\x -> applyPos p (f x)))

showpos p = show p

-- initial position: line 1
pos0 = 1

showP m = showE (m 0)

errorP s = P (\p -> errorE (showpos p ++ ": " ++ s))

resetP :: Position -> P x -> P x
resetP q m = P (\p -> applyPos q m)

