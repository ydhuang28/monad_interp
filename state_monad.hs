module StateMonad where
type State = Integer
data ST a = St (State -> (a, State))

instance Functor ST where
	fmap f (St g) = St ((help f).g)

help f (a, s) = (f a, s)

instance Monad ST where
	return a = St(\s -> (a,s))
	St(f) >>= g = St(\s0 -> let (b,s1) = f s0 in
		let St(h) = g b in
			h s0)

showST (St(t)) x = t x