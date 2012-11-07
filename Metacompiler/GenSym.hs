module Metacompiler.GenSym where

data GenSym a = GenSym { internalStateOfGenSym :: State [String] a }

instance Monad GenSym where
	return x = GenSym (return x)
	a >>= b = GenSym (internalStateOfGenSym a >>= (internalStateOfGenSym . b))

genSym :: GenSym String
genSym = GenSym $ do
	(symbol:rest) <- getState
	putState rest
	return symbol

