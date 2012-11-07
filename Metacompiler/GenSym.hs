module Metacompiler.GenSym where

-- The `GenSym` monad is a thin wrapper around the `State` monad that allows
-- generating unique string identifiers. Currently, they are of the form "_1",
-- "_2", "_3", etc.

import Control.Monad.State

data GenSym a = GenSym { internalStateOfGenSym :: State [String] a }

instance Monad GenSym where
	return x = GenSym (return x)
	a >>= b = GenSym (internalStateOfGenSym a >>= (internalStateOfGenSym . b))

genSym :: GenSym String
genSym = GenSym $ do
	(symbol:rest) <- get
	put rest
	return symbol

runGenSym :: GenSym a -> a
runGenSym (GenSym s) = evalState s ["_" ++ show i | i <- [1..]]

