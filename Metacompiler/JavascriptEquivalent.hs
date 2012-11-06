module Metacompiler.JavascriptEquivalent where

import Control.Monad.State
import qualified Data.Map as M
import Language.Javascript.Parser
import qualified Metacompiler.TL as TL

data GenSym a = GenSym { internalStateOfGenSym :: State [String] a }

instance Monad GenSym where
	return x = GenSym (return x)
	a >>= b = GenSym (internalStateOfGenSym a >>= (internalStateOfGenSym . b))

genSym :: GenSym String
genSym = GenSym $ do
	(symbol:rest) <- getState
	putState rest
	return symbol

javascriptEquivalentOfTerm :: M.Map String MetaObject a
                           -> M.Map String JSNode
                           -> MetaObject a
                           -> State Int JSNode
javascriptEquivalentOfTerm vars obj = case reduceTerm obj of
	TL.MOJSExpr { TL.implOfMetaObject = block } -> expandJavascriptBlock vars block
	TL.MOJSSubstitution { TL.jsSubstitutionOfMetaObject = code } -> code
	TL.MOAbs { } -> error "trying to take JS equivalent of 
	_ -> error "reduceTerm returned unreduced term, wtf?"



