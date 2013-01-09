module Test.Runtime where

import Control.Monad.State
import qualified Data.Map as M

import qualified Metacompiler.ParseSExpr
import qualified Metacompiler.Runtime as R
import qualified Metacompiler.SExprToTL
import qualified Metacompiler.SLCompile as SLC
import qualified Metacompiler.SLSyntax as SL
import qualified Metacompiler.TLCompile

dummyScope :: Metacompiler.TLCompile.Scope
dummyScope = Metacompiler.TLCompile.Scope
	M.empty
	(M.singleton (SL.NameOfType "Int") (SLC.TypeInScope [] (const $ R.MOSLTypeName (R.NameOfSLType "Int") R.SLKindType)))
	M.empty
	M.empty

testMetaObject :: String -> IO ()
testMetaObject string = do
	putStrLn ("------------------------------")
	putStrLn ("Testing: " ++ show string)
	case Metacompiler.ParseSExpr.parseSExprs string of
		Left err -> do
			putStrLn ("Parsing S-exprs: Failed.")
			putStrLn err
		Right sexprs -> do
			putStrLn ("Parsing S-exprs: OK.")
			case Metacompiler.SExprToTL.parseTLMetaObjectFromSExprs sexprs of
				Left err -> do
					putStrLn ("Converting to TL syntax: Failed. ")
					putStrLn err
				Right tlSyntax -> do
					putStrLn ("Converting to TL syntax: OK.")
					case runStateT (Metacompiler.TLCompile.compileMetaObject dummyScope tlSyntax) Metacompiler.TLCompile.LocalState of
						Left err -> do
							putStrLn ("Compiling TL: Failed.")
							putStrLn err
						Right (metaObject, _) -> do
							putStrLn ("Compiling TL: OK.")
							print metaObject
							print (R.reduceMetaObject metaObject)

main = do
	testMetaObject "a"
	testMetaObject "(sl-term (\\ (a :: Int) -> a))"


