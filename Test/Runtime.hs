module Test.Runtime where

import Control.Monad.State
import qualified Data.Map as M

import qualified Metacompiler.ParseSExpr
import qualified Metacompiler.Runtime as R
import Metacompiler.SExpr
import qualified Metacompiler.SExprToTL
import qualified Metacompiler.SLCompile as SLC
import qualified Metacompiler.SLSyntax as SL
import qualified Metacompiler.TLCompile

testDirectives :: String -> IO (Maybe Metacompiler.TLCompile.Scope)
testDirectives string = do
	putStrLn ("------------------------------")
	putStrLn ("Testing directives.")
	case Metacompiler.ParseSExpr.parseSExprs string of
		Left err -> do
			putStrLn ("Parsing S-exprs: Failed.")
			putStrLn err
			return Nothing
		Right sexprs -> do
			putStrLn ("Parsing S-exprs: OK.")
			case mapM Metacompiler.SExprToTL.parseTLDirectiveFromSExpr (sExprsToList sexprs) of
				Left err -> do
					putStrLn ("Converting to TL syntax: Failed. ")
					putStrLn err
					return Nothing
				Right tlSyntax -> do
					putStrLn ("Converting to TL syntax: OK.")
					case Metacompiler.TLCompile.compileDirectives tlSyntax of
						Left err -> do
							putStrLn ("Compiling TL: Failed.")
							putStrLn err
							return Nothing
						Right globalResults -> do
							putStrLn ("Compiling TL: OK.")
							return (Just (Metacompiler.TLCompile.scopeOfGlobalResults globalResults))

testMetaObject :: Metacompiler.TLCompile.Scope -> String -> IO ()
testMetaObject scope string = do
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
					case runStateT (Metacompiler.TLCompile.compileMetaObject scope tlSyntax) Metacompiler.TLCompile.LocalState of
						Left err -> do
							putStrLn ("Compiling TL: Failed.")
							putStrLn err
						Right (metaObject, _) -> do
							putStrLn ("Compiling TL: OK.")
							print metaObject
							print (R.reduceMetaObject metaObject)

main = do
	Just testScope <- testDirectives "\
		\(sl-code (data Nat = (Zero) (Succ Nat)))\
		\"
	testMetaObject testScope "a"
	testMetaObject testScope "(sl-term (\\ (a :: Nat) -> a))"
	testMetaObject testScope "(\\ (b' :: sl-type *) (c :: sl-type *) (x' :: sl-term b') -> \
		\(sl-term ((\\(a :: b) -> a) x) (type \"b\" = b') (term \"x\" = x')) \
		\)"

