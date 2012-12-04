module Metacompiler.Main where

import Control.Monad (liftM)
import Control.Monad.State
import qualified Data.Map as M
import Metacompiler.ParseSExpr
import Metacompiler.SExpr
import Metacompiler.SExprToSL (errorContext)   -- TODO: Move `errorContext` into its own file
import Metacompiler.SExprToTL
import Metacompiler.TLRuntime
import System.Environment
import System.Exit
import System.IO

main = do
	filenames <- getArgs
	allDirectives <- liftM concat $ sequence [do
		hPutStrLn stderr ("note: reading " ++ filename)
		contents <- readFile filename
		let maybeDirectives = errorContext ("in " ++ show filename) $ do
			sexprs <- parseSExprs contents
			mapM parseTLDirectiveFromSExpr (sExprsToList sexprs)
		case maybeDirectives of
			Left err -> do
				hPutStrLn stderr "error:"
				hPutStrLn stderr err
				exitFailure
			Right d -> return d
		| filename <- filenames]
	case runStateT (compileDirectives allDirectives) initialCompileState of
		Left err -> do
			hPutStrLn stderr "error:"
			hPutStrLn stderr err
			exitFailure
		Right ((), finalCompileState) -> do
			hPutStrLn stderr ("note: found " ++ show (M.size (definitionsInCompileState finalCompileState)) ++ " definitions")
			forM_ (M.toList (definitionsInCompileState finalCompileState)) $ \ (name, rmo) -> do
				hPutStrLn stderr ("note:     " ++ name ++ " :: " ++ formatRMT (typeOfRMO rmo))
			hPutStrLn stderr ("note: found " ++ show (M.size (seenGlobalsInCompileState finalCompileState)) ++ " globals")
			when (null (emitsOfCompileState finalCompileState)) $ do
				hPutStrLn stderr "warning: nothing is being emitted because \
					\there are no `(emit ...)` or `(js-global ...)` \
					\constructs, so nothing is being emitted"
			putStrLn (renderStatements (emitsOfCompileState finalCompileState))
			exitSuccess

