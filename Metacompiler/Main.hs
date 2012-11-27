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
	case runStateT (mapM_ processDirective allDirectives) emptyResults of
		Left err -> do
			hPutStrLn stderr "error:"
			hPutStrLn stderr err
			exitFailure
		Right ((), results) -> do
			hPutStrLn stderr ("note: found " ++ show (M.size (definitionsInResults results)) ++ " definitions")
			forM_ (M.toList (definitionsInResults results)) $ \ (name, rmo) -> do
				hPutStrLn stderr ("note: " ++ show name ++ " :: " ++ formatRMT (typeOfRMO rmo))
			when (null (emittedCodeOfResults results)) $ do
				hPutStrLn stderr "warning: there are no emit-directives, or all emit-directives are empty"
			putStrLn (emittedCodeOfResults results)
			exitSuccess

