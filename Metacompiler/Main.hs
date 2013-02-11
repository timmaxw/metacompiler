module Metacompiler.Main where

import Control.Monad (liftM)
import qualified Data.Map as M
import qualified Language.ECMAScript3.PrettyPrint as JS
import Metacompiler.ParseSExpr
import Metacompiler.SExpr
import Metacompiler.SExprToTL
import Metacompiler.SLCompile as SLC
import Metacompiler.SLSyntax as SLS
import Metacompiler.TLCompile as TLC
import Metacompiler.TLSyntax as TLS
import System.Environment
import System.Exit
import System.IO

main = do
	filenames <- getArgs
	allDirectives <- liftM concat $ sequence [do
		hPutStrLn stderr ("note: reading " ++ filename)
		contents <- readFile filename
		let maybeDirectives = do
			sexprs <- parseSExprs contents
			mapM parseTLDirectiveFromSExpr (sExprsToList sexprs)
		case maybeDirectives of
			Left err -> do
				hPutStrLn stderr "error:"
				hPutStrLn stderr err
				exitFailure
			Right d -> return d
		| filename <- filenames]
	case TLC.compileDirectives allDirectives of
		Left err -> do
			hPutStrLn stderr "error:"
			hPutStrLn stderr err
			exitFailure
		Right (TLC.GlobalResults (SLC.Defns slDataDefns slCtorDefns slTermDefns) tlDefns emits) -> do
			hPutStrLn stderr ("note: found " ++ show (M.size slDataDefns) ++ " SL data definition(s)")
			forM_ (M.toList slDataDefns) $ \ (name, _) -> do
				hPutStrLn stderr ("note:     " ++ SLS.unNameOfType name)
			hPutStrLn stderr ("note: found " ++ show (M.size slCtorDefns) ++ " SL ctor definition(s)")
			forM_ (M.toList slCtorDefns) $ \ (name, _) -> do
				hPutStrLn stderr ("note:     " ++ SLS.unNameOfCtor name)
			hPutStrLn stderr ("note: found " ++ show (M.size slTermDefns) ++ " SL term definition(s)")
			forM_ (M.toList slTermDefns) $ \ (name, _) -> do
				hPutStrLn stderr ("note:     " ++ SLS.unNameOfTerm name)
			hPutStrLn stderr ("note: found " ++ show (M.size tlDefns) ++ " TL meta-object definition(s)")
			forM_ (M.toList tlDefns) $ \ (name, _) -> do
				hPutStrLn stderr ("note:     " ++ TLS.unName name)
			when (null emits) $ do
				hPutStrLn stderr "warning: there is no JS code to emit"
			putStrLn (JS.renderStatements emits)
			exitSuccess

