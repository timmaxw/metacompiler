module Metacompiler.Main where

import Control.Monad
import qualified Data.Map as M
import Metacompiler.Error
import qualified Metacompiler.JS.JS as JS
import Metacompiler.SExpr.Parse
import Metacompiler.SExpr.Types
import qualified Metacompiler.SLCompile.Compile as SLC
import qualified Metacompiler.SLSyntax.Types as SLS
import qualified Metacompiler.TLCompile.Compile as TLC
import qualified Metacompiler.TLCompile.Format as TLF
import qualified Metacompiler.TLRuntime.TLRuntime as TLR
import qualified Metacompiler.TLSyntax.FromSExpr as TLS
import qualified Metacompiler.TLSyntax.Types as TLS
import qualified Metacompiler.TLSyntax.ToSExpr as TLS
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
			mapM TLS.parseTLDirectiveFromSExpr (sExprsToList sexprs)
		case maybeDirectives of
			Failure err -> do
				hPutStrLn stderr "error:"
				hPutStrLn stderr err
				exitFailure
			Success d -> return d
		| filename <- filenames]
	case TLC.compileDirectives allDirectives of
		Failure err -> do
			hPutStrLn stderr "error:"
			hPutStrLn stderr err
			exitFailure
		Success (TLC.GlobalResults (SLC.Defns slDataDefns slCtorDefns slTermDefns) tlDefns emits) -> do
			hPutStrLn stderr ("note: found " ++ show (M.size slDataDefns) ++ " SL data definition(s)")
			forM_ (M.toList slDataDefns) $ \ (name, _) -> do
				hPutStrLn stderr ("note:     " ++ SLS.unNameOfType name)
			hPutStrLn stderr ("note: found " ++ show (M.size slCtorDefns) ++ " SL ctor definition(s)")
			forM_ (M.toList slCtorDefns) $ \ (name, _) -> do
				hPutStrLn stderr ("note:     " ++ SLS.unNameOfTerm name)
			hPutStrLn stderr ("note: found " ++ show (M.size slTermDefns) ++ " SL term definition(s)")
			forM_ (M.toList slTermDefns) $ \ (name, _) -> do
				hPutStrLn stderr ("note:     " ++ SLS.unNameOfTerm name)
			hPutStrLn stderr ("note: found " ++ show (M.size tlDefns) ++ " TL meta-object definition(s)")
			forM_ (M.toList tlDefns) $ \ (name, value) -> do
				let type1 = TLR.typeOfMetaObject value
				let type2 = TLF.formatMetaTypeAsString type1
				hPutStrLn stderr ("note:     " ++ TLS.unName name ++ " :: " ++ type2)
			when (null emits) $ do
				hPutStrLn stderr "warning: there is no JS code to emit"
			putStrLn (JS.renderStatements emits)
			exitSuccess

