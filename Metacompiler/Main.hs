module Metacompiler.Main where

import Control.Monad
import qualified Data.Map as M
import qualified Language.ECMAScript3.PrettyPrint as JS
import qualified Metacompiler.Compile.CompileSL as CSL
import qualified Metacompiler.Compile.CompileTL as CTL
import qualified Metacompiler.Compile.FormatTL as FTL
import qualified Metacompiler.Runtime as R
import Metacompiler.SExpr.Parse
import Metacompiler.SExpr.Types
import qualified Metacompiler.SL.Syntax as SL
import qualified Metacompiler.TL.FromSExpr as TL
import qualified Metacompiler.TL.Syntax as TL
import qualified Metacompiler.TL.ToSExpr as TL
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
			mapM TL.parseTLDirectiveFromSExpr (sExprsToList sexprs)
		case maybeDirectives of
			Left err -> do
				hPutStrLn stderr "error:"
				hPutStrLn stderr err
				exitFailure
			Right d -> return d
		| filename <- filenames]
	case CTL.compileDirectives allDirectives of
		Left err -> do
			hPutStrLn stderr "error:"
			hPutStrLn stderr err
			exitFailure
		Right (CTL.GlobalResults (CSL.Defns slDataDefns slCtorDefns slTermDefns) tlDefns emits) -> do
			hPutStrLn stderr ("note: found " ++ show (M.size slDataDefns) ++ " SL data definition(s)")
			forM_ (M.toList slDataDefns) $ \ (name, _) -> do
				hPutStrLn stderr ("note:     " ++ SL.unNameOfType name)
			hPutStrLn stderr ("note: found " ++ show (M.size slCtorDefns) ++ " SL ctor definition(s)")
			forM_ (M.toList slCtorDefns) $ \ (name, _) -> do
				hPutStrLn stderr ("note:     " ++ SL.unNameOfTerm name)
			hPutStrLn stderr ("note: found " ++ show (M.size slTermDefns) ++ " SL term definition(s)")
			forM_ (M.toList slTermDefns) $ \ (name, _) -> do
				hPutStrLn stderr ("note:     " ++ SL.unNameOfTerm name)
			hPutStrLn stderr ("note: found " ++ show (M.size tlDefns) ++ " TL meta-object definition(s)")
			forM_ (M.toList tlDefns) $ \ (name, value) -> do
				let type1 = R.typeOfMetaObject value
				let type2 = FTL.formatMetaTypeAsTL type1
				let type3 = TL.formatTLMetaTypeAsString type2
				hPutStrLn stderr ("note:     " ++ TL.unName name ++ " :: " ++ type3)
			when (null emits) $ do
				hPutStrLn stderr "warning: there is no JS code to emit"
			putStrLn (JS.renderStatements emits)
			exitSuccess

