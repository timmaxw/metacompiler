module TestMetacompiler.TLRuntime where

import Control.Monad (unless)
import qualified Data.Map as M
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
import TestMetacompiler.Error
import TestMetacompiler.SLRuntime
import Test.HUnit

defaultScope :: TLC.Scope
defaultScope = TLC.Scope {
	TLC.metaObjectsInScope = M.fromList [
		(var "type1" "sl-type [[*]]"),
		(var "type2" "sl-type [[*]]"),
		(var "term1" "sl-term (sl-type [[Nat]])"),
		(var "someTypeFun" "fun (x :: sl-type [[*]]) -> sl-type [[*]]"),
		(var "someTermFun" "fun (x :: sl-term (sl-type [[Nat]])) -> sl-term (sl-type [[Nat]])")
		],
	TLC.slObjectsInScope = SLC.scopeForDefns defaultSLDefns
	}
	where var n t = (TLS.Name n, TLR.MOName (TLR.NameOfMetaObject n) (compileMetaType t))

compileMetaType :: String -> TLR.MetaType
compileMetaType str = assumeSuccess ("in compileMetaType " ++ show str) $ do
	sexprs <- parseSExprs str
	syntax <- TLS.parseTLMetaTypeFromSExprs sexprs
	TLC.compileMetaType defaultScope syntax

compileMetaObject :: String -> TLR.MetaObject
compileMetaObject str = assumeSuccess ("in compileMetaObject " ++ show str) $ do
	sexprs <- parseSExprs str
	syntax <- TLS.parseTLMetaObjectFromSExprs sexprs
	TLC.compileMetaObject defaultScope syntax

reductionTest :: Bool -> (String, String) -> Test
reductionTest shouldBeEqual (string1, string2) = TestCase $ do
	let obj1 = compileMetaObject string1
	obj1 `seq` return ()
	let obj2 = compileMetaObject string2
	obj2 `seq` return ()
	unless ((obj1 `TLR.equivalentMetaObjects` obj2) == shouldBeEqual) $ do
		let msg = case shouldBeEqual of
			True -> "Expected equality, got inequality:"
			False -> "Expected inequality, got equality:"
		let repr1 = TLF.formatMetaObjectAsString obj1
		let repr1' = TLF.formatMetaObjectAsString (TLR.reduceMetaObject obj1)
		let repr2 = TLF.formatMetaObjectAsString obj2
		let repr2' = TLF.formatMetaObjectAsString (TLR.reduceMetaObject obj2)
		assertFailure $
			msg ++
				"\nObject 1: " ++ repr1 ++
				"\nReduced: " ++ repr1' ++
				"\nObject 2: " ++ repr2 ++
				"\nReduced: " ++ repr2'

allTests = TestList [
	reductionTest True (
		"type1",
		"type1"),
	reductionTest False (
		"type1",
		"type2"),
	reductionTest True (
		"(\\ (a :: sl-type [[*]]) -> a) type1",
		"type1"),
	reductionTest True (
		"(\\ (a :: sl-type [[*]]) -> (\\ (x :: sl-type [[fun * -> *]]) -> a)) type1",
		"(\\ (y :: sl-type [[fun * -> *]]) -> type1)"),
	reductionTest True (
		"sl-type [[Nat]]",
		"sl-type [[Nat]]"),
	reductionTest False (
		"sl-type [[Nat]]",
		"type1"),
	reductionTest True (
		"sl-term [[\\ (x :: Nat) -> x]]",
		"sl-term [[\\ (y :: Nat) -> y]]"),
	reductionTest True (
		"sl-term [[Succ . x]] (term [[x]] = sl-term [[Zero]])",
		"sl-term [[Succ . Zero]]"),
	reductionTest True (
		"sl-term [[Succ . (f . Zero)]] (term [[f]] (x :: sl-term (sl-type [[Nat]])) = someTermFun x)",
		"sl-term [[Succ . y]] (term [[y]] = someTermFun (sl-term [[Zero]]))"),
	reductionTest True (
		"sl-term [[Succ . x]] (term [[x]] = sl-term [[Succ . y]] (term [[y]] = sl-term [[Zero]]))",
		"sl-term [[Succ . (Succ . Zero)]]"),
	reductionTest True (
		"sl-term [[Zero]] (term [[x]] = term1)",
		"sl-term [[Zero]]"),
	reductionTest True (
		"sl-term [[Succ . (f . Zero Zero)]] \
			\(term [[f]] (x :: sl-term (sl-type [[Nat]])) (y :: sl-term (sl-type [[Nat]])) = someTermFun y)",
		"sl-term [[Succ . (f . Zero)]] \
			\(term [[f]] (z :: sl-term (sl-type [[Nat]])) = someTermFun z)"),
	reductionTest True (
		"sl-term [[f . Zero]] (term [[f]] (x :: sl-term (sl-type [[Nat]])) = x)",
		"sl-term [[Zero]]"),
	reductionTest True (
		"sl-term [[Succ . x]] \
			\(term [[x]] = \
				\sl-term [[Succ . (y . Zero)]] (term [[y]] (z :: sl-term (sl-type [[Nat]])) = someTermFun z) \
				\)",
		"sl-term [[Succ . (Succ . (y . Zero))]] (term [[y]] (z :: sl-term (sl-type [[Nat]])) = someTermFun z)"),
	reductionTest True (
		"sl-type [[x]] (type [[x]] = type1)",
		"type1")
	]




