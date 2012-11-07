module Test.JavascriptEquivalent where

import Control.Monad (foldM)
import qualified Data.Map as M
import Language.ECMAScript3.PrettyPrint
import Metacompiler.GenSym
import Metacompiler.ParseSExpr
import Metacompiler.SExpr
import Metacompiler.SExprToTL
import Metacompiler.TLRuntime

test :: String -> String -> IO ()
test directives target = case parseSExprs directives of
	Left err -> putStrLn ("MALFORMED S-EXPR: " ++ show err)
	Right directives' -> case sequence (map parseTLDirectiveFromSExpr (sExprsToList directives')) of
		Left err -> putStrLn ("MALFORMED DIRECTIVE: " ++ show err)
		Right directives'' -> case parseSExprs target of
			Left err -> putStrLn ("MALFORMED S-EXPR: " ++ show err)
			Right target' -> case parseTLMetaObjectFromSExprs target' of
				Left err -> putStrLn ("MALFORMED META-OBJECT: " ++ show err)
				Right target'' -> do
					let target''' = runGenSym $ do
						vars <- foldM processDirective M.empty directives''
						reduce vars M.empty target''
					putStrLn ("ok, " ++ renderExpression (jsEquivalentOfJSTerm target'''))

main = do
	test "" "(js-expr (spec a) (type a) (impl [[test]]))"
	test "" "(\\ (x :: js-term a) -> x) (js-expr (spec a) (type a) (impl [[test]]))"
	test "" "(js-expr (spec a) (type a) (impl [[test]] (free [[test]])))"
	test "\
		\(let NatAsNumberZero = (js-expr \
		\    (type NatAsNumber) \
		\    (spec Zero) \
		\    (impl \"0\") \
		\)) \
		\ \
		\(let NatAsNumberSucc (x :: js-term NatAsNumber) = (js-expr \
		\    (type NatAsNumber) \
		\    (spec (Succ x)) \
		\    (impl \"x + 1\" (set \"x\" x)) \
		\)) \
		\ \
		\(let NatAsNumberPlus (x :: js-term NatAsNumber) (y :: js-term NatAsNumber) = (js-expr \
		\    (type NatAsNumber) \
		\    (spec (plus x y)) \
		\    (impl \"x + y\" (set \"x\" x) (set \"y\" y)) \
		\)) \
		\ \
		\(let NatAsNumberTimes (x :: js-term NatAsNumber) (y :: js-term NatAsNumber) = (js-expr \
		\    (type NatAsNumber) \
		\    (spec (times x y)) \
		\    (impl \"x * y\" (set \"x\" x) (set \"y\" y)) \
		\)) \
		\ "
		"(NatAsNumberPlus (NatAsNumberSucc (NatAsNumberZero)) (NatAsNumberSucc (NatAsNumberZero)))"
