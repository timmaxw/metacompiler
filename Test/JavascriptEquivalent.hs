module Test.JavascriptEquivalent where

import qualified Data.Map as M
import Language.ECMAScript3.PrettyPrint
import Metacompiler.GenSym
import Metacompiler.ParseSExpr
import Metacompiler.SExprToTL
import Metacompiler.TLRuntime

test :: String -> IO ()
test s = case parseSExprs s of
	Left err -> putStrLn ("MALFORMED S-EXPR: " ++ show err)
	Right sexprs -> case parseTLMetaObjectFromSExprs sexprs of
		Left err -> putStrLn ("MALFORMED TL METAOBJECT: " ++ show err)
		Right metaObject -> do
			let obj = runGenSym (reduce M.empty M.empty metaObject)
			putStrLn ("ok, " ++ renderExpression (jsEquivalentOfJSTerm obj))

main = do
	test "(js-expr (spec a) (type a) (impl [[test]]))"
	test "(\\ (x :: js-term a) -> x) (js-expr (spec a) (type a) (impl [[test]]))"
	test "(js-expr (spec a) (type a) (impl [[test]] (free [[test]])))"

