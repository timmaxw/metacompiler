module Test.JavascriptEquivalent where

import Control.Monad (foldM)
import qualified Data.Map as M
import Language.ECMAScript3.PrettyPrint
import Metacompiler.JSEval
import Metacompiler.JSUtils
import Metacompiler.ParseSExpr
import Metacompiler.SExpr
import Metacompiler.SExprToTL
import Metacompiler.TLRuntime

-- `test` takes two inputs. The first will be interpreted as a TL input file;
-- that is, a series of TL directives. The second will be interpreted as a TL
-- meta-object, that must evaluate to a JS term. It will return the JS
-- equivalent of the latter, with access to any variables defined by the
-- directives.

test :: String -> String -> IO ()
test directives target = do
	let maybeTargetTerm = do
		directives' <- parseSExprs directives
		directives'' <- sequence (map parseTLDirectiveFromSExpr (sExprsToList directives'))
		vars <- foldM processDirective M.empty directives''
		target' <- parseSExprs target
		target'' <- parseTLMetaObjectFromSExprs target'
		ty <- computeMetaType vars target''
		case ty of
			NMTJSTerm _ -> return ()
			other -> Left ("target should have type (js-term ...), but instead had type " ++ show ty)
		return (reduce (M.map snd vars) target'')
	case maybeTargetTerm of
		Left err -> putStrLn ("MALFORMED: " ++ show err)
		Right targetTerm -> do
			let targetAST = runRenameSymbols (jsEquivalentOfJSTerm targetTerm)
			let targetString = renderExpression targetAST
			result <- evalJS targetAST
			case result of
				Left err -> do
					putStrLn "------------------"
					putStrLn "RUN ERROR:"
					putStrLn targetString
					putStr err
				Right answer -> do
					putStrLn "------------------"
					putStrLn "ok:"
					putStrLn targetString
					putStr answer

main = do
	-- Basic test
	test "" "(js-expr (spec a) (type a) (impl [[1]]))"

	-- Test of `MOAbs` and `MOApp`
	test "" "(\\ (x :: js-term a) -> x) (js-expr (spec a) (type a) (impl [[1]]))"

	-- Test of the variable substitution mechanism
	test "" "(js-expr (spec a) (type a) (impl [[(function (x) { return x; })(1)]]))"

	-- First example from `tutorial.md`
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

