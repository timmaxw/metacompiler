module Test.SLParser where

import Metacompiler.ParseSExpr
import Metacompiler.SExprToSL

good f x = case parseSExprs x of
	Left e -> putStrLn ("MALFORMED S-EXPR: " ++ show e)
	Right se -> case f se of
		Left e -> putStrLn ("FALSE POSITIVE: " ++ show e)
		Right x -> putStrLn ("ok, " ++ show x)

bad f x = case parseSExprs x of
	Left e -> putStrLn ("MALFORMED S-EXPR: " ++ show e)
	Right se -> case f se of
		Left e -> putStrLn ("ok, " ++ show e)
		Right x -> putStrLn ("FALSE NEGATIVE: " ++ show x) 

goodKind = good parseSLKindFromSExprs
badKind = bad parseSLKindFromSExprs

goodType = good parseSLTypeFromSExprs
badType = bad parseSLTypeFromSExprs

goodTerm = good parseSLTermFromSExprs
badTerm = bad parseSLTermFromSExprs

main = do
	goodKind "*"
	goodKind "(*)"
	goodKind "fun * -> *"
	badKind "error"

	goodType "a"
	goodType "a b c"
	goodType "(a (b) c)"
	goodType "lazy x"
	goodType "fun a -> b"
	badType "fun a ->"
	badType "lazy"

	goodTerm "a"
	goodTerm "a b"
	goodTerm "((((a))))"
	goodTerm "(\\ (a :: x) -> b)"
	badTerm "(\\ (a :: x) ->)"
	badTerm "(\\ (a :: x))"
	badTerm "(\\)"
	goodTerm "case x of (y f1 f2 f3) -> (z) (p) -> (q)"


