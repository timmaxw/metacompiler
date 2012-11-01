module Test.TLParser where

import Control.Monad ((>=>))
import Metacompiler.ParseSExpr
import qualified Metacompiler.SExpr as SExpr
import Metacompiler.SExprToTL

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

goodMetaType = good parseTLMetaTypeFromSExprs
badMetaType = bad parseTLMetaTypeFromSExprs

goodMetaObject = good parseTLMetaObjectFromSExprs
badMetaObject = bad parseTLMetaObjectFromSExprs

expectOne :: SExpr.SExprs -> Either String SExpr.SExpr
expectOne (SExpr.Cons x (SExpr.Nil _)) = return x
expectOne other = Left ("expected only one")

goodDirective = good (expectOne >=> parseTLDirectiveFromSExpr)
badDirective = bad (expectOne >=> parseTLDirectiveFromSExpr)

main = do
	goodMetaType "type"
	goodMetaType "((type))"
	goodMetaType "term A"
	goodMetaType "term (A B)"
	badMetaType "term A B"
	badMetaType "[[]]"
	badMetaType "()"
	goodMetaType "fun (a :: type) -> type"

	goodMetaObject "foo"
	goodMetaObject "\\ (a :: type) -> a"
	goodMetaObject "a b c d"
	goodMetaObject "(((((((((blah)))))))))"
	goodMetaObject "(js-expr (spec a) (type b) (impl [[c]] (set \"c\" d) (free \"e\"))"
	goodMetaObject "\\ (a :: type) -> \\ (b :: type) -> c"

	goodDirective "(let foo = bar)"
	goodDirective "(let foo :: type = bar)"
	goodDirective "(let foo (a :: term a) :: (term b) = bar)"
	goodDirective "(js-repr FooAsFoo (x :: type) = (spec foo))"


