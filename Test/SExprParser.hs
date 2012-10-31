module Test.SExprParser where

import Metacompiler.ParseSExpr

good x = case parseSExprs x of
	Left e -> putStrLn ("FALSE POSITIVE: " ++ show e)
	Right x -> putStrLn ("ok, " ++ show x)

bad x = case parseSExprs x of
	Left e -> putStrLn ("ok, " ++ show e)
	Right x -> putStrLn ("FALSE NEGATIVE: " ++ show x) 

main = do
	good "foobar"
	good "(())"
	good "(a b c \"xy)z\")"
	good "a a a\na\na a"
	good "[[hi mike]]"
	bad "intentional error ("
	bad "]]"
	bad "\""


	
