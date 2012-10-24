module Metacompiler.SExpr where

data SExpr a
	= List a [SExpr a]
	| Atom a String
	| Quoted a String
	deriving (Show, Eq, Ord)

