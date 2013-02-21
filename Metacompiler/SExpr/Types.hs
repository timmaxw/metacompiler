module Metacompiler.SExpr.Types where

-- This module describes S-expression types. Our S-expressions consist only of
-- lists, atoms, and string literals. This module is primarily meant to be used
-- for building S-expression-based syntaxes, so it has a lot of convenience
-- functions for people writing parsers that accept S-expressions.

import Control.Monad.Error
import Data.List
import Metacompiler.Range

-- `SExpr` is a S-expression, tagged with line/column information. `SExprs` is
-- a sequence of S-expressions, tagged with line/column information. Some
-- S-expression libraries allow parameterization on what the S-expressions are
-- tagged with. We don't because `SExprs` is special; an empty `SExprs` still
-- indicates what point in the source file the S-expressions would have gone.
-- We need this information to make nice error messages. This concept doesn't
-- make much sense for arbitrary tag-types.

data SExpr
	= List Range SExprs
	| Atom Range String
	| Quoted Range String
	deriving (Show, Eq, Ord)

data SExprs
	= Cons SExpr SExprs
	| Nil Point   -- This `Point` is where the empty list would have been
	deriving (Show, Eq, Ord)

infixr 5 `Cons`

sExprsToList :: SExprs -> [SExpr]
sExprsToList (Cons x xs) = x : sExprsToList xs
sExprsToList (Nil _) = []

sExprsInitAndLast :: SExprs -> Maybe (SExprs, SExpr)
sExprsInitAndLast (Nil _) = Nothing
sExprsInitAndLast (Cons last (Nil _)) = Just (Nil (startOfRange (rangeOfSExpr last)), last)
sExprsInitAndLast (Cons x xs) = Just (Cons x init, last)
	where Just (init, last) = sExprsInitAndLast xs

-- `rangeOfSExpr` and `rangeOfSExprs` give the line/column ranges that the
-- `SExpr` or `SExprs` originally came from.

rangeOfSExpr :: SExpr -> Range
rangeOfSExpr (Atom r _) = r
rangeOfSExpr (List r _) = r
rangeOfSExpr (Quoted r _) = r

rangeOfSExprs :: SExprs -> Range
rangeOfSExprs (Nil p) = Range p p
rangeOfSExprs s@(Cons x _) = Range (startOfRange (rangeOfSExpr x)) (endOfRange (rangeOfSExpr (endOf s)))
	where
		endOf (Cons x (Nil _)) = x
		endOf (Cons _ y) = endOf y

