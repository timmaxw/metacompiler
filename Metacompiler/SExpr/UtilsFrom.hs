module Metacompiler.SExpr.UtilsFrom where

-- This module contains utilities that will be useful when trying to convert from S-expressions to some other format;
-- hence the somewhat strange name `UtilsFrom`.

import Metacompiler.Error
import Metacompiler.SExpr.Format
import Metacompiler.SExpr.Types

-- `formatSExprForMessage` and `formatSExprsForMessage` are for formatting error messages. They return a textual
-- representation of the `SExpr` or `SExprs`, with parts replaced with `...` as necessary to keep it from getting too
-- long.

formatSExprForMessage :: SExpr -> String
formatSExprForMessage e = "`" ++ formatSExprLimit 2 e ++ "`"

formatSExprsForMessage :: SExprs -> String
formatSExprsForMessage e = "`" ++ formatSExprsLimit 2 e ++ "`"

-- `breakOnAtom` is a convenience function for parsers. Given an atom name and
-- an `SExprs`, it breaks the `SExprs` at the first occurrance of that atom. If
-- the atom is not found, it produces a nice error message. `multiBreakOnAtom`
-- is similar, except that it expects to find zero or more occurrances of the
-- atom, so it returns a list instead of a pair. `maybeBreakOnAtom` expects to
-- find zero or one occurrance.

breakOnAtom :: String -> SExprs -> ErrorMonad (SExprs, SExprs)
breakOnAtom atom block = break' block
	where
		break' (Nil p) =
			fail ("expected \"" ++ atom ++ "\" at " ++ formatRange (rangeOfSExprs block))
		break' (Cons (Atom r a) rest) | a == atom =
			return (Nil (startOfRange r), rest)
		break' (Cons x rest) = do
			(before, after) <- break' rest
			return (Cons x before, after)

multiBreakOnAtom :: String -> SExprs -> ErrorMonad [SExprs]
multiBreakOnAtom atom block = break' block
	where
		break' :: SExprs -> ErrorMonad [SExprs]
		break' (Nil p) = return [Nil p]
		break' (Cons (Atom r a) rest) | a == atom = do
			groups <- break' rest
			return (Nil (startOfRange r):groups)
		break' (Cons x rest) = do
			(group:groups) <- break' rest
			return (Cons x group:groups)

maybeBreakOnAtom :: String -> SExprs -> ErrorMonad (SExprs, Maybe SExprs)
maybeBreakOnAtom atom block = break' block
	where
		break' (Nil p) =
			return (Nil p, Nothing)
		break' (Cons (Atom r a) rest) | a == atom =
			return (Nil (startOfRange r), Just rest)
		break' (Cons x rest) = do
			(before, after) <- break' rest
			return (Cons x before, after)

-- `takeOne` and `expectOne` are functions for parsers. `takeOne` expects the
-- given sequence to contain at least one object; it splits off that object,
-- and returns it and the rest of the sequence. `expectOne` expects the given
-- sequence to contain exactly one object, which it returns.

takeOne :: String -> SExprs -> ErrorMonad (SExpr, SExprs)
takeOne what (Nil p) = fail ("expected " ++ what ++ " at " ++ formatPoint p)
takeOne _ (Cons a b) = return (a, b)

expectOne :: String -> SExprs -> ErrorMonad SExpr
expectOne what (Nil p) = fail ("expected " ++ what ++ " at " ++ formatPoint p)
expectOne _ (Cons a (Nil _)) = return a
expectOne what (Cons a b) = fail ("expected nothing after the " ++ what ++ " at " ++ formatRange (rangeOfSExpr a) ++
	", but found more at " ++ formatRange (rangeOfSExprs b) ++ ".")

