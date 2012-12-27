module Metacompiler.SExpr where

-- This module describes S-expression types. Our S-expressions consist only of
-- lists, atoms, and string literals. This module is primarily meant to be used
-- for building S-expression-based syntaxes, so it has a lot of convenience
-- functions for people writing parsers that accept S-expressions.

import Control.Monad.Error
import Data.List

-- `Point` is a location in the source file.

data Point = Point {
	lineOfPoint :: Int,
	columnOfPoint :: Int
	}
	deriving (Eq, Ord)

instance Show Point where
	showsPrec p (Point a b) s
		| p == 10 || p == 11 = "(" ++ showsPrec 0 (Point a b) (")" ++ s)
		| otherwise = "Point " ++ showsPrec 10 a (" " ++ showsPrec 10 b s)

formatPoint :: Point -> String
formatPoint (Point l c) = show l ++ ":" ++ show c

-- `Range` is a range of characters in the source file.

data Range = Range {
	startOfRange :: Point,
	endOfRange :: Point
	}
	deriving (Eq, Ord)

instance Show Range where
	showsPrec p (Range a b) s
		| p == 10 || p == 11 = "(" ++ showsPrec 0 (Range a b) (")" ++ s)
		| otherwise = "Range " ++ showsPrec 10 a (" " ++ showsPrec 10 b s)

formatRange :: Range -> String
formatRange (Range a b)
	| a == b = formatPoint a
	| otherwise = formatPoint a ++ "-" ++ formatPoint b

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

sExprsToList :: SExprs -> [SExpr]
sExprsToList (Cons x xs) = x : sExprsToList xs
sExprsToList (Nil _) = []

sExprsInitAndLast :: SExprs -> Maybe (SExprs, SExpr)
sExprsInitAndLast (Nil _) = Nothing
sExprsInitAndLast (Cons last (Nil _)) = Just (Nil (startOfRange (rangeOfSExpr last)), last)
sExprsInitAndLast (Cons x xs) = Just (Cons x init, last)
	where Just (init, last) = sExprsInitAndLast xs

-- `summarizeSExpr` and `summarizeSExprs` are for formatting error messages.
-- They return a textual representation of the `SExpr` or `SExprs`, with parts
-- replaced with `...` as necessary to keep it from getting too long.

summarizeSExpr :: SExpr -> String
summarizeSExpr e = "\"" ++ summarizeSExpr' 2 e ++ "\""

summarizeSExprs :: SExprs -> String
summarizeSExprs e = "\"" ++ summarizeSExprs' 2 e ++ "\""

summarizeSExpr' :: Int -> SExpr -> String
summarizeSExpr' _ (Atom _ a) = a
summarizeSExpr' 0 _ = "..."
summarizeSExpr' level (List _ xs) = "(" ++ summarizeSExprs' level xs ++ ")"
summarizeSExpr' level (Quoted _ s)
	-- This is incorrect because Haskell escapes are different from the escapes
	-- that `Metacompiler.ParseSExpr` accepts.
	| length s <= level * 5 = show s
	| otherwise = show (take (level*5-3) s ++ "...")

summarizeSExprs' :: Int -> SExprs -> String
summarizeSExprs' 0 _ = "..."
summarizeSExprs' level xs
	| length list <= level * 3 = intercalate " " (map (summarizeSExpr' (level-1)) list)
	| otherwise = intercalate " " (map (summarizeSExpr' (level-1)) (take (level*3-1) list)) ++ " ..."
	where list = sExprsToList xs

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

-- `breakOnAtom` is a convenience function for parsers. Given an atom name and
-- an `SExprs`, it breaks the `SExprs` at the first occurrance of that atom. If
-- the atom is not found, it produces a nice error message. `multiBreakOnAtom`
-- is similar, except that it expects to find zero or more occurrances of the
-- atom, so it returns a list instead of a pair.

breakOnAtom :: String -> SExprs -> Either String (SExprs, SExprs)
breakOnAtom atom block = break' block
	where
		break' (Nil p) =
			Left ("expected \"" ++ atom ++ "\" at " ++ formatRange (rangeOfSExprs block))
		break' (Cons (Atom r a) rest) | a == atom =
			return (Nil (startOfRange r), rest)
		break' (Cons x rest) = do
			(before, after) <- break' rest
			return (Cons x before, after)

multiBreakOnAtom :: String -> SExprs -> Either String [SExprs]
multiBreakOnAtom atom block = break' block
	where
		break' (Nil p) = [Nil p]
		break' (Cons (Atom r a) rest) | a == atom = do
			groups <- break' rest
			return (Nil (startOfRange r):groups)
		break' (Cons x rest) = do
			(group:groups) <- break' rest
			return (Cons x group:groups)

-- `takeOne` and `expectOne` are functions for parsers. `takeOne` expects the
-- given sequence to contain at least one object; it splits off that object,
-- and returns it and the rest of the sequence. `expectOne` expects the given
-- sequence to contain exactly one object, which it returns.

takeOne :: String -> SExprs -> Either String (SExpr, SExprs)
takeOne what (Nil p) = Left ("expected " ++ what ++ " at " ++ formatPoint p)
takeOne _ (Cons a b) = return (a, b)

expectOne :: String -> SExprs -> Either String SExpr
expectOne what (Nil p) = Left ("expected " ++ what ++ " at " ++ formatPoint p)
expectOne _ (Cons a (Nil _)) = return a
expectOne what (Cons a b) = Left ("expected nothing after the " ++ what ++ " at " ++ formatRange (rangeOfSExprs a) ++
	", but found more at " ++ formatRange (rangeOfSExprs b) ++ ".")

