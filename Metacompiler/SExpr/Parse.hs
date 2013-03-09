module Metacompiler.SExpr.Parse (parseSExprs) where

-- This module contains facilities for parsing a string into an `SExpr`.

import Control.Monad (unless)
import Data.List
import Metacompiler.Error
import Metacompiler.SExpr.Types

-- `summarize` returns the input in quotes, truncated if it is long.

summarize :: String -> String
summarize s
	| length s < 10 = show s
	| otherwise = show (take 10 s ++ "...")

-- `parseSExprs` takes a string and interprets it as a sequence of
-- S-expressions.

parseSExprs :: String -> BacktraceMonad SExprs
parseSExprs string1 = do
	let (string2, point2) = strip (string1, Point 1 1)
	(topLevels, (string3, point3)) <- parseMany (string2, point2)
	let (string4, point4) = strip (string3, point3)
	unless (null string4) $
		fail ("unexpected " ++ summarize string4 ++ " at " ++ formatPoint point4)
	return topLevels

	where
		parseMany :: (String, Point) -> BacktraceMonad (SExprs, (String, Point))
		parseMany (string1, _) | not (isStripped string1) = error "input wasn't stripped properly"
		parseMany ("", point) = return (Nil point, ("", point))
		parseMany (')':string2, point1) = return (Nil point1, (')':string2, point1))
		parseMany (string1, point1) = do
			(elem, (string2, point2)) <- parseOne (string1, point1)
			let (string3, point3) = strip (string2, point2)
			(elems, (string4, point4)) <- parseMany (string3, point3)
			return (Cons elem elems, (string4, point4))

		parseOne :: (String, Point) -> BacktraceMonad (SExpr, (String, Point))
		parseOne (string1, _) | not (isStripped string1) = error "input wasn't stripped properly"
		parseOne ('(':string2, point1) = do
			let point2 = stepPoint point1
			let (string3, point3) = strip (string2, point2)
			(elems, (string4, point4)) <-
				frameBacktrace ("in list " ++ summarize ('(':string1) ++ " starting at " ++ formatPoint point1) $
				parseMany (string3, point3)
			let (string5, point5) = strip (string4, point4)
			case string5 of
				[] -> fail ("parentheses at " ++ formatPoint point1 ++ " are never closed")
				')':string6 -> do
					let point6 = stepPoint point5
					return (List (Range point1 point6) elems, (string6, point6))
				_ -> error ("why didn't parseMany try to parse this? " ++ summarize string5 ++ " " ++ formatPoint point5)
		parseOne ('"':string2, point1) = do
			let point2 = stepPoint point1
			let parseString (s1, p1) = case s1 of
				[] -> fail "there is no closing quote"
				'"':s2 -> do
					let p2 = stepPoint p1
					return ("", (s2, p2))
				'\\':[] -> fail "there is no closing quote"
				'\\':a:s2 | a `elem` "nrt\"\'\\" -> do
					let p2 = stepPoint (stepPoint p1)
					let a' = case a of {
						'n' -> '\n'; 'r' -> '\r'; 't' -> '\t';
						'\"' -> '\"'; '\'' -> '\''; '\\' -> '\\' }
					(rest, (s3, p3)) <- parseString (s2, p2)
					return (a:rest, (s3, p3))
				'\\':s2 -> fail ("invalid escape sequence " ++ summarize s2 ++ " at " ++ formatPoint p1)
				a:s2 -> do
					let p2 = stepPoint p1
					(rest, (s3, p3)) <- parseString (s2, p2)
					return (a:rest, (s3, p3))
			(body, (string3, point3)) <-
				frameBacktrace ("in string literal " ++ summarize ('"':string2) ++ " starting at " ++ formatPoint point1) $
				parseString (string2, point2)
			return (Quoted (Range point1 point3) body, (string3, point3))
		parseOne ('[':string2, point1) = do
			let point2 = stepPoint point1
			let countEquals (s1, p1) = case s1 of
				'=':s2 -> do
					let p2 = stepPoint p1
					(n, (s3, p3)) <- countEquals (s2, p2)
					return (n+1, (s3, p3))
				_ -> return (0, (s1, p1))
			(equals, (string3, point3)) <- countEquals (string2, point2)
			(string4, point4) <- case string3 of
				'[':s4 -> return (s4, stepPoint point3)
				_ -> fail ("malformed string literal starting at " ++ formatPoint point1)
			let endDelimiter = "]" ++ replicate equals '=' ++ "]"
			let takeBody (s1, p1) = case s1 of
				_ | endDelimiter `Data.List.isPrefixOf` s1 ->
					return ("", (drop (equals + 2) s1, stepPoint' (equals + 2) p1))
				[] ->
					fail ("there is no closing \"" ++ endDelimiter ++ "\"")
				a:s2 -> do
					let p2 = if a /= '\n' then stepPoint p1 else stepNewlinePoint p1
					(rest, (s3, p3)) <- takeBody (s2, p2)
					return (a:rest, (s3, p3))
			(body, (string5, point5)) <-
				frameBacktrace ("in string literal " ++ summarize ('[':string2) ++ " starting at " ++ formatPoint point1) $
				takeBody (string4, point4)
			return (Quoted (Range point1 point5) body, (string5, point5))
		parseOne (a:string2, point1) | validAtomChar a = do
			let (atom, string3) = break (not . validAtomChar) (a:string2)
			let point3 = stepPoint' (length atom) point1
			return (Atom (Range point1 point3) atom, (string3, point3))
			where
				validAtomChar a = a `elem` "!#$%&\'*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWX\
					\YZ\\^_`abcdefghijklmnopqrstuvwxyz{|}~"
		parseOne ('\r':_, point1) = fail ("illegal '\\r' character at " ++ formatPoint point1)
		parseOne ([], point1) = fail ("expected s-expression, found EOF")
		parseOne (string1, point1) = fail ("don't know how to handle " ++ summarize string1 ++ " at " ++ formatPoint point1)

		isStripped :: String -> Bool
		isStripped (a:_) | a `elem` " \n\t;" = False
		isStripped _ = True

		strip :: (String, Point) -> (String, Point)
		strip ([], p1) = ([], p1)
		strip (' ':s2, p1) = strip (s2, stepPoint p1)
		strip ('\t':s2, p1) = strip (s2, stepPoint p1)
		strip ('\n':s2, p1) = strip (s2, stepNewlinePoint p1)
		strip (';':s2, p1) = let
			(comment, s3) = break (== '\n') s2
			p3 = stepPoint' (1 + length comment) p1
			in strip (s3, p3)
		strip (s1, p1) = (s1, p1)

