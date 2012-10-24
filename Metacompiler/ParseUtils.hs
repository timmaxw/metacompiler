module Metacompiler.ParseUtils where

import Control.Monad.Error
import Data.List
import Metacompiler.SExpr

errorContext :: String -> Either String a -> Either String a
errorContext s (Left m) = Left (s ++ "\n" ++ m)
errorContext s (Right x) = Right x

-- Text tools

data Point = Point Int Int deriving (Show, Eq, Ord)
data Range = Range {
	rangeStart :: Point,
	rangeEnd :: Point
	} deriving (Show, Eq, Ord)

stepPoint :: Point -> Point
stepPoint = stepPoint' 1

stepPoint' :: Int -> Point -> Point
stepPoint' n (Point l c) = (Point l (c+n))

stepNewlinePoint :: Point -> Point
stepNewlinePoint (Point l c) = (Point (l+1) 0)

formatPoint :: Point -> String
formatPoint (Point l c) = show l ++ ":" ++ show c

formatRange :: Range -> String
formatRange (Range a b)
	| a == b = formatPoint a
	| otherwise = formatPoint a ++ "-" ++ formatPoint b

summarize :: String -> String
summarize s
	| length s < 10 = show s
	| otherwise = show (take 10 s ++ "...")

-- Parsing S-expressions into more complicated structures

summarizeSExpr :: SExpr a -> String
summarizeSExpr e = "\"" ++ summarizeSExpr' 2 e ++ "\""

summarizeSExprs :: [SExpr a] -> String
summarizeSExprs e = "\"" ++ summarizeSExprs' 2 e ++ "\""

summarizeSExprs' :: Int -> [SExpr a] -> String
summarizeSExprs' 0 _ = "..."
summarizeSExprs' level xs
	| length xs <= level * 3 = intercalate " " (map (summarizeSExpr' (level-1)) xs)
	| otherwise = intercalate " " (map (summarizeSExpr' (level-1)) (take (level*3-1) xs)) ++ " ..."

summarizeSExpr' :: Int -> SExpr a -> String
summarizeSExpr' _ (Atom _ a) = a
summarizeSExpr' 0 _ = "..."
summarizeSExpr' level (List _ xs) = "(" ++ summarizeSExprs' level xs ++ ")"
summarizeSExpr' level (Quoted _ s)
	| length s <= level * 5 = show s
	| otherwise = show (take (level*5-3) s ++ "...")

sExprRange :: SExpr Range -> Range
sExprRange (Atom r _) = r
sExprRange (List r _) = r
sExprRange (Quoted r _) = r

sExprsRange :: [SExpr Range] -> Range
sExprsRange [] = Range (Point 0 0) (Point 0 0)   -- Hack
sExprsRange xs = Range (rangeStart (sExprRange (head xs))) (rangeEnd (sExprRange (last xs)))

breakOnAtom :: String -> [SExpr a] -> Either String ([SExpr a], [SExpr a])
breakOnAtom atom [] =
	Left ("expected \"" ++ atom ++ "\", didn't find it")
breakOnAtom atom ((Atom _ a):xs) | a == atom =
	return ([], xs)
breakOnAtom atom (x:xs) = do
	(before, after) <- breakOnAtom atom xs
	return (x:before, after)

