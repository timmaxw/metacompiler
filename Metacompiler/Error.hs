module Metacompiler.Error (
	module Control.Exception.Base,   -- re-export `assert`
	assertM,
	errorContext,
	Point(..), formatPoint, Range(..), formatRange,
	stepPoint, stepPoint', stepNewlinePoint
	) where

import Control.Exception.Base (assert)
import Control.Monad.Trans.Error   -- allows `fail` in `Either String`

-- `assertM` is like `assert`, but meant for use in monadic contexts. If you throw a `assertM condition` into a
-- do-block, then if the condition fails, the do-block will throw an exception when it reaches that point.

assertM :: Monad m => Bool -> m ()
assertM condition = assert condition (return ())

-- `errorContext` prepends a note to an error message

errorContext :: String -> Either String a -> Either String a
errorContext note (Left msg) = Left (note ++ "\n" ++ msg)
errorContext note (Right x) = Right x

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

-- `stepPoint` is used every time we advance one character in the source file.
-- `stepNewlinePoint` is used when we encounter a newline. `stepPoint'` is used
-- to step more than one point at a time.

stepPoint :: Point -> Point
stepPoint = stepPoint' 1

stepPoint' :: Int -> Point -> Point
stepPoint' n (Point l c) = (Point l (c+n))

stepNewlinePoint :: Point -> Point
stepNewlinePoint (Point l c) = (Point (l+1) 1)

