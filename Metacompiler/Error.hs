module Metacompiler.Error (
	module Control.Exception.Base,   -- re-export `assert`
	assertM,
	Point(..), formatPoint, Range(..), formatRange, stepPoint, stepPoint', stepNewlinePoint,
	Backtrace, BacktraceMonad(..), frameBacktrace, getBacktrace,
	Location(..)
	) where

import Control.Exception.Base (assert)

-- `assertM` is like `assert`, but meant for use in monadic contexts. If you throw a `assertM condition` into a
-- do-block, then if the condition fails, the do-block will throw an exception when it reaches that point.

assertM :: Monad m => Bool -> m ()
assertM condition = assert condition (return ())

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

-- `BacktraceMonad` is a combination of `Reader` and `Either String`. Use `frameBacktrace` to recursively build up a
-- backtrace. If `fail` is called, the backtrace will be prepended to the error message. You can also call
-- `getBacktrace` to get the current backtrace if you need it for something else.

type Backtrace = [String]

data BacktraceMonad a = BacktraceMonad {
	runBacktraceMonad :: Backtrace -> Either String a
	}

instance Monad BacktraceMonad where
	return x = BacktraceMonad (const (Right x))
	a >>= b = BacktraceMonad $ \ bt ->
		case runBacktraceMonad a bt of
			Left err -> Left err
			Right x -> runBacktraceMonad (b x) bt
	fail msg = BacktraceMonad $ \ bt ->
		Left (formatBacktrace bt msg)

frameBacktrace :: String -> BacktraceMonad a -> BacktraceMonad a
frameBacktrace frame inner = BacktraceMonad $ \ frames ->
	runErrorMonad inner (frames ++ [frame])

getBacktrace :: BacktraceMonad Backtrace
getBacktrace = BacktraceMonad $ \ bt -> Right bt

-- `Location` is `Range` combined with a backtrace.

data Location = Location {
	rangeOfLocation :: Range,
	backtraceOfLocation :: Backtrace
	}

getLocation :: Range -> BacktraceMonad Location
getLocation r = do
	bt <- getBacktrace
	return (Location r bt)

