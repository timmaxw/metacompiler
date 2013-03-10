module Metacompiler.SExpr.Format (
	formatSExpr, formatSExprs, formatSExprLimit, formatSExprsLimit
	) where

import Data.List
import Metacompiler.SExpr.Types

-- `formatSExpr` and `formatSExprs` convert the given S-expression back into textual form, using the same syntax that
-- is accepted by `parseSExpr`.

formatSExpr :: SExpr -> String
formatSExpr = formatSExprLimit maxBound

formatSExprs :: SExprs -> String
formatSExprs = formatSExprsLimit maxBound

-- `formatSExprLimit` and `formatSExprsLimit` put a limit on how long the printout can grow, and replace parts of the
-- expression with `...` as necessary.

formatSExprLimit :: Int -> SExpr -> String
formatSExprLimit _ (Atom _ a) = a
formatSExprLimit 0 _ = "..."
formatSExprLimit level (List _ xs) = "(" ++ formatSExprsLimit level xs ++ ")"
formatSExprLimit level (Quoted _ s)
	-- This is incorrect because Haskell escapes are different from the escapes
	-- that `Metacompiler.ParseSExpr` accepts.
	| length s <= level * 5 = show s
	| otherwise = show (take (level*5-3) s ++ "...")

formatSExprsLimit :: Int -> SExprs -> String
formatSExprsLimit 0 _ = "..."
formatSExprsLimit level xs
	| length list <= level * 3 = intercalate " " (map (formatSExprLimit (level-1)) list)
	| otherwise = intercalate " " (map (formatSExprLimit (level-1)) (take (level*3-1) list)) ++ " ..."
	where list = sExprsToList xs

