module Metacompiler.SExpr.Format (
	formatSExpr, formatSExprs, summarizeSExpr, summarizeSExprs
	) where

-- `formatSExpr` and `formatSExprs` convert the given S-expression back into textual form, using the same syntax that
-- is accepted by `parseSExpr`.

formatSExpr :: SExpr -> String
formatSExpr = summarizeSExpr' maxBound

formatSExprs :: SExprs -> String
formatSExprs = summarizeSExpr' maxBound

-- `summarizeSExpr` and `summarizeSExprs` are for formatting error messages. They return a textual representation of
-- the `SExpr` or `SExprs`, with parts replaced with `...` as necessary to keep it from getting too long.

summarizeSExpr :: SExpr -> String
summarizeSExpr e = summarizeSExpr' 2 e

summarizeSExprs :: SExprs -> String
summarizeSExprs e = summarizeSExprs' 2 e

-- `summarizeSExpr'` and `summarizeSExprs'` are internal functions used to implement both the `format...` and
-- `summarize...` variants

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

