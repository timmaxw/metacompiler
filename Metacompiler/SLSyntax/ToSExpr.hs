module Metacompiler.SLSyntax.ToSExpr where

import Metacompiler.Error
import Metacompiler.SExpr.Format
import Metacompiler.SExpr.Types
import Metacompiler.SExpr.UtilsTo
import Metacompiler.SLSyntax.Types

formatKindAsSExpr :: Kind a -> SExpr
formatKindAsSExpr (KindType _) =
	mkAtom "*"
formatKindAsSExpr (KindFun _ as r) =
	mkList' ([mkAtom "fun"] ++ map formatKindAsSExpr as ++ [mkAtom "->", formatKindAsSExpr r])

formatKindAsSExprs :: Kind a -> SExprs
formatKindAsSExprs other =
	sExprsFromList [formatKindAsSExpr other]

formatKindAsString :: Kind a -> String
formatKindAsString = formatSExpr . formatKindAsSExpr

formatTypeAsSExpr :: Type a -> SExpr
formatTypeAsSExpr (TypeName _ (NameOfType name)) =
	mkAtom name
formatTypeAsSExpr (TypeFun _ as r) =
	mkList' $
		[mkAtom "fun"] ++
		map formatTypeAsSExpr as ++
		[mkAtom "->"] ++
		sExprsToList (formatTypeAsSExprs r)
formatTypeAsSExpr (TypeLazy _ x) =
	mkList' [mkAtom "lazy", formatTypeAsSExpr x]
formatTypeAsSExpr other =
	mkList (formatTypeAsSExprs other)

formatTypeAsSExprs :: Type a -> SExprs
formatTypeAsSExprs (TypeApp _ f a) = let
	f' = sExprsToList (formatTypeAsSExprs f)
	in sExprsFromList (f' ++ [formatTypeAsSExpr a])
formatTypeAsSExprs other =
	sExprsFromList [formatTypeAsSExpr other]

formatTypeAsString :: Type a -> String
formatTypeAsString = formatSExpr . formatTypeAsSExpr

formatTermAsSExpr :: Term a -> SExpr
formatTermAsSExpr t@(TermName _ (NameOfTerm name) typs) =
	case typs of
		[] -> mkAtom name
		_ -> mkList (formatTermAsSExprs t)
formatTermAsSExpr (TermAbs _ as b) =
	mkList' $
		[mkAtom "\\"] ++
		[mkList' ([mkAtom n, mkAtom "::"] ++ sExprsToList (formatTypeAsSExprs t))
			| (NameOfTerm n, t) <- as] ++
		[mkAtom "->"] ++
		sExprsToList (formatTermAsSExprs b)
formatTermAsSExpr (TermCase _ s cs) =
	mkList' $
		[mkAtom "case", formatTermAsSExpr s, mkAtom "of"] ++
		concat [[
				-- TODO: Type parameters on ctor
				mkList' ([mkAtom (unNameOfTerm c)] ++ map mkAtom [n | NameOfTerm n <- fns]),
				mkAtom "->",
				formatTermAsSExpr b
			]
			| (c, tps, fns, b) <- cs]
formatTermAsSExpr (TermWrap _ x) =
	mkList' [mkAtom "wrap", formatTermAsSExpr x]
formatTermAsSExpr (TermUnwrap _ x) =
	mkList' [mkAtom "unwrap", formatTermAsSExpr x]
formatTermAsSExpr other =
	mkList (formatTermAsSExprs other)

formatTermAsSExprs :: Term a -> SExprs
formatTermAsSExprs (TermApp _ f a) = let
	f' = sExprsToList (formatTermAsSExprs f)
	in sExprsFromList (f' ++ [formatTermAsSExpr a])
formatTermAsSExprs (TermName _ (NameOfTerm name) typs) =
	sExprsFromList $ [mkAtom name] ++ map formatTypeAsSExpr typs ++ [mkAtom "."]
formatTermAsSExprs other =
	sExprsFromList [formatTermAsSExpr other]

formatTermAsString :: Term a -> String
formatTermAsString = formatSExpr . formatTermAsSExpr

