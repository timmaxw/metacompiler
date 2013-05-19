module Metacompiler.SLSyntax.ToSExpr where

import Metacompiler.Error
import Metacompiler.SExpr.Format
import Metacompiler.SExpr.Types
import Metacompiler.SExpr.UtilsTo
import Metacompiler.SLSyntax.Types

formatSLKindAsSExpr :: Kind a -> SExpr
formatSLKindAsSExpr (KindType _) =
	mkAtom "*"
formatSLKindAsSExpr (KindFun _ as r) =
	mkList' ([mkAtom "fun"] ++ map formatSLKindAsSExpr as ++ [mkAtom "->", formatSLKindAsSExpr r])

formatSLKindAsSExprs :: Kind a -> SExprs
formatSLKindAsSExprs other =
	sExprsFromList [formatSLKindAsSExpr other]

formatSLKindAsString :: Kind a -> String
formatSLKindAsString = formatSExpr . formatSLKindAsSExpr

formatSLTypeAsSExpr :: Type a -> SExpr
formatSLTypeAsSExpr (TypeName _ (NameOfType name)) =
	mkAtom name
formatSLTypeAsSExpr (TypeFun _ as r) =
	mkList' $
		[mkAtom "fun"] ++
		map formatSLTypeAsSExpr as ++
		[mkAtom "->"] ++
		sExprsToList (formatSLTypeAsSExprs r)
formatSLTypeAsSExpr (TypeLazy _ x) =
	mkList' [mkAtom "lazy", formatSLTypeAsSExpr x]
formatSLTypeAsSExpr other =
	mkList (formatSLTypeAsSExprs other)

formatSLTypeAsSExprs :: Type a -> SExprs
formatSLTypeAsSExprs (TypeApp _ f a) = let
	f' = sExprsToList (formatSLTypeAsSExprs f)
	in sExprsFromList (f' ++ [formatSLTypeAsSExpr a])
formatSLTypeAsSExprs other =
	sExprsFromList [formatSLTypeAsSExpr other]

formatSLTypeAsString :: Type a -> String
formatSLTypeAsString = formatSExpr . formatSLTypeAsSExpr

formatSLTermAsSExpr :: Term a -> SExpr
formatSLTermAsSExpr t@(TermName _ (NameOfTerm name) typs) =
	case typs of
		[] -> mkAtom name
		_ -> mkList (formatSLTermAsSExprs t)
formatSLTermAsSExpr (TermAbs _ as b) =
	mkList' $
		[mkAtom "\\"] ++
		[mkList' ([mkAtom n, mkAtom "::"] ++ sExprsToList (formatSLTypeAsSExprs t))
			| (NameOfTerm n, t) <- as] ++
		[mkAtom "->"] ++
		sExprsToList (formatSLTermAsSExprs b)
formatSLTermAsSExpr (TermCase _ s cs) =
	mkList' $
		[mkAtom "case", formatSLTermAsSExpr s, mkAtom "of"] ++
		concat [[
				-- TODO: Type parameters on ctor
				mkList' ([mkAtom (unNameOfTerm c)] ++ map mkAtom [n | NameOfTerm n <- fns]),
				mkAtom "->",
				formatSLTermAsSExpr b
			]
			| (c, tps, fns, b) <- cs]
formatSLTermAsSExpr (TermWrap _ x) =
	mkList' [mkAtom "wrap", formatSLTermAsSExpr x]
formatSLTermAsSExpr (TermUnwrap _ x) =
	mkList' [mkAtom "unwrap", formatSLTermAsSExpr x]
formatSLTermAsSExpr other =
	mkList (formatSLTermAsSExprs other)

formatSLTermAsSExprs :: Term a -> SExprs
formatSLTermAsSExprs (TermApp _ f a) = let
	f' = sExprsToList (formatSLTermAsSExprs f)
	in sExprsFromList (f' ++ [formatSLTermAsSExpr a])
formatSLTermAsSExprs (TermName _ (NameOfTerm name) typs) =
	sExprsFromList $ [mkAtom name] ++ map formatSLTypeAsSExpr typs ++ [mkAtom "."]
formatSLTermAsSExprs other =
	sExprsFromList [formatSLTermAsSExpr other]

formatSLTermAsString :: Term a -> String
formatSLTermAsString = formatSExpr . formatSLTermAsSExpr

