module Metacompiler.SL.ToSExpr where

import Metacompiler.Error
import Metacompiler.SExpr.Format
import Metacompiler.SExpr.Types
import Metacompiler.SExpr.UtilsTo
import qualified Metacompiler.SL.Syntax as SL

formatSLKindAsSExpr :: SL.Kind a -> SExpr
formatSLKindAsSExpr (SL.KindType _) =
	mkAtom "*"
formatSLKindAsSExpr (SL.KindFun _ as r) =
	mkList' ([mkAtom "fun"] ++ map formatSLKindAsSExpr as ++ [mkAtom "->", formatSLKindAsSExpr r])

formatSLKindAsSExprs :: SL.Kind a -> SExprs
formatSLKindAsSExprs other =
	sExprsFromList [formatSLKindAsSExpr other]

formatSLKindAsString :: SL.Kind a -> String
formatSLKindAsString = formatSExpr . formatSLKindAsSExpr

formatSLTypeAsSExpr :: SL.Type a -> SExpr
formatSLTypeAsSExpr (SL.TypeName _ (SL.NameOfType name)) =
	mkAtom name
formatSLTypeAsSExpr (SL.TypeFun _ as r) =
	mkList' $
		[mkAtom "fun"] ++
		map formatSLTypeAsSExpr as ++
		[mkAtom "->"] ++
		sExprsToList (formatSLTypeAsSExprs r)
formatSLTypeAsSExpr (SL.TypeLazy _ x) =
	mkList' [mkAtom "lazy", formatSLTypeAsSExpr x]
formatSLTypeAsSExpr other =
	mkList (formatSLTypeAsSExprs other)

formatSLTypeAsSExprs :: SL.Type a -> SExprs
formatSLTypeAsSExprs (SL.TypeApp _ f a) = let
	f' = sExprsToList (formatSLTypeAsSExprs f)
	in sExprsFromList (f' ++ [formatSLTypeAsSExpr a])
formatSLTypeAsSExprs other =
	sExprsFromList [formatSLTypeAsSExpr other]

formatSLTypeAsString :: SL.Type a -> String
formatSLTypeAsString = formatSExpr . formatSLTypeAsSExpr

formatSLTermAsSExpr :: SL.Term a -> SExpr
formatSLTermAsSExpr (SL.TermName _ (SL.NameOfTerm name) typs) =
	case typs of
		[] -> mkAtom name
		_ -> error "type parameters on SL term names not really supported yet"
formatSLTermAsSExpr (SL.TermAbs _ as b) =
	mkList' $
		[mkAtom "\\"] ++
		[mkList' ([mkAtom n, mkAtom "::"] ++ sExprsToList (formatSLTypeAsSExprs t))
			| (SL.NameOfTerm n, t) <- as] ++
		[mkAtom "->"] ++
		sExprsToList (formatSLTermAsSExprs b)
formatSLTermAsSExpr (SL.TermCase _ s cs) =
	mkList' $
		[mkAtom "case", formatSLTermAsSExpr s, mkAtom "of"] ++
		concat [[
				-- TODO: Type parameters on ctor
				mkList' ([mkAtom (SL.unNameOfTerm c)] ++ map mkAtom [n | SL.NameOfTerm n <- fns]),
				mkAtom "->",
				formatSLTermAsSExpr b
			]
			| (c, tps, fns, b) <- cs]
formatSLTermAsSExpr (SL.TermWrap _ x) =
	mkList' [mkAtom "wrap", formatSLTermAsSExpr x]
formatSLTermAsSExpr (SL.TermUnwrap _ x) =
	mkList' [mkAtom "unwrap", formatSLTermAsSExpr x]
formatSLTermAsSExpr other =
	mkList (formatSLTermAsSExprs other)

formatSLTermAsSExprs :: SL.Term a -> SExprs
formatSLTermAsSExprs (SL.TermApp _ f a) = let
	f' = sExprsToList (formatSLTermAsSExprs f)
	in sExprsFromList (f' ++ [formatSLTermAsSExpr a])
formatSLTermAsSExprs other =
	sExprsFromList [formatSLTermAsSExpr other]

formatSLTermAsString :: SL.Term a -> String
formatSLTermAsString = formatSExpr . formatSLTermAsSExpr

