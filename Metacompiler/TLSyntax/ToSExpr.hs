module Metacompiler.TLSyntax.ToSExpr where

import qualified Data.List
import qualified Metacompiler.JS.JS as JS
import Metacompiler.Error
import Metacompiler.SExpr.Format
import Metacompiler.SExpr.Types
import Metacompiler.SExpr.UtilsTo
import qualified Metacompiler.SLSyntax.Types as SLS
import qualified Metacompiler.SLSyntax.ToSExpr as SLS
import qualified Metacompiler.TLSyntax.Types as TLS

formatMetaTypeAsSExpr :: TLS.MetaType a -> SExpr
formatMetaTypeAsSExpr (TLS.MTFun _ ps r) =
	mkList' $
		[mkAtom "fun"] ++
		[mkList' ([mkAtom n, mkAtom "::"] ++ sExprsToList (formatMetaTypeAsSExprs t))
			| (TLS.Name n, t) <- ps] ++
		[mkAtom "->"] ++
		sExprsToList (formatMetaTypeAsSExprs r)
formatMetaTypeAsSExpr other =
	mkList (formatMetaTypeAsSExprs other)

formatMetaTypeAsSExprs :: TLS.MetaType a -> SExprs
formatMetaTypeAsSExprs (TLS.MTSLType _ kind) =
	sExprsFromList [mkAtom "sl-type", mkQuoted (SLS.formatKindAsString kind)]
formatMetaTypeAsSExprs (TLS.MTSLTerm _ type_) =
	sExprsFromList [mkAtom "sl-term", formatMetaObjectAsSExpr type_]
formatMetaTypeAsSExprs (TLS.MTJSExprType _ slType) =
	sExprsFromList [mkAtom "js-expr-type", formatMetaObjectAsSExpr slType]
formatMetaTypeAsSExprs (TLS.MTJSExpr _ jsType slTerm) =
	sExprsFromList [mkAtom "js-expr", formatMetaObjectAsSExpr jsType, formatMetaObjectAsSExpr slTerm]
formatMetaTypeAsSExprs other =
	sExprsFromList [formatMetaTypeAsSExpr other]

formatMetaTypeAsString :: TLS.MetaType a -> String
formatMetaTypeAsString = formatSExpr . formatMetaTypeAsSExpr

formatMetaObjectAsSExpr :: TLS.MetaObject a -> SExpr
formatMetaObjectAsSExpr (TLS.MOAbs _ ps r) =
	mkList' $
		[mkAtom "fun"] ++
		[mkList' ([mkAtom n, mkAtom "::"] ++ sExprsToList (formatMetaTypeAsSExprs t))
			| (TLS.Name n, t) <- ps] ++
		[mkAtom "->"] ++
		sExprsToList (formatMetaObjectAsSExprs r)
formatMetaObjectAsSExpr (TLS.MOName _ (TLS.Name n)) =
	mkAtom n
formatMetaObjectAsSExpr (TLS.MOSLTypeLiteral _ c bs) =
	mkList' $
		[mkAtom "sl-type", mkQuoted (SLS.formatTypeAsString c)] ++
		map (formatTLBindingAsSExpr "type" SLS.unNameOfType) bs
formatMetaObjectAsSExpr (TLS.MOSLTermLiteral _ c tybs tebs) =
	mkList' $
		[mkAtom "sl-term", mkQuoted (SLS.formatTermAsString c)] ++
		map (formatTLBindingAsSExpr "type" SLS.unNameOfType) tybs ++
		map (formatTLBindingAsSExpr "term" SLS.unNameOfTerm) tebs
formatMetaObjectAsSExpr (TLS.MOJSExprLiteral _ e t c bs) =
	mkList' [
		mkAtom "js-expr",
		mkList' ([mkAtom "spec"] ++ sExprsToList (formatMetaObjectAsSExprs e)),
		mkList' ([mkAtom "type"] ++ sExprsToList (formatMetaObjectAsSExprs t)),
		mkList' (
			[mkAtom "impl", mkQuoted (JS.renderExpression c)] ++
			map (formatTLBindingAsSExpr "expr" JS.unId) bs
			)
		]
formatMetaObjectAsSExpr (TLS.MOJSExprConvertEquiv _ i o c) =
	mkList' [
		mkAtom "js-expr-convert-equiv",
		mkList' ([mkAtom "in-spec"] ++ sExprsToList (formatMetaObjectAsSExprs i)),
		mkList' ([mkAtom "out-spec"] ++ sExprsToList (formatMetaObjectAsSExprs o)),
		mkList' ([mkAtom "content"] ++ sExprsToList (formatMetaObjectAsSExprs c))
		]
formatMetaObjectAsSExpr other =
	mkList (formatMetaObjectAsSExprs other)

formatMetaObjectAsSExprs :: TLS.MetaObject a -> SExprs
formatMetaObjectAsSExprs (TLS.MOApp _ f a) = let
	f' = sExprsToList (formatMetaObjectAsSExprs f)
	in sExprsFromList (f' ++ [formatMetaObjectAsSExpr a])
formatMetaObjectAsSExprs other =
	sExprsFromList [formatMetaObjectAsSExpr other]

formatMetaObjectAsString :: TLS.MetaObject a -> String
formatMetaObjectAsString = formatSExpr . formatMetaObjectAsSExpr

formatTLBindingAsSExpr :: String -> (name -> String) -> TLS.Binding a name -> SExpr
formatTLBindingAsSExpr tag nameFun (TLS.Binding _ n ps v) =
	mkList' $
		[mkAtom tag, mkQuoted (nameFun n)] ++
		map formatTLBindingParamAsSExpr ps ++
		[mkAtom "="] ++
		sExprsToList (formatMetaObjectAsSExprs v)

formatTLBindingParamAsSExpr :: TLS.BindingParam a -> SExpr
formatTLBindingParamAsSExpr (TLS.BindingParam _ ps) =
	mkList' $ Data.List.intercalate [mkAtom "|"] $ [
		[mkAtom n, mkAtom "::"] ++
		sExprsToList (formatMetaTypeAsSExprs t)
		| (TLS.Name n, t) <- ps]

