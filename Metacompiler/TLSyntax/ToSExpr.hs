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

formatTLMetaTypeAsSExpr :: TLS.MetaType a -> SExpr
formatTLMetaTypeAsSExpr (TLS.MTFun _ ps r) =
	mkList' $
		[mkAtom "fun"] ++
		[mkList' ([mkAtom n, mkAtom "::"] ++ sExprsToList (formatTLMetaTypeAsSExprs t))
			| (TLS.Name n, t) <- ps] ++
		[mkAtom "->"] ++
		sExprsToList (formatTLMetaTypeAsSExprs r)
formatTLMetaTypeAsSExpr other =
	mkList (formatTLMetaTypeAsSExprs other)

formatTLMetaTypeAsSExprs :: TLS.MetaType a -> SExprs
formatTLMetaTypeAsSExprs (TLS.MTSLType _ kind) =
	sExprsFromList [mkAtom "sl-type", mkQuoted (formatSLKindAsString kind)]
formatTLMetaTypeAsSExprs (TLS.MTSLTerm _ type_) =
	sExprsFromList [mkAtom "sl-term", formatTLMetaObjectAsSExpr type_]
formatTLMetaTypeAsSExprs (TLS.MTJSExprType _ slType) =
	sExprsFromList [mkAtom "js-expr-type", formatTLMetaObjectAsSExpr slType]
formatTLMetaTypeAsSExprs (TLS.MTJSExpr _ jsType slTerm) =
	sExprsFromList [mkAtom "js-expr", formatTLMetaObjectAsSExpr jsType, formatTLMetaObjectAsSExpr slTerm]
formatTLMetaTypeAsSExprs other =
	sExprsFromList [formatTLMetaTypeAsSExpr other]

formatTLMetaTypeAsString :: TLS.MetaType a -> String
formatTLMetaTypeAsString = formatSExpr . formatTLMetaTypeAsSExpr

formatTLMetaObjectAsSExpr :: TLS.MetaObject a -> SExpr
formatTLMetaObjectAsSExpr (TLS.MOAbs _ ps r) =
	mkList' $
		[mkAtom "fun"] ++
		[mkList' ([mkAtom n, mkAtom "::"] ++ sExprsToList (formatTLMetaTypeAsSExprs t))
			| (TLS.Name n, t) <- ps] ++
		[mkAtom "->"] ++
		sExprsToList (formatTLMetaObjectAsSExprs r)
formatTLMetaObjectAsSExpr (TLS.MOName _ (TLS.Name n)) =
	mkAtom n
formatTLMetaObjectAsSExpr (TLS.MOSLTypeLiteral _ c bs) =
	mkList' $
		[mkAtom "sl-type", mkQuoted (formatSLTypeAsString c)] ++
		map (formatTLBindingAsSExpr "type" SLS.unNameOfType) bs
formatTLMetaObjectAsSExpr (TLS.MOSLTermLiteral _ c tybs tebs) =
	mkList' $
		[mkAtom "sl-term", mkQuoted (formatSLTermAsString c)] ++
		map (formatTLBindingAsSExpr "type" SLS.unNameOfType) tybs ++
		map (formatTLBindingAsSExpr "term" SLS.unNameOfTerm) tebs
formatTLMetaObjectAsSExpr (TLS.MOJSExprLiteral _ e t c bs) =
	mkList' [
		mkAtom "js-expr",
		mkList' ([mkAtom "spec"] ++ sExprsToList (formatTLMetaObjectAsSExprs e)),
		mkList' ([mkAtom "type"] ++ sExprsToList (formatTLMetaObjectAsSExprs t)),
		mkList' (
			[mkAtom "impl", mkQuoted (JS.renderExpression c)] ++
			map (formatTLBindingAsSExpr "expr" JS.unId) bs
			)
		]
formatTLMetaObjectAsSExpr (TLS.MOJSExprConvertEquiv _ i o c) =
	mkList' [
		mkAtom "js-expr-convert-equiv",
		mkList' ([mkAtom "in-spec"] ++ sExprsToList (formatTLMetaObjectAsSExprs i)),
		mkList' ([mkAtom "out-spec"] ++ sExprsToList (formatTLMetaObjectAsSExprs o)),
		mkList' ([mkAtom "content"] ++ sExprsToList (formatTLMetaObjectAsSExprs c))
		]
formatTLMetaObjectAsSExpr other =
	mkList (formatTLMetaObjectAsSExprs other)

formatTLMetaObjectAsSExprs :: TLS.MetaObject a -> SExprs
formatTLMetaObjectAsSExprs (TLS.MOApp _ f a) = let
	f' = sExprsToList (formatTLMetaObjectAsSExprs f)
	in sExprsFromList (f' ++ [formatTLMetaObjectAsSExpr a])
formatTLMetaObjectAsSExprs other =
	sExprsFromList [formatTLMetaObjectAsSExpr other]

formatTLMetaObjectAsString :: TLS.MetaObject a -> String
formatTLMetaObjectAsString = formatSExpr . formatTLMetaObjectAsSExpr

formatTLBindingAsSExpr :: String -> (name -> String) -> TLS.Binding a name -> SExpr
formatTLBindingAsSExpr tag nameFun (TLS.Binding _ n ps v) =
	mkList' $
		[mkAtom tag, mkQuoted (nameFun n)] ++
		map formatTLBindingParamAsSExpr ps ++
		[mkAtom "="] ++
		sExprsToList (formatTLMetaObjectAsSExprs v)

formatTLBindingParamAsSExpr :: TLS.BindingParam a -> SExpr
formatTLBindingParamAsSExpr (TLS.BindingParam _ ps) =
	mkList' $ Data.List.intercalate [mkAtom "|"] $ [
		[mkAtom n, mkAtom "::"] ++
		sExprsToList (formatTLMetaTypeAsSExprs t)
		| (TLS.Name n, t) <- ps]

