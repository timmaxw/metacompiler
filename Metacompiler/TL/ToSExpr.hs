module Metacompiler.TL.ToSExpr where

import qualified Data.List
import qualified Language.ECMAScript3.PrettyPrint as JS
import qualified Metacompiler.JS as JS
import Metacompiler.Error
import Metacompiler.SExpr.Format
import Metacompiler.SExpr.Types
import Metacompiler.SExpr.UtilsTo
import qualified Metacompiler.SL.Syntax as SL
import Metacompiler.SL.ToSExpr
import qualified Metacompiler.TL.Syntax as TL

formatTLMetaTypeAsSExpr :: TL.MetaType a -> SExpr
formatTLMetaTypeAsSExpr (TL.MTFun _ ps r) =
	mkList' $
		[mkAtom "fun"] ++
		[mkList' ([mkAtom n, mkAtom "::"] ++ sExprsToList (formatTLMetaTypeAsSExprs t))
			| (TL.Name n, t) <- ps] ++
		[mkAtom "->"] ++
		sExprsToList (formatTLMetaTypeAsSExprs r)
formatTLMetaTypeAsSExpr other =
	mkList (formatTLMetaTypeAsSExprs other)

formatTLMetaTypeAsSExprs :: TL.MetaType a -> SExprs
formatTLMetaTypeAsSExprs (TL.MTSLType _ kind) =
	sExprsFromList [mkAtom "sl-type", mkQuoted (formatSLKindAsString kind)]
formatTLMetaTypeAsSExprs (TL.MTSLTerm _ type_) =
	sExprsFromList [mkAtom "sl-term", formatTLMetaObjectAsSExpr type_]
formatTLMetaTypeAsSExprs (TL.MTJSExprType _ slType) =
	sExprsFromList [mkAtom "js-expr-type", formatTLMetaObjectAsSExpr slType]
formatTLMetaTypeAsSExprs (TL.MTJSExpr _ jsType slTerm) =
	sExprsFromList [mkAtom "js-expr", formatTLMetaObjectAsSExpr jsType, formatTLMetaObjectAsSExpr slTerm]
formatTLMetaTypeAsSExprs other =
	sExprsFromList [formatTLMetaTypeAsSExpr other]

formatTLMetaTypeAsString :: TL.MetaType a -> String
formatTLMetaTypeAsString = formatSExpr . formatTLMetaTypeAsSExpr

formatTLMetaObjectAsSExpr :: TL.MetaObject a -> SExpr
formatTLMetaObjectAsSExpr (TL.MOAbs _ ps r) =
	mkList' $
		[mkAtom "fun"] ++
		[mkList' ([mkAtom n, mkAtom "::"] ++ sExprsToList (formatTLMetaTypeAsSExprs t))
			| (TL.Name n, t) <- ps] ++
		[mkAtom "->"] ++
		sExprsToList (formatTLMetaObjectAsSExprs r)
formatTLMetaObjectAsSExpr (TL.MOName _ (TL.Name n)) =
	mkAtom n
formatTLMetaObjectAsSExpr (TL.MOSLTypeLiteral _ c bs) =
	mkList' $
		[mkAtom "sl-type", mkQuoted (formatSLTypeAsString c)] ++
		map (formatTLBindingAsSExpr "type" SL.unNameOfType) bs
formatTLMetaObjectAsSExpr (TL.MOSLTermLiteral _ c tybs tebs) =
	mkList' $
		[mkAtom "sl-term", mkQuoted (formatSLTermAsString c)] ++
		map (formatTLBindingAsSExpr "type" SL.unNameOfType) tybs ++
		map (formatTLBindingAsSExpr "term" SL.unNameOfTerm) tebs
formatTLMetaObjectAsSExpr (TL.MOJSExprLiteral _ e t c bs) =
	mkList' [
		mkAtom "js-expr",
		mkList' ([mkAtom "spec"] ++ sExprsToList (formatTLMetaObjectAsSExprs e)),
		mkList' ([mkAtom "type"] ++ sExprsToList (formatTLMetaObjectAsSExprs t)),
		mkList' (
			[mkAtom "impl", mkQuoted (JS.renderExpression c)] ++
			map (formatTLBindingAsSExpr "expr" JS.unId) bs
			)
		]
formatTLMetaObjectAsSExpr (TL.MOJSExprLoopBreak _ e t c) =
	mkList' [
		mkAtom "js-expr-loop-break",
		mkList' ([mkAtom "spec"] ++ sExprsToList (formatTLMetaObjectAsSExprs e)),
		mkList' ([mkAtom "type"] ++ sExprsToList (formatTLMetaObjectAsSExprs t)),
		mkList' ([mkAtom "content"] ++ sExprsToList (formatTLMetaObjectAsSExprs c))
		]
formatTLMetaObjectAsSExpr other =
	mkList (formatTLMetaObjectAsSExprs other)

formatTLMetaObjectAsSExprs :: TL.MetaObject a -> SExprs
formatTLMetaObjectAsSExprs (TL.MOApp _ f a) = let
	f' = sExprsToList (formatTLMetaObjectAsSExprs f)
	in sExprsFromList (f' ++ [formatTLMetaObjectAsSExpr a])
formatTLMetaObjectAsSExprs other =
	sExprsFromList [formatTLMetaObjectAsSExpr other]

formatTLMetaObjectAsString :: TL.MetaObject a -> String
formatTLMetaObjectAsString = formatSExpr . formatTLMetaObjectAsSExpr

formatTLBindingAsSExpr :: String -> (name -> String) -> TL.Binding a name -> SExpr
formatTLBindingAsSExpr tag nameFun (TL.Binding _ n ps v) =
	mkList' $
		[mkAtom tag, mkQuoted (nameFun n)] ++
		map formatTLBindingParamAsSExpr ps ++
		[mkAtom "="] ++
		sExprsToList (formatTLMetaObjectAsSExprs v)

formatTLBindingParamAsSExpr :: TL.BindingParam a -> SExpr
formatTLBindingParamAsSExpr (TL.BindingParam _ ps) =
	mkList' $ Data.List.intercalate [mkAtom "|"] $ [
		[mkAtom n, mkAtom "::"] ++
		sExprsToList (formatTLMetaTypeAsSExprs t)
		| (TL.Name n, t) <- ps]

