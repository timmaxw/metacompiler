module Metacompiler.TLRuntime where

import qualified Metacompiler.SLRuntime as SLR

newtype Var = Var { unVar :: String } deriving (Ord, Eq)

data MetaType
	= MTFun (Var, MetaType) MetaType
	| MTSLType
	| MTSLTerm MetaObject
	| MTJSExpr
	| MTJSStatement
	| MTJSEquivExprType MetaObject
	| MTJSEquivExpr MetaObject MetaObject

data MetaObject
	= MOApp MetaObject MetaObject
	| MOAbs (Var, MetaType) MetaObject
	| MOVar Var MetaType
	| MOSLTypeLiteral SLR.Type (M.Map SLR.Var MetaObject)
	| MOSLTermLiteral SLR.Term (M.Map SLR.Var MetaObject) (M.Map SLR.Var Var) (M.Map SLR.Var MetaObject)
	| MOJSExprLiteral (JS.Expression ()) (M.Map (JS.Id ()) Var) (M.Map (JS.Id ()) MetaObject)
	| MOJSStatementLiteral [JS.Statement ()] (M.Map (JS.Id ()) Var) (M.Map (JS.Id ()) MetaObject)
	| MOJSEquivExprWrap MetaObject MetaObject MetaObject
	| MOJSEquivExprUnwrap MetaObject

typeOfMetaObject :: MetaObject -> MetaType
typeOfMetaObject (MOApp fun arg) = case typeOfMetaObject fun of
	MTFun (paramName, _) bodyType -> reduceMetaType (substituteMetaType (M.singleton paramName arg) bodyType)
	_ -> error "badly typed meta-object"
typeOfMetaObject (MOAbs (paramName, paramType) body) = MTFun (paramName, paramType) (typeOfMetaObject body)
typeOfMetaObject (MOVar _ type_) = type_
typeOfMetaObject (MOSLTypeLiteral _ _) = MTSLType
typeOfMetaObject (MOSLTermLiteral term typeBinds _ _) = MTSLTerm (MOSLTypeLiteral (SLR.typeOfTerm term) typeBinds)
typeOfMetaObject (MOJSExprLiteral _ _ _) = MTJSExpr
typeOfMetaObject (MOJSStatementLiteral _ _ _) = MTJSStatement
typeOfMetaObject (MOJSEquivExprLiteral slEquiv type_ _) = MTJSEquivExpr slEquiv type_
typeOfMetaObject (MOJSEquivExprUnwrap _) = MTJSExpr

substituteMetaType :: M.Map Var MetaObject -> MetaType -> MetaType
...

substituteMetaObject :: M.Map Var MetaObject -> MetaObject -> MetaObject
...

reduceMetaType :: MetaType -> MetaType
...

reduceMetaObject :: MetaObject -> MetaObject
...

