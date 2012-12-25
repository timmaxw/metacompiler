module Metacompiler.TLRuntime where

import qualified Metacompiler.SLRuntime as SLR

newtype Name = Name { unName :: String } deriving (Ord, Eq)

data MetaType
	= MTFun (Name, MetaType) MetaType
	| MTSLType
	| MTSLTerm MetaObject
	| MTJSEquivExprType MetaObject
	| MTJSEquivExpr MetaObject MetaObject

data MetaObject
	= MOApp MetaObject MetaObject
	| MOAbs (Name, MetaType) MetaObject
	| MOName Name MetaType
	| MOSLTypeLiteral SLR.Type (M.Map SLR.Name BindSLType)
	| MOSLTermLiteral SLR.Term (M.Map SLR.Name BindSLType) (M.Map SLR.Name ([Name], MetaObject))
	| MOJSEquivExprLiteral MetaObject MetaObject (JS.Expression ())

data BindSLType = BindSLType {
	typeParamsOfBindSLType :: [Name],
	valueOfBindSLType :: MetaObject
	}

data BindSLTerm = BindSLTerm {
	typeParamsOfBindSLTerm 

typeOfMetaObject :: MetaObject -> MetaType
typeOfMetaObject (MOApp fun arg) = case typeOfMetaObject fun of
	MTFun (paramName, _) bodyType -> reduceMetaType (substituteMetaType (M.singleton paramName arg) bodyType)
	_ -> error "badly typed meta-object"
typeOfMetaObject (MOAbs (paramName, paramType) body) = MTFun (paramName, paramType) (typeOfMetaObject body)
typeOfMetaObject (MOName _ type_) = type_
typeOfMetaObject (MOSLTypeLiteral _ _) = MTSLType
typeOfMetaObject (MOSLTermLiteral term typeBinds _) = MTSLTerm (MOSLTypeLiteral (SLR.typeOfTerm term) typeBinds)
typeOfMetaObject (MOJSExprLiteral _ _) = MTJSExpr
typeOfMetaObject (MOJSStatementLiteral _ _) = MTJSStatement
typeOfMetaObject (MOJSEquivExprLiteral slEquiv type_ _) = MTJSEquivExpr slEquiv type_
typeOfMetaObject (MOJSEquivExprUnwrap _) = MTJSExpr

substituteMetaType :: M.Map Name MetaObject -> MetaType -> MetaType
...

substituteMetaObject :: M.Map Name MetaObject -> MetaObject -> MetaObject
...

reduceMetaType :: MetaType -> MetaType
...

reduceMetaObject :: MetaObject -> MetaObject
...

