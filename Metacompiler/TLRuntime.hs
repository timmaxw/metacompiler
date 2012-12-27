module Metacompiler.TLRuntime where

import qualified Metacompiler.SLRuntime as SLR

newtype Name = Name { unName :: String } deriving (Ord, Eq)

data MetaType
	= MTFun (Name, MetaType) MetaType
	| MTSLType SLR.Kind
	| MTSLTerm MetaObject
	| MTJSEquivExprType MetaObject
	| MTJSEquivExpr MetaObject MetaObject

data MetaObject
	= MOApp MetaObject MetaObject
	| MOAbs (Name, MetaType) MetaObject
	| MOName Name MetaType
	| MOSLTypeLiteral SLR.Type (M.Map SLR.NameOfType BindingSLType)
	| MOSLTermLiteral SLR.Term (M.Map SLR.NameOfType BindingSLType) (M.Map SLR.NameOfTerm BindingSLTerm)
	| MOJSEquivExprLiteral MetaObject MetaObject (JS.Expression ()) (M.Map (JS.Id ()) BindingJSEquivExpr)

data BindingSLType = BindingSLType {
	typeParamsOfBindingSLType :: [(Name, Kind)],
	valueOfBindingSLType :: MetaObject
	}

data BindingSLTerm = BindingSLTerm {
	typeParamsOfBindingSLTerm :: [(Name, Kind)],
	termParamsOfBindingSLTerm :: [(Name, MetaObject)],
	valueOfBindingSLTerm :: MetaObject
	}

data BindingJSEquivExpr = BindingJSEquivExpr {
	paramsOfBindingJSEquivExpr :: [(Name, MetaObject, Name, MetaObject)],
	valueOfBindingJSEquivExpr :: MetaObject
	}

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

