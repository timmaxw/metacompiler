module Metacompiler.Runtime.Types where

import qualified Data.Map as M
import qualified Metacompiler.JS as JS

newtype NameOfMetaObject = NameOfMetaObject { unNameOfMetaObject :: String } deriving (Ord, Show, Eq)
newtype NameOfSLType = NameOfSLType { unNameOfSLType :: String } deriving (Eq, Show, Ord)
newtype NameOfSLTerm = NameOfSLTerm { unNameOfSLTerm :: String } deriving (Eq, Show, Ord)

data SLKind
	= SLKindType
	| SLKindFun SLKind SLKind
	deriving (Show, Eq)

data SLDataDefn = SLDataDefn {
	nameOfSLDataDefn :: NameOfSLType,
	typeParamsOfSLDataDefn :: [SLKind]
	}

data SLCtorDefn = SLCtorDefn {
	nameOfSLCtorDefn :: NameOfSLTerm,
	parentDataOfSLCtorDefn :: SLDataDefn,
	fieldTypesOfSLCtorDefn :: [[MetaObject] -> MetaObject]
	}

data SLTermDefn = SLTermDefn {
	nameOfSLTermDefn :: NameOfSLTerm,
	typeParamsOfSLTermDefn :: [SLKind],
	typeOfSLTermDefn :: [MetaObject] -> MetaObject,
	valueOfSLTermDefn :: [MetaObject] -> MetaObject
	}

data MetaType
	= MTFun (NameOfMetaObject, MetaType) MetaType
	| MTSLType SLKind
	| MTSLTerm MetaObject
	| MTJSExprType MetaObject
	| MTJSExpr MetaObject MetaObject   -- SL type, SL equivalent

data JSExprTypeDefn = JSExprTypeDefn {
	nameOfJSExprTypeDefn :: NameOfMetaObject,
	paramsOfJSExprTypeDefn :: [MetaType],
	slEquivOfJSExprTypeDefn :: [MetaObject] -> MetaObject
	}

data MetaObject
	= MOApp MetaObject MetaObject
	| MOAbs (NameOfMetaObject, MetaType) MetaObject
	| MOName NameOfMetaObject MetaType

	| MOSLTypeDefn SLDataDefn
	| MOSLTypeName NameOfSLType SLKind
	| MOSLTypeApp MetaObject MetaObject
	| MOSLTypeFun MetaObject MetaObject
	| MOSLTypeLazy MetaObject

	| MOSLTermDefn SLTermDefn [MetaObject]
	| MOSLTermName NameOfSLTerm MetaObject
	| MOSLTermApp MetaObject MetaObject
	| MOSLTermAbs (NameOfSLTerm, MetaObject) MetaObject
	| MOSLTermCase MetaObject [(SLCtorDefn, [MetaObject], [NameOfSLTerm], MetaObject)]
	| MOSLTermData SLCtorDefn [MetaObject] [MetaObject]
	| MOSLTermWrap MetaObject
	| MOSLTermUnwrap MetaObject

	| MOJSExprTypeDefn JSExprTypeDefn [MetaObject]

	| MOJSExprLiteral MetaObject MetaObject (JS.Expression ()) (M.Map (JS.Id ()) JSExprBinding)   -- equiv, type

	| MOJSExprConvertEquiv MetaObject MetaObject   -- new equivalent, expression to convert

data JSExprBinding = JSExprBinding {
	paramsOfJSExprBinding :: [JSExprBindingParam],
	valueOfJSExprBinding :: MetaObject
	}

data JSExprBindingParam = JSExprBindingParam {
	nameOfSLOfJSExprBindingParam :: NameOfMetaObject,
	typeOfSLOfJSExprBindingParam :: MetaObject,
	nameOfJSOfJSExprBindingParam :: NameOfMetaObject,
	typeOfJSOfJSExprBindingParam :: MetaObject
	}

