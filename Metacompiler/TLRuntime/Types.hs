module Metacompiler.TLRuntime.Types where

import qualified Data.Map as M
import qualified Metacompiler.JS.JS as JS
import qualified Metacompiler.SLRuntime.Types as SLR

newtype NameOfMetaObject = NameOfMetaObject { unNameOfMetaObject :: String } deriving (Ord, Show, Eq)

data JSExprTypeDefn = JSExprTypeDefn {
	nameOfJSExprTypeDefn :: NameOfMetaObject,
	paramsOfJSExprTypeDefn :: [MetaType],
	slEquivOfJSExprTypeDefn :: [MetaObject] -> MetaObject
	}

data MetaType
	= MTFun (NameOfMetaObject, MetaType) MetaType
	| MTSLType SLKind
	| MTSLTerm MetaObject
	| MTJSExprType MetaObject
	| MTJSExpr MetaObject MetaObject   -- SL type, SL equivalent

data MetaObject
	= MOApp MetaObject MetaObject
	| MOAbs (NameOfMetaObject, MetaType) MetaObject
	| MOName NameOfMetaObject MetaType
	| MOSLType SLR.Type (M.Map SLR.NameOfType SLTypeBinding)
	| MOSLTerm SLR.Term (M.Map SLR.NameOfType SLTypeBinding) (M.Map SLR.NameOfTerm SLTermBinding)
	| MOJSExprTypeDefn JSExprTypeDefn [MetaObject]
	| MOJSExprLiteral MetaObject MetaObject (JS.Expression ()) (M.Map (JS.Id ()) JSExprBinding)   -- equiv, type
	| MOJSExprConvertEquiv MetaObject MetaObject   -- new equivalent, expression to convert

data SLTypeBinding = SLTypeBinding {
	valueOfSLTypeBinding :: MetaObject
	}

data SLTermBinding = SLTermBinding {
	paramsOfSLTermBinding :: [(NameOfMetaObject, MetaObject)],
	valueOfSLTermBinding :: MetaObject
	}

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

