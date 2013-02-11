module Metacompiler.Runtime.Types where

import qualified Data.Map as M
import qualified Language.ECMAScript3.Syntax as JS

{-
import Control.Applicative
import Control.Monad.Writer
import Data.Foldable (all)
import Data.Functor.Identity
import Data.List (find)
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Set as S
import Data.Traversable (traverse)

import Prelude hiding (all)
-}

newtype Name = Name { unName :: String } deriving (Ord, Show, Eq)
newtype NameOfSLType = NameOfSLType { unNameOfSLType :: String } deriving (Eq, Show, Ord)
newtype NameOfSLTerm = NameOfSLTerm { unNameOfSLTerm :: String } deriving (Eq, Show, Ord)
newtype NameOfSLCtor = NameOfSLCtor { unNameOfSLCtor :: String } deriving (Eq, Show, Ord)

data SLKind
	= SLKindType
	| SLKindFun SLKind SLKind
	deriving (Show, Eq)

data SLDataDefn = SLDataDefn {
	nameOfSLDataDefn :: NameOfSLType,
	typeParamsOfSLDataDefn :: [SLKind]
	}

data SLCtorDefn = SLCtorDefn {
	nameOfSLCtorDefn :: NameOfSLCtor,
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
	= MTFun (Name, MetaType) MetaType
	| MTSLType SLKind
	| MTSLTerm MetaObject
	| MTJSExprType MetaObject
	| MTJSExpr MetaObject MetaObject

data MetaObject
	= MOApp MetaObject MetaObject
	| MOAbs (Name, MetaType) MetaObject
	| MOName Name MetaType

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

	| MOJSExprType Name [MetaObject] MetaObject

	| MOJSExprLiteral MetaObject MetaObject (JS.Expression ()) (M.Map (JS.Id ()) JSExprBinding)

data JSExprBinding = JSExprBinding {
	paramsOfJSExprBinding :: [JSExprBindingParam],
	valueOfJSExprBinding :: MetaObject
	}

data JSExprBindingParam = JSExprBindingParam {
	nameOfSLOfJSExprBindingParam :: Name,
	typeOfSLOfJSExprBindingParam :: MetaObject,
	nameOfJSOfJSExprBindingParam :: Name,
	typeOfJSOfJSExprBindingParam :: MetaObject
	}

