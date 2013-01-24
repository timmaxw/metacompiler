module Metacompiler.Runtime where

import Control.Applicative
import Control.Monad.Writer
import Data.Foldable (all)
import Data.Functor.Identity
import Data.List (find)
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Set as S
import Data.Traversable (traverse)
import qualified Language.ECMAScript3.Syntax as JS
import Prelude hiding (all)

newtype Name = Name { unName :: String } deriving (Ord, Show, Eq)
newtype NameOfSLType = NameOfSLType { unNameOfSLType :: String } deriving (Eq, Show, Ord)
newtype NameOfSLTerm = NameOfSLTerm { unNameOfSLTerm :: String } deriving (Eq, Show, Ord)
newtype NameOfSLCtor = NameOfSLCtor { unNameOfSLCtor :: String } deriving (Eq, Show, Ord)

data SLKind
	= SLKindType
	| SLKindFun SLKind SLKind
	deriving (Show, Eq)

data SLCtor = SLCtor {
	nameOfSLCtor :: NameOfSLCtor,
	typeParamsOfSLCtor :: [SLKind],
	fieldsOfSLCtor :: [[MetaObject] -> MetaObject],
	typeOfSLCtor :: [MetaObject] -> MetaObject
	}

instance Show SLCtor where
	show (SLCtor name _ _ _) = "(SLCtor " ++ show name ++ " ...)"

data MetaType
	= MTFun (Name, MetaType) MetaType
	| MTSLType SLKind
	| MTSLTerm MetaObject
	| MTJSExprType MetaObject
	| MTJSExpr MetaObject MetaObject
	deriving Show

data MetaObject
	= MOApp MetaObject MetaObject
	| MOAbs (Name, MetaType) MetaObject
	| MOName Name MetaType

	| MOSLTypeName NameOfSLType SLKind
	| MOSLTypeApp MetaObject MetaObject
	| MOSLTypeFun MetaObject MetaObject
	| MOSLTypeLazy MetaObject

	| MOSLTermName NameOfSLTerm [MetaObject] MetaObject
	| MOSLTermApp MetaObject MetaObject
	| MOSLTermAbs (NameOfSLTerm, MetaObject) MetaObject
	| MOSLTermCase MetaObject [(SLCtor, [MetaObject], [NameOfSLTerm], MetaObject)]
	| MOSLTermData SLCtor [MetaObject] [MetaObject]
	| MOSLTermWrap MetaObject
	| MOSLTermUnwrap MetaObject

	| MOJSExprType Name [MetaObject] MetaObject

	| MOJSExprLiteral MetaObject MetaObject (JS.Expression ()) (M.Map (JS.Id ()) JSExprBinding)

	deriving Show

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

typeOfMetaObject :: MetaObject -> MetaType
typeOfMetaObject (MOApp fun arg) = case typeOfMetaObject fun of
	MTFun (paramName, _) bodyType -> reduceMetaType $
		substituteMetaType (Substitutions (M.singleton paramName arg) M.empty M.empty) bodyType
	_ -> error "bad meta-object: MOApp of non-function"
typeOfMetaObject (MOAbs (paramName, paramType) body) = MTFun (paramName, paramType) (typeOfMetaObject body)
typeOfMetaObject (MOName _ type_) = type_
typeOfMetaObject (MOSLTypeName _ kind) = MTSLType kind
typeOfMetaObject (MOSLTypeApp fun _) = case typeOfMetaObject fun of
	MTSLType (SLKindFun _ retKind) -> MTSLType retKind
	_ -> error "bad meta-object: MOSLTypeApp of non-function kind"
typeOfMetaObject (MOSLTypeFun _ _) = MTSLType SLKindType
typeOfMetaObject (MOSLTypeLazy _) = MTSLType SLKindType
typeOfMetaObject (MOSLTermName _ subs type_) = MTSLTerm type_
typeOfMetaObject (MOSLTermApp fun _) = case typeOfMetaObject fun of
	MTSLTerm funSLType -> case reduceMetaObject funSLType of
		MOSLTypeFun _ retSLType -> MTSLTerm retSLType
		_ -> error "bad meta-object: MOSLTypeApp of SL non-function"
	_ -> error "bad meta-object: MOSLTypeApp of non-SL"
typeOfMetaObject (MOSLTermAbs (_, paramType) body) = case typeOfMetaObject body of
	MTSLTerm bodyType -> MTSLTerm (MOSLTypeFun paramType bodyType)
	_ -> error "bad meta-object: MOSLTermAbs of non-SL"
typeOfMetaObject (MOSLTermCase subject clauses) = case clauses of
	(_, _, _, first):_ -> typeOfMetaObject first
	_ -> error "bad meta-object: MOSLTermCase needs at least one clause"
typeOfMetaObject (MOSLTermData ctor typeParams _) = MTSLTerm (typeOfSLCtor ctor typeParams)
typeOfMetaObject (MOSLTermWrap x) = case typeOfMetaObject x of
	MTSLTerm xType -> MTSLTerm (MOSLTypeLazy xType)
typeOfMetaObject (MOSLTermUnwrap x) = case typeOfMetaObject x of
	MTSLTerm xType -> case reduceMetaObject xType of
		MOSLTypeLazy xInnerType -> MTSLTerm xInnerType
		_ -> error "bad meta-object: MOSLTermUnwrap needs a MOSLTypeLazy"
typeOfMetaObject (MOJSExprType _ _ equiv) = MTJSExprType equiv
typeOfMetaObject (MOJSExprLiteral equiv type_ _ _ ) = MTJSExpr equiv type_

slKindOfMetaObject :: MetaObject -> SLKind
slKindOfMetaObject mo = case typeOfMetaObject mo of
	MTSLType k -> k
	_ -> error "slKindOfMetaObject: type is not MTSLType"

slTypeOfMetaObject :: MetaObject -> MetaObject
slTypeOfMetaObject mo = case typeOfMetaObject mo of
	MTSLTerm t -> t
	_ -> error "slTypeOfMetaObject: type is not MTSLTerm"

-- `traverseMetaType` and `traverseMetaObject` invoke `visitMetaType` or `visitMetaObject` of the given visitor on each
-- sub-node of the given meta-type or meta-object, then combine the results using an applicative functor. They are a
-- generic way to implement many different things with a minimum of boilerplate.

data Visitor f = Visitor {
	visitMetaType :: MetaType -> f MetaType,
	visitMetaObject :: MetaObject -> f MetaObject
	}

defaultVisitor :: Applicative f => Visitor f -> Visitor f
defaultVisitor subVisitor = Visitor {
	visitMetaType = traverseMetaType subVisitor,
	visitMetaObject = traverseMetaObject subVisitor
	}

traverseMetaType :: Applicative f => Visitor f -> MetaType -> f MetaType
traverseMetaType v t = case t of
	MTFun (paramName, paramType) returnType -> liftA2 MTFun (liftA ((,) paramName) (visitT paramType)) (visitT returnType)
	MTSLType kind -> pure (MTSLType kind)
	MTSLTerm type_ -> liftA MTSLTerm (visitO type_)
	where
		visitT = visitMetaType v
		visitO = visitMetaObject v

traverseMetaObject :: Applicative f => Visitor f -> MetaObject -> f MetaObject
traverseMetaObject v t = case t of
	MOApp fun arg -> liftA2 MOApp (visitO fun) (visitO arg)
	MOAbs (paramName, paramType) body -> liftA2 MOAbs (liftA ((,) paramName) (visitT paramType)) (visitO body)
	MOName name type_ -> liftA (MOName name) (visitT type_)
	MOSLTypeName name kind -> pure (MOSLTypeName name kind)
	MOSLTypeApp fun arg -> liftA2 MOSLTypeApp (visitO fun) (visitO arg)
	MOSLTypeFun argType retType -> liftA2 MOSLTypeFun (visitO argType) (visitO retType)
	MOSLTypeLazy x -> liftA MOSLTypeLazy (visitO x)
	MOSLTermName name params type_ -> liftA2 (MOSLTermName name) (traverse visitO params) (visitO type_)
	MOSLTermApp fun arg -> liftA2 MOSLTermApp (visitO fun) (visitO arg)
	MOSLTermAbs (paramName, paramType) body -> liftA2 MOSLTermAbs (liftA ((,) paramName) (visitO paramType)) (visitO body)
	MOSLTermCase subject clauses -> liftA2 MOSLTermCase
		(visitO subject)
		(traverse (\(ctor, typeParams, fieldNames, body) -> (,,,)
				<$> (pure ctor)
				<*> (traverse visitO typeParams)
				<*> (pure fieldNames)
				<*> (visitO body)
				)
			clauses
			)
	MOSLTermData ctor typeParams fields -> liftA2 (MOSLTermData ctor) (traverse visitO typeParams) (traverse visitO fields)
	MOSLTermWrap x -> liftA MOSLTermWrap (visitO x)
	MOSLTermUnwrap x -> liftA MOSLTermUnwrap (visitO x)
	MOJSExprType x -> liftA MOJSExprType (visitO x)
	MOJSExprLiteral equiv type_ expr bindings -> let
		visitJSExprBinding (JSExprBinding params value) =
			JSExprBinding <$> traverse traverseJSExprBindingParam params <*> visitO value
		visitJSExprBindingParam (JSExprBindingParam nameOfSL typeOfSL nameOfJS typeOfJS) =
			JSExprBindingParam <$> pure nameOfSL <*> visitO typeOfSL <$> pure nameOfJS <*> visitO nameOfJS
		in MOJSExprLiteral <$> visitO equiv <*> visitO type_ <*> pure expr <*> traverse visitJSExprBinding bindings
	where
		visitT = visitMetaType v
		visitO = visitMetaObject v

