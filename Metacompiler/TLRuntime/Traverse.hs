module Metacompiler.TLRuntime.Traverse where

import Control.Applicative
import Data.Traversable
import Metacompiler.TLRuntime.Types

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
	MTJSExprType equiv -> liftA MTJSExprType (visitO equiv)
	MTJSExpr type_ equiv -> liftA2 MTJSExpr (visitO type_) (visitO equiv)
	where
		visitT = visitMetaType v
		visitO = visitMetaObject v

traverseMetaObject :: Applicative f => Visitor f -> MetaObject -> f MetaObject
traverseMetaObject v t = case t of
	MOApp fun arg -> liftA2 MOApp (visitO fun) (visitO arg)
	MOAbs (paramName, paramType) body -> liftA2 MOAbs (liftA ((,) paramName) (visitT paramType)) (visitO body)
	MOName name type_ -> liftA (MOName name) (visitT type_)
	MOSLType type_ bs -> let
		visitSLTypeBinding (SLTypeBinding v) =
			SLTypeBinding <$> visitO v
		in MOSLType type_ <$> traverse visitSLTypeBinding bs
	MOSLTerm term tybs tebs -> let
		visitSLTypeBinding (SLTypeBinding v) =
			SLTypeBinding <$> visitO v
		visitSLTermBinding (SLTermBinding ps v) =
			SLTermBinding <$> sequenceA [(,) pn <$> visitO pv | (pn, pv) <- ps] <*> visitO v
		in MOSLTerm term <$> traverse visitSLTypeBinding tybs <*> traverse visitSLTermBinding tebs
	MOJSExprTypeDefn defn params -> liftA (MOJSExprTypeDefn defn) (traverse visitO params)
	MOJSExprLiteral equiv type_ expr bindings -> let
		visitJSExprBinding (JSExprBinding params value) =
			JSExprBinding <$> traverse visitJSExprBindingParam params <*> visitO value
		visitJSExprBindingParam (JSExprBindingParam nameOfSL typeOfSL nameOfJS typeOfJS) =
			JSExprBindingParam <$> pure nameOfSL <*> visitO typeOfSL <*> pure nameOfJS <*> visitO typeOfJS
		in MOJSExprLiteral <$> visitO equiv <*> visitO type_ <*> pure expr <*> traverse visitJSExprBinding bindings
	MOJSExprConvertEquiv newEquiv content -> liftA2 MOJSExprConvertEquiv (visitO newEquiv) (visitO content)
	where
		visitT = visitMetaType v
		visitO = visitMetaObject v

