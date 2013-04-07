module Metacompiler.TL.Traverse where

import Control.Applicative
import Data.Traversable
import Metacompiler.TL.Syntax

-- `traverse*` invokes `visitMetaType` or `visitMetaObject` of the given visitor on each sub-node of the given object,
-- then combine the results using an applicative functor. They are a generic way to implement many different things
-- with a minimum of boilerplate.

data Visitor f a = Visitor {
	visitMetaType :: MetaType a -> f (MetaType a),
	visitMetaObject :: MetaObject a -> f (MetaObject a)
	}

defaultVisitor :: Applicative f => Visitor f a -> Visitor f a
defaultVisitor subVisitor = Visitor {
	visitMetaType = traverseMetaType subVisitor,
	visitMetaObject = traverseMetaObject subVisitor
	}

traverseMetaType :: Applicative f => Visitor f a -> MetaType a -> f (MetaType a)
traverseMetaType v t = case t of
	MTFun a ps r -> MTFun a <$> sequenceA [(,) pn <$> visitT pt | (pn, pt) <- ps] <*> visitT r
	MTSLType a kind -> pure (MTSLType a kind)
	MTSLTerm a type_ -> MTSLTerm a <$> visitO type_
	MTJSExprType a equiv -> MTJSExprType a <$> visitO equiv
	MTJSExpr a type_ equiv -> MTJSExpr a <$> visitO type_ <*> visitO equiv
	where
		visitT = visitMetaType v
		visitO = visitMetaObject v

traverseMetaObject :: Applicative f => Visitor f a -> MetaObject a -> f (MetaObject a)
traverseMetaObject v t = case t of
	MOApp a fun arg -> MOApp a <$> visitO fun <*> visitO arg
	MOAbs a ps r -> MOAbs a <$> sequenceA [(,) pn <$> visitT pt | (pn, pt) <- ps] <*> visitO r
	MOName a name -> pure (MOName a name)
	MOSLTypeLiteral a c bs -> MOSLTypeLiteral a c <$> traverse (traverseBinding v) bs
	MOSLTermLiteral a c tybs tebs -> MOSLTermLiteral a c <$>
		traverse (traverseBinding v) tybs <*>
		traverse (traverseBinding v) tebs
	MOJSExprLiteral a equiv type_ c bs -> MOJSExprLiteral a <$>
		visitO equiv <*> visitO type_ <*>
		pure c <*> traverse (traverseBinding v) bs
	MOJSExprLoopBreak a equiv type_ content -> MOJSExprLoopBreak a <$>
		visitO equiv <*> visitO type_ <*> visitO content
	MOJSExprConvertEquiv a inEquiv outEquiv content -> MOJSExprConvertEquiv a <$>
		visitO inEquiv <*> visitO outEquiv <*> visitO content
	where
		visitT = visitMetaType v
		visitO = visitMetaObject v

traverseBinding :: Applicative f => Visitor f a -> Binding a n -> f (Binding a n)
traverseBinding v (Binding a name ps val) =
	Binding a name <$>
		sequenceA [BindingParam a' <$>
			sequenceA [(,) pn <$> visitMetaType v pt | (pn, pt) <- parts]	
			| BindingParam a' parts <- ps] <*>
		visitMetaObject v val

traverseDirective :: Applicative f => Visitor f a -> Directive a -> f (Directive a)
traverseDirective v d = case d of
	DLet a name ps ty val -> DLet a name <$>
		sequenceA [(,) pn <$> visitT pt | (pn, pt) <- ps] <*>
		traverse visitT ty <*>
		visitO val
	DSLCode a c -> pure (DSLCode a c)
	DJSExprType a name ps equiv -> DJSExprType a name <$>
		sequenceA [(,) pn <$> visitT pt | (pn, pt) <- ps] <*>
		visitO equiv
	DJSEmit a c bs -> DJSEmit a c <$>
		traverse (traverseBinding v) bs
	where
		visitT = visitMetaType v
		visitO = visitMetaObject v

