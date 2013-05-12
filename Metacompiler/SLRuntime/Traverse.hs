module Metacompiler.SLRuntime.Traverse where

import Control.Applicative
import Data.Traversable
import Metacompiler.SLRuntime.Types

-- `traverseSLType` and `traverseSLTerm` invoke `visitSLType` or `visitSLTerm` of the given visitor on each sub-node of
-- the given meta-type or meta-object, then combine the results using an applicative functor. They are a generic way to
-- implement many different things with a minimum of boilerplate.

data TypeVisitor f = TypeVisitor {
	visitSLType :: SLType -> f SLType,
	}

data TermVisitor f = TermVisitor {
	getTypeVisitor :: TypeVisitor f,
	visitSLTerm :: SLTerm -> f SLTerm
	}

traverseSLType :: Applicative f => TypeVisitor f -> SLType -> f SLType
traverseSLType v t = case t of
	SLTypeDefined d -> pure (SLTypeDefined d)
	SLTypeName n -> pure (SLTypeName n)
	SLTypeApp f x -> SLTypeApp <$> visitTy f <*> visitTy x
	SLTypeFun a r -> SLTypeFun <$> visitTy a <*> visitTy r
	SLTypeLazy x -> SLTypeLazy <$> visitTy x
	where
		visitTy = visitSLType v

traverseSLTerm :: Applicative f => TermVisitor f -> SLTerm -> f SLTerm
traverseSLTerm v t = case t of
	SLTermDefined defn tps -> SLTermDefined defn <*> traverse visitTy tps
	SLTermName name -> pure (SLTermName name)
	SLTermApp f x -> SLTermApp <$> visitTe f <*> visitTe x
	SLTermAbs (an, at) b -> SLTermAbs <$> ((,) paramName <$> visitTy at) <*> visitTe b
	SLTermCase s cs -> SLTermCase
		<$> visitTe s
		<*> traverse (\(c, tps, fns, b) ->
			(,,,) c
			<$> traverse visitTe tps
			<*> pure fns
			<*> visitTe b
			) cs
	SLTermData c tps fs -> SLTermData c <$> traverse visitTy tps <*> traverse visitTe fs
	SLTermWrap x -> SLTermWrap <$> visitTe x
	SLTermUnwrap x -> SLTermUnwrap <$> visitTe x
	where
		visitTy = visitSLType (getTypeVisitor v)
		visitTe = visitSLTerm v

