module Metacompiler.SLRuntime.Traverse where

import Control.Applicative
import Data.Traversable
import Metacompiler.SLRuntime.Types

-- `traverseType` and `traverseTerm` invoke `visitType` or `visitTerm` of the given visitor on each sub-node of
-- the given meta-type or meta-object, then combine the results using an applicative functor. They are a generic way to
-- implement many different things with a minimum of boilerplate.

data TypeVisitor f = TypeVisitor {
	visitType :: Type -> f Type,
	}

data TermVisitor f = TermVisitor {
	getTypeVisitor :: TypeVisitor f,
	visitTerm :: Term -> f Term
	}

traverseType :: Applicative f => TypeVisitor f -> Type -> f Type
traverseType v t = case t of
	TypeDefined d -> pure (TypeDefined d)
	TypeName n -> pure (TypeName n)
	TypeApp f x -> TypeApp <$> visitTy f <*> visitTy x
	TypeFun a r -> TypeFun <$> visitTy a <*> visitTy r
	TypeLazy x -> TypeLazy <$> visitTy x
	where
		visitTy = visitType v

traverseTerm :: Applicative f => TermVisitor f -> Term -> f Term
traverseTerm v t = case t of
	TermDefined defn tps -> TermDefined defn <*> traverse visitTy tps
	TermName name -> pure (TermName name)
	TermApp f x -> TermApp <$> visitTe f <*> visitTe x
	TermAbs (an, at) b -> TermAbs <$> ((,) paramName <$> visitTy at) <*> visitTe b
	TermCase s cs -> TermCase
		<$> visitTe s
		<*> traverse (\(c, tps, fns, b) ->
			(,,,) c
			<$> traverse visitTe tps
			<*> pure fns
			<*> visitTe b
			) cs
	TermData c tps fs -> TermData c <$> traverse visitTy tps <*> traverse visitTe fs
	TermWrap x -> TermWrap <$> visitTe x
	TermUnwrap x -> TermUnwrap <$> visitTe x
	where
		visitTy = visitType (getTypeVisitor v)
		visitTe = visitTerm v

