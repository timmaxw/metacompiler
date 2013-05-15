module Metacompiler.SLRuntime.TypeOf where

import Metacompiler.SLRuntime.Types

kindOfType :: Type -> Kind
kindOfType (TypeDefined defn) = foldr KindFun KindType (typeParamsOfSLDataDefn defn)
kindOfType (TypeName _ kind) = kind
kindOfType (TypeApp f _) = case kindOfType f of
	KindFun _ r -> r
	_ -> error "badly kinded type"
kindOfType (TypeFun _ _) = KindType
kindOfType (TypeLazy _) = KindType

typeOfTerm :: Term -> Type
typeOfTerm (TermDefined defn tps) = typeOfTermDefn defn tps
typeOfTerm (TermName _ type_) = type_
typeOfTerm (TermApp f _) = case typeOfTerm f of
	TypeFun _ r -> r
	_ -> error "badly typed term"
typeOfTerm (TermAbs (_, a) b) = TypeFun a (typeOfTerm b)
typeOfTerm (TermCase _ cs) = case cs of
	(_, _, _, b):_ -> typeOfTerm b
	[] -> error "case with no clauses"
typeOfTerm (TermData c tps _) = TypeDefined (parentDataOfSLCtorDefn c)
typeOfTerm (TermWrap x) = TypeLazy (typeOfTerm x)
typeOfTerm (TermUnwrap x) = case typeOfTerm x of
	TypeLazy t -> t
	_ -> error "badly typed term"

