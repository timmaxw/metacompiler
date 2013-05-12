module Metacompiler.SLRuntime.TypeOf where

kindOfSLType :: SLType -> SLKind
kindOfSLType (SLTypeDefined defn) = foldr SLKindFun SLKindType (typeParamsOfSLDataDefn defn)
kindOfSLType (SLTypeName _ kind) = kind
kindOfSLType (SLTypeApp f _) = case kindOfSLType f of
	SLKindFun _ r -> r
	_ -> error "badly kinded type"
kindOfSLType (SLTypeFun _ _) = SLKindType
kindOfSLType (SLTypeLazy _) = SLKindType

typeOfSLTerm :: SLTerm -> SLType
typeOfSLTerm (SLTermDefined defn tps) = typeOfSLTermDefn defn tps
typeOfSLTerm (SLTermName _ type_) = type_
typeOfSLTerm (SLTermApp f _) = case typeOfSLTerm f of
	SLTypeFun _ r -> r
	_ -> error "badly typed term"
typeOfSLTerm (SLTermAbs (_, a) b) = SLTypeFun a (typeOfSLTerm b)
typeOfSLTerm (SLTermCase _ cs) = case cs of
	(_, _, _, b):_ -> typeOfSLTerm b
	[] -> error "case with no clauses"
typeOfSLTerm (SLTermData c tps _) = SLTypeDefined (parentDataOfSLCtorDefn c)
typeOfSLTerm (SLTermWrap x) = SLTypeLazy (typeOfSLTerm x)
typeOfSLTerm (SLTermUnwrap x) = case typeOfSLTerm x of
	SLTypeLazy t -> t
	_ -> error "badly typed term"

