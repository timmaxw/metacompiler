module Metacompiler.Runtime.TypeOf where

import qualified Data.Map as M
import Metacompiler.Runtime.Reduce
import Metacompiler.Runtime.Substitute
import Metacompiler.Runtime.Types

kindOfSLDataDefn :: SLDataDefn -> SLKind
kindOfSLDataDefn defn = foldl SLKindFun SLKindType (typeParamsOfSLDataDefn defn)

typeOfMetaObject :: MetaObject -> MetaType
typeOfMetaObject (MOApp fun arg) =
	case typeOfMetaObject fun of
		MTFun (paramName, _) bodyType -> reduceMetaType $
			substituteMetaType (Substitutions (M.singleton paramName arg) M.empty M.empty) bodyType
		_ -> error "bad meta-object: MOApp of non-function"
typeOfMetaObject (MOAbs (paramName, paramType) body) =
	MTFun (paramName, paramType) (typeOfMetaObject body)
typeOfMetaObject (MOName _ type_) =
	type_
typeOfMetaObject (MOSLTypeDefn defn) =
	MTSLType (kindOfSLDataDefn defn)
typeOfMetaObject (MOSLTypeName _ kind) =
	MTSLType kind
typeOfMetaObject (MOSLTypeApp fun _) =
	case typeOfMetaObject fun of
		MTSLType (SLKindFun _ retKind) -> MTSLType retKind
		_ -> error "bad meta-object: MOSLTypeApp of non-function kind"
typeOfMetaObject (MOSLTypeFun _ _) =
	MTSLType SLKindType
typeOfMetaObject (MOSLTypeLazy _) =
	MTSLType SLKindType
typeOfMetaObject (MOSLTermDefn defn typeParams) =
	MTSLTerm (typeOfSLTermDefn defn typeParams)
typeOfMetaObject (MOSLTermName _ type_) =
	MTSLTerm type_
typeOfMetaObject (MOSLTermApp fun _) =
	case typeOfMetaObject fun of
		MTSLTerm funSLType -> case reduceMetaObject funSLType of
			MOSLTypeFun _ retSLType -> MTSLTerm retSLType
			_ -> error "bad meta-object: MOSLTypeApp of SL non-function"
		_ -> error "bad meta-object: MOSLTypeApp of non-SL"
typeOfMetaObject (MOSLTermAbs (_, paramType) body) =
	case typeOfMetaObject body of
		MTSLTerm bodyType -> MTSLTerm (MOSLTypeFun paramType bodyType)
		_ -> error "bad meta-object: MOSLTermAbs of non-SL"
typeOfMetaObject (MOSLTermCase subject clauses) =
	case clauses of
		(_, _, _, first):_ -> typeOfMetaObject first
		_ -> error "bad meta-object: MOSLTermCase needs at least one clause"
typeOfMetaObject (MOSLTermData ctor typeParams _) = let
	dataType = MOSLTypeDefn (parentDataOfSLCtorDefn ctor)
	in MTSLTerm (foldl MOSLTypeApp dataType typeParams)
typeOfMetaObject (MOSLTermWrap x) =
	case typeOfMetaObject x of
		MTSLTerm xType -> MTSLTerm (MOSLTypeLazy xType)
typeOfMetaObject (MOSLTermUnwrap x) =
	case typeOfMetaObject x of
		MTSLTerm xType -> case reduceMetaObject xType of
			MOSLTypeLazy xInnerType -> MTSLTerm xInnerType
			_ -> error "bad meta-object: MOSLTermUnwrap needs a MOSLTypeLazy"
typeOfMetaObject (MOJSExprTypeDefn defn params) =
	MTJSExprType (slEquivOfJSExprTypeDefn defn params)
typeOfMetaObject (MOJSExprLiteral equiv type_ _ _ ) =
	MTJSExpr type_ equiv
typeOfMetaObject (MOJSExprConvertEquiv newEquiv content) = case typeOfMetaObject content of
	MTJSExpr type_ _ -> MTJSExpr type_ newEquiv

slKindOfMetaObject :: MetaObject -> SLKind
slKindOfMetaObject mo = case typeOfMetaObject mo of
	MTSLType k -> k
	_ -> error "slKindOfMetaObject: type is not MTSLType"

slTypeOfMetaObject :: MetaObject -> MetaObject
slTypeOfMetaObject mo = case typeOfMetaObject mo of
	MTSLTerm t -> t
	_ -> error "slTypeOfMetaObject: type is not MTSLTerm"

