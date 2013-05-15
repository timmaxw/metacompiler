module Metacompiler.TLRuntime.TypeOf where

import qualified Data.Map as M
import qualified Metacompiler.SLRuntime.TypeOf as SLR
import Metacompiler.TLRuntime.Reduce
import Metacompiler.TLRuntime.Substitute
import Metacompiler.TLRuntime.Types

typeOfMetaObject :: MetaObject -> MetaType
typeOfMetaObject (MOApp fun arg) =
	case typeOfMetaObject fun of
		MTFun (paramName, _) bodyType -> reduceMetaType $
			substituteMetaType (Substitutions (M.singleton paramName arg) M.empty M.empty) bodyType
		_ -> error "badly typed meta-object: MOApp of non-function"
typeOfMetaObject (MOAbs (paramName, paramType) body) =
	MTFun (paramName, paramType) (typeOfMetaObject body)
typeOfMetaObject (MOName _ type_) =
	type_
typeOfMetaObject (MOSLType type_ _) =
	MTSLType (SLR.kindOfSLType type_)
typeOfMetaObject (MOSLTerm term subs) =
	MTSLTerm (reduceMetaObject (MOSLType (SLR.typeOfSLTerm term) subs))
typeOfMetaObject (MOJSExprTypeDefn defn params) =
	MTJSExprType (slEquivOfJSExprTypeDefn defn params)
typeOfMetaObject (MOJSExprLiteral equiv type_ _ _ ) =
	MTJSExpr type_ equiv
typeOfMetaObject (MOJSExprConvertEquiv newEquiv content) = case typeOfMetaObject content of
	MTJSExpr type_ _ -> MTJSExpr type_ newEquiv
	_ -> error "badly typed meta-object"

