module Metacompiler.TLRuntime where

import Control.Monad (liftM, unless)
import qualified Data.Map as M
import qualified Language.ECMAScript3.Syntax as JS
import qualified Language.ECMAScript3.Syntax.Annotations as JS
import qualified Metacompiler.JSUtils as JSUtils
import Metacompiler.SExpr (Range, formatRange)
import Metacompiler.SExprToSL (errorContext)   -- TODO: Move `errorContext` into its own file
import qualified Metacompiler.SLSyntax as SL
import Metacompiler.TLSyntax

-- A `NormedMetaType' is the result of de-sugaring a type and evaluating any
-- variables in the types of `js-term ...` meta-types.

data NormedMetaType

	= NMTFun NormedMetaType String NormedMetaType

	| NMTJSType

	-- This is `Nothing` if the term comes from a bare string, and `Just ...`
	-- where `...` is the type of the term otherwise. This `JSType` is the only
	-- is the only context where a `JSTypePlaceholder` should ever occur.
	| NMTJSTerm (Maybe JSType)

	deriving (Show, Eq)

formatNormedMetaType :: NormedMetaType -> String
-- TODO: Make error messages prettier. This may have to wait until the `js-term`
-- type issues are sorted out.
formatNormedMetaType = show

tryToCastNormedMetaType :: NormedMetaType -> NormedMetaType -> Either String ()
tryToCastNormedMetaType a b
	| canCast a b = return ()
	| otherwise = Left ("expected a value of type " ++ formatNormedMetaType b ++
		", but instead got a value of type " ++ formatNormedMetaType a)
	where
		canCast :: NormedMetaType -> NormedMetaType -> Bool
		canCast (NMTFun arg1 ret1) (NMTFun arg2 ret2) =
			(arg1 == ret1) && (arg2 == ret2)
		canCast (NMTJSType) (NMTJSType) = True
		canCast (NMTJSTerm Nothing) (NMTJSTerm _) = True
		canCast (NMTJSTerm (Just type1)) (NMTJSTerm (Just type2)) =
			type1 == type2
		canCast _ _ = False

computeMetaType :: M.Map String (NormedMetaType, ReducedMetaObject) -> MetaObject Range -> Either String NormedMetaType

computeMetaType vars (MOApp tag fun arg) = do
	funType <-
		errorContext ("in function of application at " ++ formatRange tag) $
		computeMetaType vars fun
	argType <-
		errorContext ("in argument of application at " ++ formatRange tag) $
		computeMetaType vars arg
	case funType of
		NMTFun argType' retType -> do
			errorContext ("for argument of application at " ++
					formatRange tag) $
				tryToCastNormedMetaType argType argType'
			return retType
		_ -> Left ("in application at " ++ formatRange tag ++ ", the thing to \
			\be called has type " ++ formatNormedMetaType funType ++ ", which \
			\is not the type of a function.")

computeMetaType vars (MOAbs tag params result) = do
	let
		processParams :: M.Map String (NormedMetaType, ReducedMetaObject)
		              -> [(String, MetaType Range)]
		              -> Either String NormedMetaType
		processParams vars' [] = do
			resultType <-
				errorContext ("in return value of function at " ++
					formatRange tag) $
				computeMetaType vars' result
			return resultType
		processParams vars' ((paramName, paramType):params') = do
			paramType' <-
				errorContext ("in type of parameter \"" ++ paramName ++ "\" \
					\to function at " ++ formatRange tag) $
				normMetaType vars' paramType

			let placeholderValue = case paramType' of
				NMTJSType -> RMOJSType (JSTypePlaceholder paramName)
				-- `placeholderValue` is not strictly evaluated; this error
				-- message will sit harmlessly in the map unless something
				-- tries to use the value.
				_ -> error "dependent types should only ever depend on JS types"

			let vars'' = M.insert paramName (paramType', placeholderValue) vars'
			resultType <- processParams vars'' params'
			return (NMTFun paramType' resultType)

	ty <- processParams vars params
	return ty

computeMetaType vars (MOVar tag name) = case M.lookup name vars of
	Just (type_, _) -> return type_
	Nothing -> Left ("variable \"" ++ name ++ "\" is not in scope")

computeMetaType vars (MOJSExpr tag type_ spec impl) = do
	shouldBeJSType <-
		errorContext ("in \"type\" clause of \"js-expr\" at " ++
			formatRange tag) $
		computeMetaType vars type_
	unless (shouldBeJSType == NMTJSType) $
		Left ("in \"js-expr\" at " ++ formatRange tag ++ ", expected the type \
			\to have meta-type (js-type), but it had meta-type " ++
			formatNormedMetaType shouldBeJSType)
	let actualType = case reduce (M.map snd vars) type_ of
		RMOJSType ty -> ty
		_ -> error ("expected RMOJSType; type checker is broken")
	sequence [do
		shouldBeJSTerm <- computeMetaType vars value
		case shouldBeJSTerm of
			NMTJSTerm _ -> return ()
			_ -> Left ("in \"impl\" clause of \"js-expr\" at " ++
				formatRange tag ++ ", in \"(set " ++ show name ++ " ...)\" \
				\clause value should have meta-type (js-term ...), but it had \
				\meta-type " ++ formatNormedMetaType shouldBeJSTerm)
		| (name, value) <- varsOfJavascriptBlock impl]
	-- TODO: Check validity of spec clause.
	return (NMTJSTerm (Just actualType))

computeMetaType vars (MOJSSubstitution _ _) =
	return (NMTJSTerm Nothing)

normMetaType :: M.Map String (NormedMetaType, ReducedMetaObject) ->  MetaType Range -> Either String NormedMetaType

normMetaType vars (MTJSType tag) = return NMTJSType

normMetaType vars (MTJSTerm tag ty) = do
	shouldBeJSType <- computeMetaType vars ty
	unless (shouldBeJSType == NMTJSType) $
		Left ("in \"js-term\" at " ++ formatRange tag ++ ", expected the type \
			\to have meta-type (js-type), but it had meta-type " ++
			formatNormedMetaType shouldBeJSType)
	case reduce (M.map snd vars) ty of
		RMOJSType ty' -> return (NMTJSTerm (Just ty'))
		_ -> error "should have been a RMOJSType"

normMetaType vars (MTFun tag params ret) = do
	let
		processParams :: M.Map String (NormedMetaType, ReducedMetaObject)
		              -> [(String, MetaType Range)]
		              -> Either String NormedMetaType
		processParams vars' [] = do
			resultType <-
				errorContext ("in return type of function type at " ++
					formatRange tag) $
				normMetaType vars' ret
			return resultType
		processParams vars' ((paramName, paramType):params') = do
			paramType' <-
				errorContext ("in type of parameter \"" ++ paramName ++ "\" \
					\to function type at " ++ formatRange tag) $
				normMetaType vars' paramType

			let placeholderValue = case paramType' of
				NMTJSType -> RMOJSType (JSTypePlaceholder paramName)
				-- `placeholderValue` is not strictly evaluated; this error
				-- message will sit harmlessly in the map unless something
				-- tries to use the value.
				_ -> error "dependent types should only ever depend on JS types"

			let vars'' = safeInsert paramName (paramType', placeholderValue) vars'
			resultType <- processParams vars'' params'
			return (NMTFun paramType' resultType)

	processParams vars params

-- A `JSType` is a Javascript representation of an SL type.

data JSType

	-- `JSType`s can only come from `js-repr` directives. `nameOfJSType` is the
	-- name of the original directive that produced this JS type.
	-- `paramsOfJSType` are what the parameters came out to.
	= JSType {
		nameOfJSType :: String,
		paramsOfJSType :: [JSType]
	}

	-- This is used for dependent type checking.
	| JSTypePlaceholder String

	deriving (Show, Eq)

-- A `ReducedMetaObject` is the result of resolving variable references and
-- function applications in a `MetaObject` until no further reductions can be
-- performed.

data ReducedMetaObject

	-- If the original `MetaObject`'s meta-type was `fun ...`, then the result
	-- of reduction will be `RMOFun`. Unlike `MOFun`, `RMOFun` is curried.
	= RMOFun (ReducedMetaObject -> ReducedMetaObject)

	-- If the original `MetaObject`'s meta-type was `type`, then the result of
	-- reduction will be `RMOJSType`.
	| RMOJSType JSType

	-- If the original `MetaObject`'s meta-type was `term ...`, then it must
	-- eventually resolve to a `js-expr` or substitution term.
	-- `jsEquivalentOfJSTerm` is the Javascript equivalent of that term.
	| RMOJSTerm {
		jsEquivalentOfJSTerm :: JSUtils.RenameSymbols (JS.Expression ())
	}

-- `reduce` reduces a `MetaObject` to a `ReducedMetaObject`. Because recursion
-- is not allowed in TL, it should always terminate.

reduce :: M.Map String ReducedMetaObject -> MetaObject a -> ReducedMetaObject
reduce vars (MOApp { funOfMetaObject = f, argOfMetaObject = a }) = let
	RMOFun f' = reduce vars f
	a' = reduce vars a
	in f' a'
reduce vars (MOAbs { paramsOfMetaObject = params, resultOfMetaObject = result }) = build vars params
	where
		build vars [] =
			reduce vars result
		build vars ((paramName, paramType):params) =
			RMOFun (\paramValue -> build (M.insert paramName paramValue vars) params)
reduce vars (MOVar { varOfMetaObject = var }) = case M.lookup var vars of
	Just value -> value
	Nothing -> error ("var not in scope: " ++ show var)
reduce vars (MOJSExpr { specOfMetaObject = s, implOfMetaObject = b }) =
	RMOJSTerm {
		jsEquivalentOfJSTerm = expandJavascriptBlock vars b
	}
reduce vars (MOJSSubstitution { jsSubstitutionOfMetaObject = x }) =
	RMOJSTerm {
		jsEquivalentOfJSTerm = return (JS.removeAnnotations x)
	}

-- `safeUnion`, `safeFromList`, and `safeInsert` are like `M.union`,
-- `M.fromList`, and `M.insert` except that if there is a duplicate value, they
-- call `error` instead of silently overwriting it.

safeUnion :: (Ord k, Show k) => M.Map k v -> M.Map k v -> M.Map k v
safeUnion = M.unionWithKey (\k -> error ("redefinition of " ++ show k))

safeFromList :: (Ord k, Show k) => [(k, v)] -> M.Map k v
safeFromList = foldr (uncurry safeInsert) M.empty

safeInsert :: (Ord k, Show k) => k -> v -> M.Map k v -> M.Map k v
safeInsert k v m = case M.lookup k m of
	Just _ -> error ("redefinition of " ++ show k)
	Nothing -> M.insert k v m

-- `expandJavascriptBlock` performs the variable substitutions in the given
-- `JavascriptBlock` and returns the result as a `JS.Expression`.

expandJavascriptBlock :: M.Map String ReducedMetaObject -> JavascriptBlock a -> JSUtils.RenameSymbols (JS.Expression ())
expandJavascriptBlock vars (JavascriptBlock { codeOfJavascriptBlock = code, varsOfJavascriptBlock = substitutions }) = do
	substitutions' <- liftM safeFromList $ sequence [do
		let RMOJSTerm jsEquivalent = reduce vars value
		js <- jsEquivalent
		js' <- JSUtils.revariableExpression M.empty js
		return (name, js')
		| (name, value) <- substitutions]
	JSUtils.revariableExpression substitutions' (JS.removeAnnotations code)


