module Metacompiler.TLRuntime where

import Control.Monad (liftM)
import qualified Data.Map as M
import qualified Language.ECMAScript3.Syntax as JS
import qualified Language.ECMAScript3.Syntax.Annotations as JS
import Metacompiler.GenSym
import qualified Metacompiler.SLSyntax as SL
import Metacompiler.TLSyntax

-- A `NormedMetaType' is the result of de-sugaring a type and evaluating any
-- variables in the types of `js-term ...` meta-types.

data NormedMetaType

	= NMTFun NormedMetaType NormedMetaType

	| NMTJSType

	-- This is `Nothing` if the term comes from a bare string, and `Just ...`
	-- where `...` is the type of the term otherwise. This `JSType` is the only
	-- is the only context where a `JSTypePlaceholder` should ever occur.
	| NMTJSTerm (Maybe JSType)

	deriving (Show, Eq)

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
			return res
		_ -> Left ("in application at " ++ formatRange tag ++ ", the thing to \
			\be called has type " ++ formatNormedMetaType funType ++ ", which \
			\is not the type of a function.")

computeMetaType vars (MOAbs tag params result) = do
	let
		processParams :: M.Map String NormedMetaType
		              -> [(String, MetaType Range)]
		              -> Either String (M.Map String NormedMetaType, NormedMetaType)
		processParams vars' [] = do
			resultType <-
				errorContext ("in return value of function at " ++
					formatRange tag)
				computeMetaType vars' result
			return (vars', resultType)
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

			vars'' <- checkedInsert paramName (paramType', placeholderValue) vars'
			resultType <- processParams vars'' params'
			return (NMTFun paramType' resultType)

	processParams vars params

computeMetaType vars (MOVar tag name) = case M.lookup name vars of
	Just (type_, _) -> return type_
	Nothing -> Left ("variable \"" ++ name ++ \" is not in scope")

computeMetaType vars (MOJSExpr tag type_ spec impl) = do
	shouldBeJSType <-
		errorContext ("in \"type\" clause of \"js-expr\" at " ++
			formatRange tag) $
		computeMetaType vars type_
	unless (shouldBeJSType == NMTJSType) $
		Left ("in \"js-expr\" at " ++ formatRange tag ++ ", expected the type \
			\to have meta-type " ++ formatNormedMetaType NMTJSType ++ ", but \
			\it had meta-type " ++ formatNormedMetaType shouldBeJSType)
	actualType <- reduce (M.map snd vars) type_
	sequence [do
		shouldBeJSTerm <- computeMetaType vars value
		case shouldBeJSTerm of
			NMTJSTerm _ -> ()
			_ -> Left ("in \"impl\" clause of \"js-expr\" at " ++
				formatRange tag ++ ", in \"(set " ++ show name ++ " ...)\" \
				\clause value should have meta-type " ++
				formatNormedMetaType NMTJSTerm ++ ", but it had meta-type " ++
				formatNormedMetaType shouldBeJSTerm)
		| (name, JBVSet value) <- varsOfJavascriptBlock impl]
	-- TODO: Check validity of spec clause.
	return (NMTJSType (Just actualType))

computeMetaType vars (MOJSSubstitution _ _) =
	return (NMTJSType Nothing)

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
		jsEquivalentOfJSTerm :: GenSym (JS.Expression ())
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

expandJavascriptBlock :: M.Map String ReducedMetaObject -> JavascriptBlock a -> GenSym (JS.Expression ())
expandJavascriptBlock vars (JavascriptBlock { codeOfJavascriptBlock = code, varsOfJavascriptBlock = substitutions }) = do
	substitutions' <- liftM safeFromList $ sequence [do
		RMOJSTerm jsEquivalent <- reduce vars value
		js <- jsEquivalent
		js' <- JSUtils.swapFreeVariables js
		return (name, js')
		| (name, value) <- substitutions]
	code' <- JSUtils.substituteVariables substitutions code
	code'' <- JSUtils.swapFreeVariables code'
	return code''

-- `processDirective` processes a single TL directive. As input, it takes the
-- map of globally defined meta-objects before the directive. As output, it
-- returns the map of globally defined meta-objects after the directive.

processDirective :: M.Map String ReducedMetaObject -> Directive a -> Either String (M.Map String ReducedMetaObject)
processDirective vars directive@(DLet { }) = do
	type_ <- computeMetaType (valueOfDirective directive)
	let
		applyParams vars' [] =
			reduce vars' M.empty (valueOfDirective directive)
		applyParams vars' ((n, t):rest) = return $ RMOFun $ \ value ->
			applyParams (safeInsert n value vars') rest 
	let newValue = applyParams vars (paramsOfDirective directive)
	return $ safeInsert (nameOfDirective directive) newValue vars
processDirective vars directive@(DJSRepr { }) =
	undefined

