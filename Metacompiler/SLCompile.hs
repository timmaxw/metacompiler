module Metacompiler.SLCompile where

import Control.Monad (when, unless)
import qualified Data.Map as M
import qualified Metacompiler.Runtime as R
import Metacompiler.SExpr (Range, formatRange)
import qualified Metacompiler.SLSyntax as SLS

formatKind :: R.SLKind -> String
formatKind _ = "<not implemented>"

data TypeInScope = TypeInScope {
	typeParamsOfTypeInScope :: [R.SLKind],
	valueOfTypeInScope :: [R.MetaObject] -> R.MetaObject
	}

data TermInScope = TermInScope {
	typeParamsOfTermInScope :: [R.SLKind],
	termParamsOfTermInScope :: [R.MetaObject],
	valueOfTermInScope :: [R.MetaObject] -> [R.MetaObject] -> R.MetaObject
	}

data Scope = Scope {
	typesInScope :: M.Map SLS.NameOfType TypeInScope,
	ctorsInScope :: M.Map SLS.NameOfCtor R.Ctor,
	termsInScope :: M.Map SLS.NameOfTerm TermInScope,
	termValuesInScope :: M.Map R.NameOfTerm R.MetaObject
	}

compileSLKind :: SLS.Kind Range -> Either String R.SLKind
compileSLKind (SLS.KindType _) =
	return R.SLKindType
compileSLKind (SLS.KindFun _ params res) = do
	params' <- mapM compileSLKind params
	res' <- compileSLKind res
	return (foldr R.SLKindFun res' params')

compileSLType :: M.Map SLS.NameOfType TypeInScope
              -> [R.MetaObject]
              -> SLS.Type Range
              -> Either String R.MetaObject
compileSLType typeScope typeParams (SLS.TypeName range name) =
	case M.lookup name typeScope of
		Just typeInScope -> do
			when (length typeParams < length (typeParamsOfTypeInScope typeInScope)) $
				Left ("at " ++ formatRange range ++ ": substitution `" ++ SLS.unNameOfType name ++ "` takes " ++
					show (length (typeParamsOfTypeInScope typeInScope)) ++ " parameters, but only " ++
					show (length typeParams) ++ " were provided.")
			let typeParamsToSub = take (length (typeParamsOfTypeInScope typeInScope)) typeParams
			let typeParamsNotToSub = drop (length (typeParamsOfTypeInScope typeInScope)) typeParams
			sequence [do
				let actualKind = R.slKindOfMetaObject type_
				unless (actualKind == expectedKind) $
					Left ("at " ++ formatRange range ++ ": parameter #" ++ show i ++ " to substitution `" ++
						SLS.unNameOfType name ++ "` should have kind " ++ formatKind expectedKind ++ ", but actually \
						\has kind " ++ formatKind actualKind)
				| (type_, expectedKind, i) <- zip3 typeParamsToSub (typeParamsOfTypeInScope typeInScope) [1..]]
			let value = valueOfTypeInScope typeInScope typeParamsToSub
			compileSLTypeApps value typeParamsNotToSub
		Nothing -> Left ("at " ++ formatRange range ++ ": type name `" ++ SLS.unNameOfType name ++ "` is not in scope")
compileSLType typeScope typeParams (SLS.TypeApp range fun arg) = do
	arg' <- compileSLType typeScope [] arg
	compileSLType typeScope (arg':typeParams) fun
compileSLType typeScope typeParams other | not (null typeParams) = do
	other' <- compileSLType typeScope [] other
	compileSLTypeApps other' typeParams
compileSLType typeScope [] (SLS.TypeFun range params res) = do
	params' <- sequence [do
		param' <- compileSLType typeScope [] param
		unless (R.slKindOfMetaObject param' == R.SLKindType) $
			Left ("at " ++ formatRange range ++ ": all parameters must have kind `*`")
		return param'
		| param <- params]
	res' <- compileSLType typeScope [] res
	unless (R.slKindOfMetaObject res' == R.SLKindType) $
		Left ("at " ++ formatRange range ++ ": return type must have kind `*`")
	return (foldr R.MOSLTypeFun res' params')
compileSLType typeScope [] (SLS.TypeLazy range x) = do
	x' <- compileSLType typeScope [] x
	unless (R.slKindOfMetaObject x' == R.SLKindType) $
		Left ("at " ++ formatRange range ++ ": inner type must have kind `*`")
	return (R.MOSLTypeLazy x')

compileSLTypeApps :: R.MetaObject -> [R.MetaObject] -> Either String R.MetaObject
compileSLTypeApps base [] = return base
compileSLTypeApps fun (arg:args) =
	case R.slKindOfMetaObject fun of
		R.SLKindType -> Left ("cannot apply type of kind `*` like a function")
		R.SLKindFun paramKind _ -> do
			unless (paramKind == R.slKindOfMetaObject arg) $
				Left ("argument kind is not what was expected")
			compileSLTypeApps (R.MOSLTypeApp fun arg) args

compileSLTerm :: Scope
              -> [R.MetaObject]
              -> SLS.Term Range
              -> Either String R.MetaObject
compileSLTerm scope termParams (SLS.TermName range name typeParams) = do
	case M.lookup name (termsInScope scope) of
		Just termInScope -> do
			typeParams' <- mapM (compileSLType (typesInScope scope) []) typeParams
			when (length typeParams /= length (typeParamsOfTermInScope termInScope)) $
				Left ("at " ++ formatRange range ++ ": name `" ++ SLS.unNameOfTerm name ++ "` takes " ++
					show (length (typeParamsOfTermInScope termInScope)) ++ " type parameters, but " ++
					show (length typeParams) ++ " were provided.")
			when (length termParams < length (termParamsOfTermInScope termInScope)) $
				Left ("at " ++ formatRange range ++ ": name `" ++ SLS.unNameOfTerm name ++ "` takes " ++
					show (length (typeParamsOfTermInScope termInScope)) ++ " term parameters, but only " ++
					show (length typeParams) ++ " were provided.")
			let termParamsToSub = take (length (termParamsOfTermInScope termInScope)) termParams
			let termParamsNotToSub = drop (length (termParamsOfTermInScope termInScope)) termParams
			sequence [do
				let actualKind = R.slKindOfMetaObject type_
				unless (actualKind == expectedKind) $
					Left ("at " ++ formatRange range ++ ": type parameter #" ++ show i ++ " to name `" ++
						SLS.unNameOfTerm name ++ "` should have kind " ++ formatKind expectedKind ++ ", but actually \
						\has kind " ++ formatKind actualKind)
				| (type_, expectedKind, i) <- zip3 typeParams' (typeParamsOfTermInScope termInScope) [1..]]
			sequence [do
				let actualType = R.slTypeOfMetaObject arg
				unless (actualType `R.equivalentMetaObjects` expectedType) $
					Left ("at " ++ formatRange range ++ ": term parameter #" ++ show i ++ " to name `" ++
						SLS.unNameOfTerm name ++ "` has wrong type")
				| (arg, expectedType, i) <- zip3 termParamsToSub (termParamsOfTermInScope termInScope) [1..]]
			let value = valueOfTermInScope termInScope typeParams' termParamsToSub
			compileSLTermApps value termParamsNotToSub
		Nothing -> Left ("at " ++ formatRange range ++ ": term name `" ++ SLS.unNameOfTerm name ++ "` is not in scope")
compileSLTerm scope termParams (SLS.TermApp range fun arg) = do
	arg' <- compileSLTerm scope [] arg
	compileSLTerm scope (arg':termParams) fun
compileSLTerm scope termParams other | not (null termParams) = do
	other' <- compileSLTerm scope [] other
	compileSLTermApps other' termParams
compileSLTerm scope [] (SLS.TermAbs range params body) = f (termsInScope scope) params
	where
		f :: M.Map SLS.NameOfTerm TermInScope -> [(SLS.NameOfTerm, SLS.Type Range)] -> Either String R.MetaObject
		f termScope' [] = compileSLTerm (scope { termsInScope = termScope' }) [] body
		f termScope' ((paramName, paramType):paramsLeft) = do
			let paramName' = R.NameOfSLTerm (SLS.unNameOfTerm paramName)
			paramType' <- compileSLType (typesInScope scope) [] paramType
			let termScope'' = M.insert paramName (TermInScope [] [] (\ _ _ -> R.MOSLTermName paramName' [] paramType')) termScope'
			body' <- f termScope'' paramsLeft
			return (R.MOSLTermAbs (paramName', paramType') body')
compileSLTerm scope [] (SLS.TermCase range subject clauses) = do
	subject' <- compileSLTerm scope [] subject
	clauses' <- sequence [do
		ctor <- case M.lookup ctorName (ctorsInScope scope) of
			Just ctor -> return ctor
			Nothing -> Left ("constructor `" ++ SLS.unNameOfCtor ctorName ++ "` is not in scope")
		ctorTypeArgs' <- mapM (compileSLType typeScope []) ctorTypeArgs
		unless (length ctorTypeArgs' == length (R.typeParamsOfSLCtor ctor)) $
			Left ("constructor `" ++ SLS.unNameOfCtor ctorName ++ "` expects " ++
				show (length (R.typeParamsOfSLCtor ctor)) ++ " type parameters, but instead it got " ++
				show (length ctorTypeArgs') ++ ".")
		sequence [do
			let actualKind = R.slKindOfMetaObject type_
			unless (actualKind == expectedKind) $
				Left ("at " ++ formatRange range ++ ": type parameter #" ++ show i ++ " to constructor `" ++
					SLS.unNameOfCtor ctorName ++ "` should have kind " ++ formatKind expectedKind ++ ", but actually \
					\has kind " ++ formatKind actualKind)
			| (type_, expectedKind, i) <- zip3 ctorTypeArgs' (R.typeParamsOfSLCtor ctor) [1..]]
		let fieldTypes = map ($ ctorTypeArgs') (R.fieldsOfSLCtor ctor)
		let fieldNames' = map (R.NameOfSLTerm . SLS.unNameOfTerm) fieldNames
		let termScope' = foldr
			(\ (name, name', type_) -> M.insert name (TermInScope [] [] (\ _ _ -> R.MOSLTermName name' [] type_)))
			(termsInScope scope)
			(zip3 fieldNames fieldNames' fieldTypes)
		body' <- compileSLTerm (scope { termsInScope = termScope' }) [] body
		return (ctor, ctorTypeArgs', fieldNames', body')
		| (ctorName, ctorTypeArgs, fieldNames, body) <- clauses]
	return (R.MOSLTermCase subject' clauses')
compileSLTerm scope [] (SLS.TermWrap _ x) = do
	x' <- compileSLTerm scope [] x
	return (R.MOSLTermWrap x')
compileSLTerm scope [] (SLS.TermUnwrap _ x) = do
	x' <- compileSLTerm scope [] x
	case R.reduceMetaObject (R.slTypeOfMetaObject x') of
		R.MOSLTypeLazy _ -> return ()
		_ -> Left ("contents of `(unwrap ...)` should have type `(lazy ...)`")
	return (R.MOSLTermUnwrap x')

compileSLTermApps :: R.MetaObject -> [R.MetaObject] -> Either String R.MetaObject
compileSLTermApps base [] = return base
compileSLTermApps fun (arg:args) =
	case R.reduceMetaObject (R.slTypeOfMetaObject fun) of
		R.MOSLTypeFun paramType _ -> do
			unless (paramType `R.equivalentMetaObjects` R.slTypeOfMetaObject arg) $
				Left ("argument type is not what was expected")
			compileSLTermApps (R.MOSLTermApp fun arg) args
		_ -> Left ("you're trying to apply something that's not a function")

compileSLGlobals :: [SLS.Dir Range]
                 -> Either String Scope
compileSLGlobals directives = do
	let scope = Scope M.empty M.empty M.empty M.empty
	-- TODO: Detect name conflicts
	newTypesAndCtorPromises <- sequence [do
		let typeName' = R.NameOfSLType (SLS.unNameOfType typeName)
		paramKinds' <- sequence [compileSLKind kind | (_, kind) <- params]
		let kind = foldr R.SLKindFun R.SLKindType paramKinds'
		let typeInScope = TypeInScope [] (const (R.MOSLTypeName typeName' kind))
		let
			ctorPromise :: M.Map SLS.NameOfType R.MetaObject -> Either String [(SLS.NameOfCtor, R.Ctor)]
			ctorPromise typeScope' = do
				let typeScope'' = foldr (uncurry M.insert) typeScope'
					[(paramName, TypeInScope [] (const (R.MOSLTypeName paramName' paramKind))
					| (paramName, paramName', paramKind') <- zip (map fst params) paramNames' paramKinds]
				sequence [
					let ctorName' = R.NameOfSLCtor (SLS.unNameOfCtor ctorName)
					fieldTypes' <- mapM (compileSLType typeScope'' []) fieldTypes
					let ctor = R.SLCtor {
						R.nameOfSLCtor = ctorName',
						R.typeParamsOfSLCtor = paramKinds,
						R.fieldsOfSLCtor = [\ paramValues -> let
							subs = R.Substitutions M.empty M.empty (M.fromList (zip paramNames' paramValues))
							in R.substituteMetaObject subs fieldType'
							| fieldType' <- fieldTypes']
						R.typeOfSLCtor = foldl R.MOSLTypeApp (R.MOSLTypeName typeName' kind)
						}
					return (ctorName, ctor)
					| (ctorName, fieldTypes) <- ctors]
		return (typeName, typeInScope, ctorPromise)
		| SLS.DirData _ typeName params ctors <- directives]
	let typeScope' = M.union
			(M.fromList [(typeName, typeInScope) | (typeName, typeInScope, _) <- newTypesAndCtorPromises])
			(typesInScope scope)
	newCtors <- liftM (M.fromList . concatMap) $
		sequence [ctorPromise typeScope | (_, _, ctorPromise) <- newTypesAndCtorPromises]
	let ctorScope' = M.union newCtors (ctorsInScope scope)
	newTermsAndValuePromises <- sequence [do
		let termName' = R.NameOfSLTerm (SLS.unNameOfTerm termName)
		let typeParamNames' = [R.NameOfSLType (SLS.unNameOfType paramName) | (paramName, _) <- typeParams]
		typeParamKinds' <- sequence [compileSLKind kind | (_, kind) <- typeParams]
		let typeScope'' = foldr (uncurry M.insert) typeScope'
			[(paramName, TypeInScope [] (const (R.MOSLTypeName paramName' paramKind))
			| (paramName, paramName', paramKind') <- zip (map fst typeParams) typeParamNames' typeParamKinds]
		let termParamNames' = [R.NameOfSLTerm (SLS.unNameOfTerm paramName) | (paramName, _) <- termParams]
		termParamTypes' <- sequence [compileSLType typeScope'' [] paramType | (_, paramType) <- termParams]
		type_' <- compileSLType typeScope'' [] type_
		let termInScope = TermInScope typeParamKinds' [] $ \typeParamValues _ -> let
				subs = R.Substitutions M.empty M.empty (M.fromList (zip typeParamNames' typeParamValues))
				in R.MOSLTermName termName' typeParamValues (R.substituteMetaObject subs type_')
		let
			valuePromise :: M.Map SLS.NameOfTerm MetaObject -> Either String MetaObject
			valuePromise termScope' = do
				let termScope'' = foldr (uncurry M.insert) termScope'
					[(paramName, TermInScope [] (const (R.MOSLTermName paramName' paramKind))
					| (paramName, paramName', paramKind') <- zip (map fst termParams) termParamNames' termParamTypes']
				compileSLTerm (Scope typeScope'' ctorScope' termScope'') [] value
		return (termName, termName', termInScope, valuePromise)
		| SLS.DirLet _ termName typeParams termParams type_ value <- directives]
	let termScope' = M.union
			(M.fromList [(termName, termInScope) | (termName, _, termInScope, _) <- newTermsAndValuePromises])
			(termsInScope scope)
	newTermValues <- liftM M.fromList $ sequence [do
		value <- valuePromise termScope'
		return (termName', value)
		| (_, termName', _, valuePromise) <- newTermsAndValuePromises]
	let termValueScope' = M.union newTermValues (termValuesInScope scope)
	return $ Scope {
		typesInScope = typeScope',
		ctorsInScope = ctorScope',
		termsInScope = termScope',
		termValuesInScope = termValueScope'
		}

