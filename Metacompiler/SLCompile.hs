module Metacompiler.SLCompile where

import Control.Monad (when, unless, liftM)
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
	ctorsInScope :: M.Map SLS.NameOfCtor R.SLCtorDefn,
	termsInScope :: M.Map SLS.NameOfTerm TermInScope
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
		ctorTypeArgs' <- mapM (compileSLType (typesInScope scope) []) ctorTypeArgs
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

data Defns = Defns {
	dataDefns :: M.Map SLS.NameOfType R.SLDataDefn,
	ctorDefns :: M.Map SLS.NameOfCtor R.SLCtorDefn,
	termDefns :: M.Map SLS.NameOfTerm R.SLTermDefn
	}

typeInScopeForDataDefn :: R.SLDataDefn -> TypeInScope
typeInScopeForDataDefn defn =
	TypeInScope [] (\ [] -> R.MOSLDataDefn defn)

termInScopeForTermDefn :: R.SLTermDefn -> TermInScope
termInScopeForTermDefn defn = 
	TermInScope (R.typeParamsOfSLTermDefn defn) [] (\ typeParams [] -> R.MOSLTermDefn defn typeParams)

scopeForDefns :: Defns -> Scope
scopeForDefns defns = Scope {
	typesInScope = M.map typeInScopeForDataDefn (dataDefns defns),
	ctorsInScope = ctorDefns,
	termsInScope = M.map termInScopeForTermDefn (termDefns defns)
	}

compileSLDirectives :: [SLS.Dir Range] -> Either String Defns
compileSLDirectives directives = do
	-- TODO: Detect name conflicts

	dataDefnsAndCtorDefnPromises <- sequence [do

		paramKinds' <- sequence [compileSLKind kind | (_, kind) <- params]

		let dataDefn = R.SLDataDefn {
			R.nameOfSLDataDefn = R.NameOfSLType (SLS.unNameOfType typeName),
			R.typeParamsOfSLDataDefn = paramKinds'
			}

		let
			ctorPromise :: M.Map SLS.NameOfType R.SLDataDefn -> Either String (M.Map SLS.NameOfCtor R.SLCtorDefn)
			ctorPromise dataDefns = do
				let runNameForParamName = R.NameOfSLType . SLS.unNameOfType
				let typeScope = M.fromList [let
					metaObject = R.MOSLTypeName (runNameForParamName name) paramKind'
					in (name, TypeInScope [] (const metaObject))
					| ((name, _), paramKind') <- zip params paramKinds']
					`M.union` M.map typeInScopeForDataDefn dataDefns
				liftM M.fromList $ sequence [do
					fieldTypes' <- mapM (compileSLType typeScope []) fieldTypes
					let ctorDefn = R.SLCtorDefn {
						R.nameOfSLCtorDefn = R.NameOfSLCtor (SLS.unNameOfCtor ctorName),
						R.parentDataOfSLCtorDefn = dataDefn,
						R.fieldTypesOfSLCtorDefn = [\ paramValues -> let
							typeSubs = M.fromList (zip [runNameForParamName n | (n, _) <- params] paramValues)
							in R.substituteMetaObject (R.Substitutions M.empty typeSubs M.empty) fieldType'
							| fieldType' <- fieldTypes']
						}
					return (ctorName, ctorDefn)
					| (ctorName, fieldTypes) <- ctors]

		return (typeName, dataDefn, ctorPromise)
		| SLS.DirData _ typeName params ctors <- directives]

	let dataDefns = M.fromList [(name, defn) | (name, defn, _) <- dataDefnsAndCtorPromises]
	ctorDefns <- liftM M.unions $ sequence [ctorPromise dataDefns | (_, _, ctorPromise) <- dataDefnsAndCtorPromises]

	-- termPromises :: M.Map SLS.NameOfTerm (M.Map SLS.NameOfTerm R.SLTermDefn -> (R.SLTermDefn, Either String ()))
	termPromises <- liftM M.fromList $ sequence [do

		typeParamKinds' <- sequence [compileSLKind kind | (_, kind) <- typeParams]
		let runNameForTypeParamName = R.NameOfSLType . SLS.unNameOfType

		let typeScope = M.fromList [let
			metaObject = R.MOSLTypeName (runNameForTypeParamName name) typeParamKind'
			in (name, TypeInScope [] (const metaObject))
			| ((name, _), typeParamKind') <- zip typeParams typeParamKinds']
			`M.union` M.map typeInScopeForDataDefn dataDefns

		termParamTypes' <- sequence [compileSLType typeScope [] paramType | (_, paramType) <- termParams]
		let runNameForTermParamName = R.NameOfSLTerm . SLS.unNameOfTerm

		type_' <- compileSLType typeScope [] type_

		return $ \termDefns -> let

			ctorScope = ctorDefns

			termScope = M.fromList [let
				metaObject = R.MOSLTermName (runNameForTermParamName name) kind'
				in (name, TermInScope [] [] (const (const metaObject)))
				| ((name, _), termParamKind') <- zip termParams termParamTypes']
				`M.union` M.map termInScopeForTermDefn termDefns

			valueOrError = compileSLTerm (Scope typeScope ctorScope termScope) [] value

			makeSubbed object typeParamValues =
				R.substituteMetaObject (R.Substitutions M.empty typeSubs M.empty) object
				where typeSubs = M.fromList (zip [runNameForTypeParamName n | (n, _) <- typeParams] typeParamValues)

			termDefn = R.SLTermDefn {
				R.nameOfSLTermDefn = R.NameOfSLTerm (SLS.unNameOfTerm termName),
				R.typeParamsOfSLTermDefn = typeParamKinds',
				R.typeOfSLTermDefn = makeSubbed (foldl R.MOSLTypeFun type_' termParamTypes'),
				R.valueOfSLTermDefn = makeSubbed (case valueOrError of Right value -> value)
				}

			in (termDefn, valueOrError >> return ())

		| SLS.DirLet _ termName typeParams termParams type_ value <- directives]

	let
		termDefns :: M.Map SLS.NameOfTerm R.SLTermDefn
		termDefns = M.map (fst . ($ termDefns)) termPromises
	sequence [snd (promise termDefns) | promise <- M.values termPromises]

	return $ Defns dataDefns ctorDefns termDefns

