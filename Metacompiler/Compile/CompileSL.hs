module Metacompiler.Compile.CompileSL where

import Control.Monad (when, unless, liftM)
import qualified Data.Map as M
import Metacompiler.Range
import qualified Metacompiler.Runtime as R
import qualified Metacompiler.SL.Syntax as SL

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
	typesInScope :: M.Map SL.NameOfType TypeInScope,
	ctorsInScope :: M.Map SL.NameOfCtor R.SLCtorDefn,
	termsInScope :: M.Map SL.NameOfTerm TermInScope
	}

compileSLKind :: SL.Kind Range -> Either String R.SLKind
compileSLKind (SL.KindType _) =
	return R.SLKindType
compileSLKind (SL.KindFun _ params res) = do
	params' <- mapM compileSLKind params
	res' <- compileSLKind res
	return (foldr R.SLKindFun res' params')

compileSLType :: M.Map SL.NameOfType TypeInScope
              -> [R.MetaObject]
              -> SL.Type Range
              -> Either String R.MetaObject
compileSLType typeScope typeParams (SL.TypeName range name) =
	case M.lookup name typeScope of
		Just typeInScope -> do
			when (length typeParams < length (typeParamsOfTypeInScope typeInScope)) $
				Left ("at " ++ formatRange range ++ ": substitution `" ++ SL.unNameOfType name ++ "` takes " ++
					show (length (typeParamsOfTypeInScope typeInScope)) ++ " parameters, but only " ++
					show (length typeParams) ++ " were provided.")
			let typeParamsToSub = take (length (typeParamsOfTypeInScope typeInScope)) typeParams
			let typeParamsNotToSub = drop (length (typeParamsOfTypeInScope typeInScope)) typeParams
			sequence [do
				let actualKind = R.slKindOfMetaObject type_
				unless (actualKind == expectedKind) $
					Left ("at " ++ formatRange range ++ ": parameter #" ++ show i ++ " to substitution `" ++
						SL.unNameOfType name ++ "` should have kind " ++ formatKind expectedKind ++ ", but actually \
						\has kind " ++ formatKind actualKind)
				| (type_, expectedKind, i) <- zip3 typeParamsToSub (typeParamsOfTypeInScope typeInScope) [1..]]
			let value = valueOfTypeInScope typeInScope typeParamsToSub
			compileSLTypeApps value typeParamsNotToSub
		Nothing -> Left ("at " ++ formatRange range ++ ": type name `" ++ SL.unNameOfType name ++ "` is not in scope")
compileSLType typeScope typeParams (SL.TypeApp range fun arg) = do
	arg' <- compileSLType typeScope [] arg
	compileSLType typeScope (arg':typeParams) fun
compileSLType typeScope typeParams other | not (null typeParams) = do
	other' <- compileSLType typeScope [] other
	compileSLTypeApps other' typeParams
compileSLType typeScope [] (SL.TypeFun range params res) = do
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
compileSLType typeScope [] (SL.TypeLazy range x) = do
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
              -> SL.Term Range
              -> Either String R.MetaObject
compileSLTerm scope termParams (SL.TermName range name typeParams) = do
	case M.lookup name (termsInScope scope) of
		Just termInScope -> do
			typeParams' <- mapM (compileSLType (typesInScope scope) []) typeParams
			when (length typeParams /= length (typeParamsOfTermInScope termInScope)) $
				Left ("at " ++ formatRange range ++ ": name `" ++ SL.unNameOfTerm name ++ "` takes " ++
					show (length (typeParamsOfTermInScope termInScope)) ++ " type parameters, but " ++
					show (length typeParams) ++ " were provided.")
			when (length termParams < length (termParamsOfTermInScope termInScope)) $
				Left ("at " ++ formatRange range ++ ": name `" ++ SL.unNameOfTerm name ++ "` takes " ++
					show (length (typeParamsOfTermInScope termInScope)) ++ " term parameters, but only " ++
					show (length typeParams) ++ " were provided.")
			let termParamsToSub = take (length (termParamsOfTermInScope termInScope)) termParams
			let termParamsNotToSub = drop (length (termParamsOfTermInScope termInScope)) termParams
			sequence [do
				let actualKind = R.slKindOfMetaObject type_
				unless (actualKind == expectedKind) $
					Left ("at " ++ formatRange range ++ ": type parameter #" ++ show i ++ " to name `" ++
						SL.unNameOfTerm name ++ "` should have kind " ++ formatKind expectedKind ++ ", but actually \
						\has kind " ++ formatKind actualKind)
				| (type_, expectedKind, i) <- zip3 typeParams' (typeParamsOfTermInScope termInScope) [1..]]
			sequence [do
				let actualType = R.slTypeOfMetaObject arg
				unless (actualType `R.equivalentMetaObjects` expectedType) $
					Left ("at " ++ formatRange range ++ ": term parameter #" ++ show i ++ " to name `" ++
						SL.unNameOfTerm name ++ "` has wrong type")
				| (arg, expectedType, i) <- zip3 termParamsToSub (termParamsOfTermInScope termInScope) [1..]]
			let value = valueOfTermInScope termInScope typeParams' termParamsToSub
			compileSLTermApps value termParamsNotToSub
		Nothing -> Left ("at " ++ formatRange range ++ ": term name `" ++ SL.unNameOfTerm name ++ "` is not in scope")
compileSLTerm scope termParams (SL.TermApp range fun arg) = do
	arg' <- compileSLTerm scope [] arg
	compileSLTerm scope (arg':termParams) fun
compileSLTerm scope termParams other | not (null termParams) = do
	other' <- compileSLTerm scope [] other
	compileSLTermApps other' termParams
compileSLTerm scope [] (SL.TermAbs range params body) = f (termsInScope scope) params
	where
		f :: M.Map SL.NameOfTerm TermInScope -> [(SL.NameOfTerm, SL.Type Range)] -> Either String R.MetaObject
		f termScope' [] = compileSLTerm (scope { termsInScope = termScope' }) [] body
		f termScope' ((paramName, paramType):paramsLeft) = do
			let paramName' = R.NameOfSLTerm (SL.unNameOfTerm paramName)
			paramType' <- compileSLType (typesInScope scope) [] paramType
			let termScope'' = M.insert paramName (TermInScope [] [] (\ _ _ -> R.MOSLTermName paramName' paramType')) termScope'
			body' <- f termScope'' paramsLeft
			return (R.MOSLTermAbs (paramName', paramType') body')
compileSLTerm scope [] (SL.TermCase range subject clauses) = do
	subject' <- compileSLTerm scope [] subject
	clauses' <- sequence [do
		ctor <- case M.lookup ctorName (ctorsInScope scope) of
			Just ctor -> return ctor
			Nothing -> Left ("constructor `" ++ SL.unNameOfCtor ctorName ++ "` is not in scope")
		ctorTypeArgs' <- mapM (compileSLType (typesInScope scope) []) ctorTypeArgs
		unless (length ctorTypeArgs' == length (R.typeParamsOfSLDataDefn (R.parentDataOfSLCtorDefn ctor))) $
			Left ("constructor `" ++ SL.unNameOfCtor ctorName ++ "` expects " ++
				show (length (R.typeParamsOfSLDataDefn (R.parentDataOfSLCtorDefn ctor))) ++ " type parameters, but instead it got " ++
				show (length ctorTypeArgs') ++ ".")
		sequence [do
			let actualKind = R.slKindOfMetaObject type_
			unless (actualKind == expectedKind) $
				Left ("at " ++ formatRange range ++ ": type parameter #" ++ show i ++ " to constructor `" ++
					SL.unNameOfCtor ctorName ++ "` should have kind " ++ formatKind expectedKind ++ ", but actually \
					\has kind " ++ formatKind actualKind)
			| (type_, expectedKind, i) <- zip3 ctorTypeArgs' (R.typeParamsOfSLDataDefn (R.parentDataOfSLCtorDefn ctor)) [1..]]
		let fieldTypes = map ($ ctorTypeArgs') (R.fieldTypesOfSLCtorDefn ctor)
		let fieldNames' = map (R.NameOfSLTerm . SL.unNameOfTerm) fieldNames
		let termScope' = foldr
			(\ (name, name', type_) -> M.insert name (TermInScope [] [] (\ _ _ -> R.MOSLTermName name' type_)))
			(termsInScope scope)
			(zip3 fieldNames fieldNames' fieldTypes)
		body' <- compileSLTerm (scope { termsInScope = termScope' }) [] body
		return (ctor, ctorTypeArgs', fieldNames', body')
		| (ctorName, ctorTypeArgs, fieldNames, body) <- clauses]
	return (R.MOSLTermCase subject' clauses')
compileSLTerm scope [] (SL.TermWrap _ x) = do
	x' <- compileSLTerm scope [] x
	return (R.MOSLTermWrap x')
compileSLTerm scope [] (SL.TermUnwrap _ x) = do
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
	dataDefns :: M.Map SL.NameOfType R.SLDataDefn,
	ctorDefns :: M.Map SL.NameOfCtor R.SLCtorDefn,
	termDefns :: M.Map SL.NameOfTerm R.SLTermDefn
	}

typeInScopeForDataDefn :: R.SLDataDefn -> TypeInScope
typeInScopeForDataDefn defn =
	TypeInScope
		[]
		(\ [] -> R.MOSLTypeDefn defn)

termInScopeForCtorDefn :: R.SLCtorDefn -> TermInScope
termInScopeForCtorDefn defn =
	TermInScope
		(R.typeParamsOfSLDataDefn (R.parentDataOfSLCtorDefn defn))
		[]
		(\ typeParams [] -> let
			fieldTypes = map ($ typeParams) (R.fieldTypesOfSLCtorDefn defn)
			fieldNames = take (length fieldTypes) [R.NameOfSLTerm ("f" ++ show i) | i <- [1..]]
			body = R.MOSLTermData defn typeParams [R.MOSLTermName n t | (n, t) <- zip fieldNames fieldTypes]
			fun = foldr R.MOSLTermAbs body (zip fieldNames fieldTypes)
			in fun
			)

termInScopeForTermDefn :: R.SLTermDefn -> TermInScope
termInScopeForTermDefn defn = 
	TermInScope
		(R.typeParamsOfSLTermDefn defn)
		[]
		(\ typeParams [] -> R.MOSLTermDefn defn typeParams)

scopeForDefns :: Defns -> Scope
scopeForDefns defns = Scope {
	typesInScope = M.map typeInScopeForDataDefn (dataDefns defns),
	ctorsInScope = ctorDefns defns,
	termsInScope = M.map termInScopeForTermDefn (termDefns defns)
		`M.union` M.map termInScopeForCtorDefn (M.mapKeysMonotonic (SL.NameOfTerm . SL.unNameOfCtor) (ctorDefns defns))
	}

compileSLDirectives :: [SL.Dir Range] -> Either String Defns
compileSLDirectives directives = do
	-- TODO: Detect name conflicts: type/type, term/term, ctor/ctor, term/ctor

	dataDefnsAndCtorDefnPromises <- sequence [do

		paramKinds' <- sequence [compileSLKind kind | (_, kind) <- params]

		let dataDefn = R.SLDataDefn {
			R.nameOfSLDataDefn = R.NameOfSLType (SL.unNameOfType typeName),
			R.typeParamsOfSLDataDefn = paramKinds'
			}

		let
			ctorPromise :: M.Map SL.NameOfType R.SLDataDefn -> Either String (M.Map SL.NameOfCtor R.SLCtorDefn)
			ctorPromise dataDefns = do
				let runNameForParamName = R.NameOfSLType . SL.unNameOfType
				let typeScope = M.fromList [let
					metaObject = R.MOSLTypeName (runNameForParamName name) paramKind'
					in (name, TypeInScope [] (const metaObject))
					| ((name, _), paramKind') <- zip params paramKinds']
					`M.union` M.map typeInScopeForDataDefn dataDefns
				liftM M.fromList $ sequence [do
					fieldTypes' <- mapM (compileSLType typeScope []) fieldTypes
					let ctorDefn = R.SLCtorDefn {
						R.nameOfSLCtorDefn = R.NameOfSLCtor (SL.unNameOfCtor ctorName),
						R.parentDataOfSLCtorDefn = dataDefn,
						R.fieldTypesOfSLCtorDefn = [\ paramValues -> let
							typeSubs = M.fromList (zip [runNameForParamName n | (n, _) <- params] paramValues)
							in R.substituteMetaObject (R.Substitutions M.empty typeSubs M.empty) fieldType'
							| fieldType' <- fieldTypes']
						}
					return (ctorName, ctorDefn)
					| (ctorName, fieldTypes) <- ctors]

		return (typeName, dataDefn, ctorPromise)
		| SL.DirData _ typeName params ctors <- directives]

	let dataDefns = M.fromList [(name, defn) | (name, defn, _) <- dataDefnsAndCtorDefnPromises]
	ctorDefns <- liftM M.unions $ sequence [ctorPromise dataDefns | (_, _, ctorPromise) <- dataDefnsAndCtorDefnPromises]

	-- termPromises :: M.Map SL.NameOfTerm (M.Map SL.NameOfTerm R.SLTermDefn -> (R.SLTermDefn, Either String ()))
	termPromises <- liftM M.fromList $ sequence [do

		typeParamKinds' <- sequence [compileSLKind kind | (_, kind) <- typeParams]
		let runNameForTypeParamName = R.NameOfSLType . SL.unNameOfType

		let typeScope = M.fromList [let
			metaObject = R.MOSLTypeName (runNameForTypeParamName name) typeParamKind'
			in (name, TypeInScope [] (const metaObject))
			| ((name, _), typeParamKind') <- zip typeParams typeParamKinds']
			`M.union` M.map typeInScopeForDataDefn dataDefns

		termParamTypes' <- sequence [compileSLType typeScope [] paramType | (_, paramType) <- termParams]
		let runNameForTermParamName = R.NameOfSLTerm . SL.unNameOfTerm

		type_' <- compileSLType typeScope [] type_

		let
			promise :: M.Map SL.NameOfTerm R.SLTermDefn -> (R.SLTermDefn, Either String ())
			promise termDefns = let

				ctorScope = ctorDefns

				termScope = M.fromList [let
					metaObject = R.MOSLTermName (runNameForTermParamName name) termParamType'
					in (name, TermInScope [] [] (const (const metaObject)))
					| ((name, _), termParamType') <- zip termParams termParamTypes']
					`M.union` M.map termInScopeForTermDefn termDefns

				valueOrError = compileSLTerm (Scope typeScope ctorScope termScope) [] value

				makeSubbed object typeParamValues =
					R.substituteMetaObject (R.Substitutions M.empty typeSubs M.empty) object
					where typeSubs = M.fromList (zip [runNameForTypeParamName n | (n, _) <- typeParams] typeParamValues)

				termDefn = R.SLTermDefn {
					R.nameOfSLTermDefn = R.NameOfSLTerm (SL.unNameOfTerm termName),
					R.typeParamsOfSLTermDefn = typeParamKinds',
					R.typeOfSLTermDefn = makeSubbed (foldl R.MOSLTypeFun type_' termParamTypes'),
					R.valueOfSLTermDefn = makeSubbed (case valueOrError of Right value -> value)
					}

				in (termDefn, valueOrError >> return ())

		return (termName, promise)

		| SL.DirLet _ termName typeParams termParams type_ value <- directives]

	let
		termDefns :: M.Map SL.NameOfTerm R.SLTermDefn
		termDefns = M.map (fst . ($ termDefns)) termPromises
	sequence [snd (promise termDefns) | promise <- M.elems termPromises]

	return $ Defns dataDefns ctorDefns termDefns

