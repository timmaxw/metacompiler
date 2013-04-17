module Metacompiler.Compile.CompileSL where

import Control.Monad (when, unless, liftM)
import qualified Data.Map as M
import Metacompiler.Error
import qualified Metacompiler.Compile.FormatSL as FSL
import qualified Metacompiler.Compile.FormatTL as FTL
import qualified Metacompiler.Runtime as R
import qualified Metacompiler.SL.Syntax as SL
import qualified Metacompiler.SL.ToSExpr as SL

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
	ctorsInScope :: M.Map SL.NameOfTerm R.SLCtorDefn,
	termsInScope :: M.Map SL.NameOfTerm TermInScope
	}

formatKindForMessage :: R.SLKind -> String
formatKindForMessage k = "`" ++ FSL.formatSLKindAsString k ++ "`"

formatTypeForMessage :: R.MetaObject -> String
formatTypeForMessage t = case FSL.formatSLTypeAsSL' (\ _ _ -> Nothing) M.empty t of
	Just sl -> "`" ++ SL.formatSLTypeAsString sl ++ "`"
	Nothing -> "(in TL) `" ++ FTL.formatMetaObjectAsString t ++ "`"

formatTermForMessage :: R.MetaObject -> String
formatTermForMessage t = case FSL.formatSLTermAsSL' (\ _ _ -> Nothing) (\ _ _ _ -> Nothing) M.empty M.empty t of
	Just sl -> "`" ++ SL.formatSLTermAsString sl ++ "`"
	Nothing -> "(in TL) `" ++ FTL.formatMetaObjectAsString t ++ "`"

compileSLKind :: SL.Kind Range -> ErrorMonad R.SLKind
compileSLKind (SL.KindType _) =
	return R.SLKindType
compileSLKind (SL.KindFun _ params res) = do
	params' <- mapM compileSLKind params
	res' <- compileSLKind res
	return (foldr R.SLKindFun res' params')

compileSLType :: M.Map SL.NameOfType TypeInScope
              -> [(R.MetaObject, Range)]
              -> SL.Type Range
              -> ErrorMonad R.MetaObject
compileSLType typeScope typeParams (SL.TypeName range name) =
	case M.lookup name typeScope of
		Just typeInScope -> do
			when (length typeParams < length (typeParamsOfTypeInScope typeInScope)) $
				fail ("at " ++ formatRange range ++ ": substitution `" ++ SL.unNameOfType name ++ "` takes " ++
					show (length (typeParamsOfTypeInScope typeInScope)) ++ " parameters, but only " ++
					show (length typeParams) ++ " were provided.")
			let typeParamsToSub = take (length (typeParamsOfTypeInScope typeInScope)) typeParams
			let typeParamsNotToSub = drop (length (typeParamsOfTypeInScope typeInScope)) typeParams
			sequence [do
				let actualKind = R.slKindOfMetaObject type_
				unless (actualKind == expectedKind) $
					fail ("at " ++ formatRange range ++ ": parameter #" ++ show i ++ " to substitution `" ++
						SL.unNameOfType name ++ "` should have kind `" ++ FSL.formatSLKindAsString expectedKind ++
						"`, but actual parameter at " ++ formatRange argRange ++ " has kind " ++
						formatKindForMessage actualKind ++ ".")
				| ((type_, argRange), expectedKind, i) <- zip3 typeParamsToSub (typeParamsOfTypeInScope typeInScope) [1..]]
			let value = valueOfTypeInScope typeInScope (map fst typeParamsToSub)
			compileSLTypeApps (value, range) typeParamsNotToSub
		Nothing -> fail ("at " ++ formatRange range ++ ": type name `" ++ SL.unNameOfType name ++ "` is not in scope")
compileSLType typeScope typeParams (SL.TypeApp range fun arg) = do
	arg' <- compileSLType typeScope [] arg
	compileSLType typeScope ((arg', SL.tagOfType arg):typeParams) fun
compileSLType typeScope typeParams other | not (null typeParams) = do
	other' <- compileSLType typeScope [] other
	compileSLTypeApps (other', SL.tagOfType other) typeParams
compileSLType typeScope [] (SL.TypeFun range params res) = do
	params' <- sequence [do
		param' <- compileSLType typeScope [] param
		unless (R.slKindOfMetaObject param' == R.SLKindType) $
			fail ("in function type at " ++ formatRange range ++ ": parameter #" ++ show i ++ " has type " ++
				formatTypeForMessage param' ++ " (at " ++ formatRange (SL.tagOfType param) ++ "), but that type has \
				\kind " ++ formatKindForMessage (R.slKindOfMetaObject param') ++ ", and function parameters \
				\are supposed to have kind `*`.")
		return param'
		| (i, param) <- zip [1..] params]
	res' <- compileSLType typeScope [] res
	unless (R.slKindOfMetaObject res' == R.SLKindType) $
		fail ("in function type at " ++ formatRange range ++ ": the return type is " ++ formatTypeForMessage res' ++
			" (at " ++ formatRange (SL.tagOfType res) ++ "), which has kind " ++
			formatKindForMessage (R.slKindOfMetaObject res') ++ ", but function return types are supposed to have \
			\kind `*`.")
	return (foldr R.MOSLTypeFun res' params')
compileSLType typeScope [] (SL.TypeLazy range x) = do
	x' <- compileSLType typeScope [] x
	unless (R.slKindOfMetaObject x' == R.SLKindType) $
		fail ("in `(lazy ...)` type at " ++ formatRange range ++ ": the inner type is " ++
			formatTypeForMessage x' ++ " (at " ++ formatRange (SL.tagOfType x) ++ "), which has kind " ++
			formatKindForMessage (R.slKindOfMetaObject x') ++ ", but it's supposed to have kind `*`.")
	return (R.MOSLTypeLazy x')

compileSLTypeApps :: (R.MetaObject, Range) -> [(R.MetaObject, Range)] -> ErrorMonad R.MetaObject
compileSLTypeApps (base, _) [] = return base
compileSLTypeApps (fun, funRange) ((arg, argRange):args) =
	case R.slKindOfMetaObject fun of
		R.SLKindType -> fail ("at " ++ formatRange (joinRanges funRange argRange) ++ ": the type " ++
			formatTypeForMessage fun ++ " (at " ++ formatRange funRange ++ "), is being applied like a function, but \
			\it has kind `*`.")
		R.SLKindFun paramKind _ -> do
			unless (paramKind == R.slKindOfMetaObject arg) $
				fail ("at " ++ formatRange (joinRanges funRange argRange) ++ ": the type " ++
					formatTypeForMessage fun ++ " (at " ++ formatRange funRange ++ "), expects an argument of kind " ++
					formatKindForMessage paramKind ++ ", but the argument it was given is " ++
					formatTypeForMessage arg ++ " (at " ++ formatRange argRange ++ "), which has kind " ++
					formatKindForMessage (R.slKindOfMetaObject arg) ++ ".")
			compileSLTypeApps (R.MOSLTypeApp fun arg, joinRanges funRange argRange) args

compileSLTerm :: Scope
              -> [(R.MetaObject, Range)]
              -> SL.Term Range
              -> ErrorMonad R.MetaObject
compileSLTerm scope termParams (SL.TermName range name typeParams) = do
	case M.lookup name (termsInScope scope) of
		Just termInScope -> do
			typeParams' <- sequence [do
				typeParam' <- compileSLType (typesInScope scope) [] typeParam
				return (typeParam', SL.tagOfType typeParam)
				| typeParam <- typeParams]
			when (length typeParams /= length (typeParamsOfTermInScope termInScope)) $
				fail ("at " ++ formatRange range ++ ": name `" ++ SL.unNameOfTerm name ++ "` takes " ++
					show (length (typeParamsOfTermInScope termInScope)) ++ " type parameters, but " ++
					show (length typeParams) ++ " were provided.")
			when (length termParams < length (termParamsOfTermInScope termInScope)) $
				fail ("at " ++ formatRange range ++ ": name `" ++ SL.unNameOfTerm name ++ "` takes " ++
					show (length (typeParamsOfTermInScope termInScope)) ++ " term parameters, but only " ++
					show (length typeParams) ++ " were provided.")
			let termParamsToSub = take (length (termParamsOfTermInScope termInScope)) termParams
			let termParamsNotToSub = drop (length (termParamsOfTermInScope termInScope)) termParams
			sequence [do
				let actualKind = R.slKindOfMetaObject type_
				unless (actualKind == expectedKind) $
					fail ("at " ++ formatRange range ++ ": type parameter #" ++ show i ++ " to name `" ++
						SL.unNameOfTerm name ++ "` should have kind " ++ formatKindForMessage expectedKind ++
						", but the parameter given is " ++ formatTypeForMessage type_ ++ " (at " ++
						formatRange typeRange ++ "), which has kind `" ++ formatKindForMessage actualKind ++ ".")
				| ((type_, typeRange), expectedKind, i) <- zip3 typeParams' (typeParamsOfTermInScope termInScope) [1..]]
			sequence [do
				let actualType = R.slTypeOfMetaObject arg
				unless (actualType `R.equivalentMetaObjects` expectedType) $
					fail ("at " ++ formatRange range ++ ": term parameter #" ++ show i ++ " to name `" ++
						SL.unNameOfTerm name ++ "` should have type " ++ formatTypeForMessage expectedType ++
						", but the parameter given is " ++ formatTermForMessage arg ++ " (at " ++
						formatRange argRange ++ "), which has type " ++ formatTypeForMessage actualType ++ ".")
				| ((arg, argRange), expectedType, i) <- zip3 termParamsToSub (termParamsOfTermInScope termInScope) [1..]]
			let value = valueOfTermInScope termInScope (map fst typeParams') (map fst termParamsToSub)
			compileSLTermApps (value, range) termParamsNotToSub
		Nothing -> fail ("at " ++ formatRange range ++ ": term name `" ++ SL.unNameOfTerm name ++ "` is not in scope")
compileSLTerm scope termParams (SL.TermApp range fun arg) = do
	arg' <- compileSLTerm scope [] arg
	compileSLTerm scope ((arg', SL.tagOfTerm arg):termParams) fun
compileSLTerm scope termParams other | not (null termParams) = do
	other' <- compileSLTerm scope [] other
	compileSLTermApps (other', SL.tagOfTerm other) termParams
compileSLTerm scope [] (SL.TermAbs range params body) = f (termsInScope scope) params
	where
		f :: M.Map SL.NameOfTerm TermInScope -> [(SL.NameOfTerm, SL.Type Range)] -> ErrorMonad R.MetaObject
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
			Nothing -> fail ("at " ++ formatRange range ++ ": constructor `" ++ SL.unNameOfTerm ctorName ++ "` is not \
				\ in scope")
		ctorTypeArgs' <- sequence [do
			ctorTypeArg' <- compileSLType (typesInScope scope) [] ctorTypeArg
			return (ctorTypeArg', SL.tagOfType ctorTypeArg)
			| ctorTypeArg <- ctorTypeArgs]
		unless (length ctorTypeArgs' == length (R.typeParamsOfSLDataDefn (R.parentDataOfSLCtorDefn ctor))) $
			fail ("at " ++ formatRange range ++ ": constructor `" ++ SL.unNameOfTerm ctorName ++ "` expects " ++
				show (length (R.typeParamsOfSLDataDefn (R.parentDataOfSLCtorDefn ctor))) ++ " type parameters, but \
				\instead it got " ++ show (length ctorTypeArgs') ++ ".")
		sequence [do
			let actualKind = R.slKindOfMetaObject type_
			unless (actualKind == expectedKind) $
				fail ("at " ++ formatRange range ++ ": type parameter #" ++ show i ++ " to constructor `" ++
					SL.unNameOfTerm ctorName ++ "` should have kind " ++ formatKindForMessage expectedKind ++
					", but the actual parameter given is " ++ formatTypeForMessage type_ ++ " (at " ++
					formatRange typeRange ++ "), which has kind " ++ formatKindForMessage actualKind ++ ".")
			| ((type_, typeRange), expectedKind, i) <- zip3 ctorTypeArgs' (R.typeParamsOfSLDataDefn (R.parentDataOfSLCtorDefn ctor)) [1..]]
		let fieldTypes = map ($ (map fst ctorTypeArgs')) (R.fieldTypesOfSLCtorDefn ctor)
		let fieldNames' = map (R.NameOfSLTerm . SL.unNameOfTerm) fieldNames
		let termScope' = foldr
			(\ (name, name', type_) -> M.insert name (TermInScope [] [] (\ _ _ -> R.MOSLTermName name' type_)))
			(termsInScope scope)
			(zip3 fieldNames fieldNames' fieldTypes)
		body' <- compileSLTerm (scope { termsInScope = termScope' }) [] body
		return (ctor, map fst ctorTypeArgs', fieldNames', body')
		| (ctorName, ctorTypeArgs, fieldNames, body) <- clauses]
	return (R.MOSLTermCase subject' clauses')
compileSLTerm scope [] (SL.TermWrap _ x) = do
	x' <- compileSLTerm scope [] x
	return (R.MOSLTermWrap x')
compileSLTerm scope [] (SL.TermUnwrap range x) = do
	x' <- compileSLTerm scope [] x
	case R.reduceMetaObject (R.slTypeOfMetaObject x') of
		R.MOSLTypeLazy _ -> return ()
		other -> fail ("at " ++ formatRange range ++ ": inner expression of `(unwrap ...)`, at " ++
			formatRange (SL.tagOfTerm x) ++ ", should have type `(lazy ...)`, but instead it has type " ++
			formatTypeForMessage other ++ ".")
	return (R.MOSLTermUnwrap x')

compileSLTermApps :: (R.MetaObject, Range) -> [(R.MetaObject, Range)] -> ErrorMonad R.MetaObject
compileSLTermApps (base, _) [] = return base
compileSLTermApps (fun, funRange) ((arg, argRange):args) =
	case R.reduceMetaObject (R.slTypeOfMetaObject fun) of
		R.MOSLTypeFun paramType _ -> do
			unless (paramType `R.equivalentMetaObjects` R.slTypeOfMetaObject arg) $
				fail ("function at " ++ formatRange funRange ++ " expects an argument of type " ++
					formatTypeForMessage paramType ++ ", but its actual argument at " ++ formatRange argRange ++
					" has type " ++ formatTypeForMessage (R.slTypeOfMetaObject arg) ++ ".")
			compileSLTermApps (R.MOSLTermApp fun arg, joinRanges funRange argRange) args
		otherType -> fail ("term at " ++ formatRange funRange ++ " has type " ++ formatTypeForMessage otherType ++
			", but at " ++ formatRange (joinRanges funRange argRange) ++ " it is being used as if it were a function.")

data Defns = Defns {
	dataDefns :: M.Map SL.NameOfType R.SLDataDefn,
	ctorDefns :: M.Map SL.NameOfTerm R.SLCtorDefn,
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
		`M.union` M.map termInScopeForCtorDefn (ctorDefns defns)
	}

compileSLDirectives :: [SL.Dir Range] -> ErrorMonad Defns
compileSLDirectives directives = do
	-- TODO: Detect name conflicts: type/type, term/term, ctor/ctor, term/ctor

	dataDefnsAndCtorDefnPromises <- sequence [do

		paramKinds' <- sequence [compileSLKind kind | (_, kind) <- params]

		let dataDefn = R.SLDataDefn {
			R.nameOfSLDataDefn = R.NameOfSLType (SL.unNameOfType typeName),
			R.typeParamsOfSLDataDefn = paramKinds'
			}

		let
			ctorPromise :: M.Map SL.NameOfType R.SLDataDefn -> ErrorMonad (M.Map SL.NameOfTerm R.SLCtorDefn)
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
						R.nameOfSLCtorDefn = R.NameOfSLTerm (SL.unNameOfTerm ctorName),
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

	-- termPromises :: M.Map SL.NameOfTerm (M.Map SL.NameOfTerm R.SLTermDefn -> (R.SLTermDefn, ErrorMonad ()))
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
			promise :: M.Map SL.NameOfTerm R.SLTermDefn -> (R.SLTermDefn, ErrorMonad ())
			promise termDefns = let

				scope1 = scopeForDefns (Defns dataDefns ctorDefns termDefns)
				paramTermsInScope = M.fromList [let
					metaObject = R.MOSLTermName (runNameForTermParamName name) termParamType'
					in (name, TermInScope [] [] (const (const metaObject)))
					| ((name, _), termParamType') <- zip termParams termParamTypes']
				scope2 = scope1 {
					typesInScope = typeScope,
					termsInScope = paramTermsInScope `M.union` termsInScope scope1
					}

				valueOrError = compileSLTerm scope2 [] value

				makeSubbed object typeParamValues =
					R.substituteMetaObject (R.Substitutions M.empty typeSubs M.empty) object
					where typeSubs = M.fromList (zip [runNameForTypeParamName n | (n, _) <- typeParams] typeParamValues)

				termDefn = R.SLTermDefn {
					R.nameOfSLTermDefn = R.NameOfSLTerm (SL.unNameOfTerm termName),
					R.typeParamsOfSLTermDefn = typeParamKinds',
					R.typeOfSLTermDefn = makeSubbed (foldr R.MOSLTypeFun type_' termParamTypes'),
					R.valueOfSLTermDefn = makeSubbed (case valueOrError of Success value -> value)
					}

				in (termDefn, valueOrError >> return ())

		return (termName, promise)

		| SL.DirLet _ termName typeParams termParams type_ value <- directives]

	let
		termDefns :: M.Map SL.NameOfTerm R.SLTermDefn
		termDefns = M.map (fst . ($ termDefns)) termPromises
	sequence [snd (promise termDefns) | promise <- M.elems termPromises]

	return $ Defns dataDefns ctorDefns termDefns

