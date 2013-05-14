module Metacompiler.Compile.CompileSL where

import Control.Monad (when, unless, liftM)
import qualified Data.Map as M
import Metacompiler.Error
import qualified Metacompiler.Compile.FormatSL as FSL
import qualified Metacompiler.Compile.FormatTL as FTL
import qualified Metacompiler.SLRuntime.Types as R
import qualified Metacompiler.SL.Syntax as SL
import qualified Metacompiler.SL.ToSExpr as SL

data TypeInScope
	= NameTypeInScope R.NameOfSLType R.SLKind
	| DefinedTypeInScope R.SLDataDefn

data TermInScope
	= NameTermInScope R.NameOfSLTerm R.SLType
	| DefinedTermInScope R.SLTermDefn
	| CtorTermInScope R.SLCtorDefn

data Scope = Scope {
	typesInScope :: M.Map SL.NameOfType TypeInScope,
	ctorsInScope :: M.Map SL.NameOfTerm R.SLCtorDefn,
	termsInScope :: M.Map SL.NameOfTerm TermInScope
	}

formatKindForMessage :: R.SLKind -> String
formatKindForMessage k = "`" ++ FSL.formatSLKindAsString k ++ "`"

formatTypeForMessage :: R.SLType -> String
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

compileSLType :: M.Map SL.NameOfType TypeInScope -> SL.Type Range -> ErrorMonad R.MetaObject
compileSLType typeScope (SL.TypeName range name) =
	case M.lookup name typeScope of
		Just (NameTypeInScope name' kind) = return (SLTypeName name' kind)
		Just (DefinedTypeInScope defn) = return (SLTypeDefined defn)
		Nothing -> fail ("at " ++ formatRange range ++ ": type name `" ++ SL.unNameOfType name ++ "` is not in scope")
compileSLType typeScope (SL.TypeApp range fun arg) = do
	fun' <- compileSLType typeScope fun
	arg' <- compileSLType typeScope arg
	case kindOfSLType fun' of
		R.SLKindType -> fail ("at " ++ formatRange range ++ ": the type " ++ formatTypeForMessage fun' ++ " (at " ++
			formatRange (SL.tagOfType fun) ++ "), is being applied like a function, but it has kind `*`.")
		R.SLKindFun paramKind _
			| paramKind == R.kindOfType arg' ->
				return (R.SLTypeApp fun' arg')
			| otherwise -> fail ("at " ++ formatRange range ++ ": the type " ++ formatTypeForMessage fun' ++ " (at " ++
				formatRange (SL.tagOfType fun) ++ "), expects an argument of kind " ++
				formatKindForMessage paramKind ++ ", but the argument it was given is " ++ formatTypeForMessage arg ++
				" (at " ++ formatRange (SL.tagOfType arg) ++ "), which has kind " ++
				formatKindForMessage (R.kindOfType arg') ++ ".")
compileSLType typeScope (SL.TypeFun range params res) = do
	params' <- sequence [do
		param' <- compileSLType typeScope param
		unless (R.kindOfType param' == R.SLKindType) $
			fail ("in function type at " ++ formatRange range ++ ": parameter #" ++ show i ++ " has type " ++
				formatTypeForMessage param' ++ " (at " ++ formatRange (SL.tagOfType param) ++ "), but that type has \
				\kind " ++ formatKindForMessage (R.kindOfType param') ++ ", and function parameters \
				\are supposed to have kind `*`.")
		return param'
		| (i, param) <- zip [1..] params]
	res' <- compileSLType typeScope res
	unless (R.kindOfType res' == R.SLKindType) $
		fail ("in function type at " ++ formatRange range ++ ": the return type is " ++ formatTypeForMessage res' ++
			" (at " ++ formatRange (SL.tagOfType res) ++ "), which has kind " ++
			formatKindForMessage (R.kindOfType res') ++ ", but function return types are supposed to have \
			\kind `*`.")
	return (foldr R.SLTypeFun res' params')
compileSLType typeScope (SL.TypeLazy range x) = do
	x' <- compileSLType typeScope x
	unless (R.kindOfType x' == R.SLKindType) $
		fail ("in `(lazy ...)` type at " ++ formatRange range ++ ": the inner type is " ++
			formatTypeForMessage x' ++ " (at " ++ formatRange (SL.tagOfType x) ++ "), which has kind " ++
			formatKindForMessage (R.kindOfType x') ++ ", but it's supposed to have kind `*`.")
	return (R.MOSLTypeLazy x')

compileSLTerm :: Scope -> SL.Term Range -> ErrorMonad R.MetaObject
compileSLTerm scope (SL.TermName range name typeParams) = do
	typeParams' <- mapM compilerSLType typeParams
	let
		formatKindList :: [R.SLKind] -> String
		formatKindList [] = "(none)"
		formatKindList l = intercalate ", " [formatKindForMessage k | k <- l]
	case (M.lookup name (termsInScope scope), typeParams') of
		(Just (NameTermInScope name' type_), []) -> return (SL.TermName name' type_)
		(Just (NameTermInScope _ _), _:_) -> fail ("at " ++ formatRange range ++ ": term name `" ++
			SL.unNameOfTerm name ++ "` expects no type parameters, but " ++ show (length typeParams) ++ " type \
			\parameters have been given.")
		(Just (DefinedTermInScope defn), _)
			| typeParamsOfSLDataDefn defn == map R.kindOfType typeParams' ->
				return (SL.TermDefined defn typeParams')
			| otherwise -> fail ("at " ++ formatRange range ++ ": global definition `" ++ SL.unNameOfTerm term ++ "` \
				\expects type parameters with the following kinds: " ++ formatKindList (typeParamsOfSLDataDefn defn) ++
				" but it got type parameters with the following kinds: " ++
				formatKindList (map R.kindOfType typeParams') ++ ".")
		(Just (CtorTermInScope ctor), _)
			| expectedKinds == map R.kindOfType typeParams' -> do
				let fieldTypes = map ($ typeParams') fieldTypesOfSLCtorDefn
				let fieldVars = [(R.NameOfSLTerm ("f" ++ show i), ty) | (i, ty) <- zip [1..] fieldTypes]
				let fieldTerms = map (uncurry R.SLTermName) fieldVars
				return $ foldr R.SLTermAbs (R.SLTermData ctor typeParams' fieldTerms) fieldVars
			| otherwise -> fail ("at " ++ formatRange range ++ ": constructor `" ++ SL.unNameOfTerm term ++ "` \
				\expects type parameters with the following kinds: " ++ formatKindList expectedKinds ++ " but \
				\it got type parameters with the following kinds: " ++ formatKindList (map R.kindOfType typeParams') ++
				".")
			where expectedKinds = typeParamsOfSLDataDefn (parentDataOfSLCtorDefn ctor)
		(Nothing, _) -> fail ("at " ++ formatRange range ++ ": term name `" ++ SL.unNameOfTerm name ++ "` is not in scope")
compileSLTerm scope (SL.TermApp range fun arg) = do
	fun' <- compileSLTerm scope fun
	arg' <- compileSLTerm scope arg
	case typeOfSLTerm fun' of
		R.SLTypeFun paramKind _
			| paramKind == R.typeOfTerm arg' ->
				return (R.SLTermApp fun' arg')
			| otherwise -> fail ("at " ++ formatRange range ++ ": the type " ++ formatTermForMessage fun' ++ " (at " ++
				formatRange (SL.tagOfType fun) ++ "), expects an argument of type " ++
				formatTypeForMessage paramType ++ ", but the argument it was given is " ++ formatTermForMessage arg ++
				" (at " ++ formatRange (SL.tagOfTerm arg) ++ "), which has type " ++
				formatTypeForMessage (R.typeOfTerm arg') ++ ".")
		other -> fail ("at " ++ formatRange range ++ ": the term " ++ formatTermForMessage fun ++ " (at " ++
			formatRange (SL.tagOfTerm fun) ++ "), is being applied like a function, but it has type " ++
			formatTypeForMessage other ++ ".")
compileSLTerm scope (SL.TermAbs range params body) = f (termsInScope scope) params
	where
		f :: M.Map SL.NameOfTerm TermInScope -> [(SL.NameOfTerm, SL.Type Range)] -> ErrorMonad R.MetaObject
		f termScope' [] = compileSLTerm (scope { termsInScope = termScope' }) body
		f termScope' ((paramName, paramType):paramsLeft) = do
			let paramName' = R.NameOfSLTerm (SL.unNameOfTerm paramName)
			paramType' <- compileSLType (typesInScope scope) paramType
			let termScope'' = M.insert paramName (NameTermInScope paramName' paramType') termScope'
			body' <- f termScope'' paramsLeft
			return (R.MOSLTermAbs (paramName', paramType') body')
compileSLTerm scope (SL.TermCase range subject clauses) = do
	subject' <- compileSLTerm scope subject
	clauses' <- sequence [do
		ctor <- case M.lookup ctorName (ctorsInScope scope) of
			Just ctor -> return ctor
			Nothing -> fail ("at " ++ formatRange range ++ ": constructor `" ++ SL.unNameOfTerm ctorName ++ "` is not \
				\ in scope")
		ctorTypeArgs' <- mapM (compileSLType (typesInScope scope)) ctorTypeArgs
		let expectedKinds = R.typeParamsOfSLDataDefn (R.parentDataOfSLCtorDefn ctor)
		let actualKinds = map R.kindOfType ctorTypeArgs'
		unless (expectedKinds == actualKinds) $
			fail ("at " ++ formatRange range ++ ": constructor `" ++ SL.unNameOfTerm ctorName ++ "` expects type \
				\parameters with the following kinds: " ++ formatKindList expectedKinds ++ " but it got type \
				\parameters with the following kinds: " ++ formatKindList actualKinds)
		let fieldTypes = map ($ ctorTypeArgs') (R.fieldTypesOfSLCtorDefn ctor)
		let fieldNames' = map (R.NameOfSLTerm . SL.unNameOfTerm) fieldNames
		let termScope' = foldr
			(\ (name, name', type_) -> M.insert name (NamedTermInScope name- type_))
			(termsInScope scope)
			(zip3 fieldNames fieldNames' fieldTypes)
		body' <- compileSLTerm (scope { termsInScope = termScope' }) body
		return (ctor, ctorTypeArgs', fieldNames', body')
		| (ctorName, ctorTypeArgs, fieldNames, body) <- clauses]
	return (R.MOSLTermCase subject' clauses')
compileSLTerm scope (SL.TermWrap _ x) = do
	x' <- compileSLTerm scope x
	return (R.MOSLTermWrap x')
compileSLTerm scope (SL.TermUnwrap range x) = do
	x' <- compileSLTerm scope x
	case R.typeOfTerm x' of
		R.MOSLTypeLazy _ -> return (R.MOSLTermUnwrap x')
		other -> fail ("at " ++ formatRange range ++ ": inner expression of `(unwrap ...)`, at " ++
			formatRange (SL.tagOfTerm x) ++ ", should have type `(lazy ...)`, but instead it has type " ++
			formatTypeForMessage other ++ ".")

data Defns = Defns {
	dataDefns :: M.Map SL.NameOfType R.SLDataDefn,
	ctorDefns :: M.Map SL.NameOfTerm R.SLCtorDefn,
	termDefns :: M.Map SL.NameOfTerm R.SLTermDefn
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
				let typeScope = M.fromList [(name, NameTypeInScope (runNameForParamName) paramKind')
					| ((name, _), paramKind') <- zip params paramKinds']
					`M.union` M.map DefinedTypeInScope dataDefns
				liftM M.fromList $ sequence [do
					fieldTypes' <- mapM (compileSLType typeScope) fieldTypes
					let ctorDefn = R.SLCtorDefn {
						R.nameOfSLCtorDefn = R.NameOfSLTerm (SL.unNameOfTerm ctorName),
						R.parentDataOfSLCtorDefn = dataDefn,
						R.fieldTypesOfSLCtorDefn = [\ paramValues ->
							let subs = M.fromList [
								(runNameForParamName n, R.TypeSub (Identity . foldl R.SLTypeApp v))
								| ((n, _), v) <- zip params paramValues]
							in runIdentity (R.substituteSLType subs fieldType')
							| fieldType' <- fieldTypes']
						}
					return (ctorName, ctorDefn)
					| (ctorName, fieldTypes) <- ctors]

		return (typeName, dataDefn, ctorPromise)
		| SL.DirData _ typeName params ctors <- directives]

	let allDataDefns = M.fromList [(name, defn) | (name, defn, _) <- dataDefnsAndCtorDefnPromises]
	allCtorDefns <- liftM M.unions $ sequence [ctorPromise allDataDefns | (_, _, ctorPromise) <- dataDefnsAndCtorDefnPromises]

	-- termPromises :: M.Map SL.NameOfTerm (M.Map SL.NameOfTerm R.SLTermDefn -> (R.SLTermDefn, ErrorMonad ()))
	termPromises <- liftM M.fromList $ sequence [do

		typeParamKinds' <- sequence [compileSLKind kind | (_, kind) <- typeParams]
		let runNameForTypeParamName = R.NameOfSLType . SL.unNameOfType

		let typeScope = M.fromList [(name, NameTypeInScope (runNameForTypeParamName name) typeParamKind')
			| ((name, _), typeParamKind') <- zip typeParams typeParamKinds']
			`M.union` M.map typeInScopeForDataDefn dataDefns

		termParamTypes' <- sequence [compileSLType typeScope paramType | (_, paramType) <- termParams]
		let runNameForTermParamName = R.NameOfSLTerm . SL.unNameOfTerm

		type_' <- compileSLType typeScope type_

		let
			promise :: M.Map SL.NameOfTerm R.SLTermDefn -> (R.SLTermDefn, ErrorMonad ())
			promise allTermDefns = let

				scope1 = Scope {
					typesInScope = M.map DefinedTypeInScope allDataDefns,
					ctorsInScope = allCtorDefns,
					termsInScope = M.map CtorTermInScope allCtorDefns
						`M.union` M.map DefinedTermInScope allTermDefns
					}
				paramTermsInScope = M.fromList [(name, NameTermInScope (runNameForTermParamName name) termParamType')
					| ((name, _), termParamType') <- zip termParams termParamTypes']
				scope2 = scope1 {
					typesInScope = typeScope,
					termsInScope = paramTermsInScope `M.union` termsInScope scope1
					}

				valueOrError = compileSLTerm scope2 value

				makeSubbed object typeParamValues =
					let subs = M.fromList [
								(runNameForParamName n, R.TypeSub (Identity . foldl R.SLTypeApp v))
								| ((n, _), v) <- zip params paramValues]
							in runIdentity (R.substituteSLType subs fieldType')

				let wholeType = foldr R.MOSLTypeFun type_' termParamTypes'

				termDefn = R.SLTermDefn {
					R.nameOfSLTermDefn = R.NameOfSLTerm (SL.unNameOfTerm termName),
					R.typeParamsOfSLTermDefn = typeParamKinds',
					R.typeOfSLTermDefn = (\ typeParamValues ->
						let
							subs = M.fromList [
								(runNameForTypeParamName n, R.TypeSub (Identity . foldl R.SLTypeApp v))
								| ((n, _), v) <- zip typeParams typeParamValues]
						in runIdentity (R.substituteSLType subs wholeType)
						),
					R.valueOfSLTermDefn = (\ typeParamValues ->
						let subs = M.fromList [
								(runNameForTypeParamName n, R.TypeSub (Identity . foldl R.SLTypeApp v))
								| ((n, _), v) <- zip typeParams typeParamValues],
							realValue = case value of Success v -> v
						in runIdentity (R.substituteSLTerm (subs, M.empty) realValue)
						)
					}

				in (termDefn, valueOrError >> return ())

		return (termName, promise)

		| SL.DirLet _ termName typeParams termParams type_ value <- directives]

	let
		allTermDefns :: M.Map SL.NameOfTerm R.SLTermDefn
		allTermDefns = M.map (fst . ($ allTermDefns)) termPromises
	sequence [snd (promise allTermDefns) | promise <- M.elems termPromises]

	return $ Defns allDataDefns allCtorDefns allTermDefns

