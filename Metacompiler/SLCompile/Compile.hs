module Metacompiler.SLCompile.Compile where

import Control.Monad (when, unless, liftM)
import Control.Monad.Identity
import Data.List (intercalate)
import qualified Data.Map as M
import Metacompiler.Error
import qualified Metacompiler.SLCompile.Format as SLF
import qualified Metacompiler.SLRuntime.Substitute as SLR
import qualified Metacompiler.SLRuntime.TypeOf as SLR
import qualified Metacompiler.SLRuntime.Types as SLR
import qualified Metacompiler.SLSyntax.Types as SLS
import qualified Metacompiler.SLSyntax.ToSExpr as SLS

data TypeInScope
	= NameTypeInScope SLR.NameOfType SLR.Kind
	| DefinedTypeInScope SLR.DataDefn

data TermInScope
	= NameTermInScope SLR.NameOfTerm SLR.Type
	| DefinedTermInScope SLR.TermDefn
	| CtorTermInScope SLR.CtorDefn

data Scope = Scope {
	typesInScope :: M.Map SLS.NameOfType TypeInScope,
	ctorsInScope :: M.Map SLS.NameOfTerm SLR.CtorDefn,
	termsInScope :: M.Map SLS.NameOfTerm TermInScope
	}

formatKindForMessage :: SLR.Kind -> String
formatKindForMessage k = "`" ++ SLF.formatKindAsString k ++ "`"

formatKindListForMessage :: [SLR.Kind] -> String
formatKindListForMessage [] = "(none)"
formatKindListForMessage l = intercalate ", " [formatKindForMessage k | k <- l]

formatTypeForMessage :: SLR.Type -> String
formatTypeForMessage t = "`" ++ SLF.formatTypeAsString t ++ "`"

formatTermForMessage :: SLR.Term -> String
formatTermForMessage t = "`" ++ SLF.formatTermAsString t ++ "`"

compileKind :: SLS.Kind Range -> ErrorMonad SLR.Kind
compileKind (SLS.KindType _) =
	return SLR.KindType
compileKind (SLS.KindFun _ params res) = do
	params' <- mapM compileKind params
	res' <- compileKind res
	return (foldr SLR.KindFun res' params')

compileType :: M.Map SLS.NameOfType TypeInScope -> SLS.Type Range -> ErrorMonad SLR.Type
compileType typeScope (SLS.TypeName range name) =
	case M.lookup name typeScope of
		Just (NameTypeInScope name' kind) -> return (SLR.TypeName name' kind)
		Just (DefinedTypeInScope defn) -> return (SLR.TypeDefined defn)
		Nothing -> fail ("at " ++ formatRange range ++ ": type name `" ++ SLS.unNameOfType name ++ "` is not in scope")
compileType typeScope (SLS.TypeApp range fun arg) = do
	fun' <- compileType typeScope fun
	arg' <- compileType typeScope arg
	case SLR.kindOfType fun' of
		SLR.KindType -> fail ("at " ++ formatRange range ++ ": the type " ++ formatTypeForMessage fun' ++ " (at " ++
			formatRange (SLS.tagOfType fun) ++ "), is being applied like a function, but it has kind `*`.")
		SLR.KindFun paramKind _
			| paramKind == SLR.kindOfType arg' ->
				return (SLR.TypeApp fun' arg')
			| otherwise -> fail ("at " ++ formatRange range ++ ": the type " ++ formatTypeForMessage fun' ++ " (at " ++
				formatRange (SLS.tagOfType fun) ++ "), expects an argument of kind " ++
				formatKindForMessage paramKind ++ ", but the argument it was given is " ++ formatTypeForMessage arg' ++
				" (at " ++ formatRange (SLS.tagOfType arg) ++ "), which has kind " ++
				formatKindForMessage (SLR.kindOfType arg') ++ ".")
compileType typeScope (SLS.TypeFun range params res) = do
	params' <- sequence [do
		param' <- compileType typeScope param
		unless (SLR.kindOfType param' == SLR.KindType) $
			fail ("in function type at " ++ formatRange range ++ ": parameter #" ++ show i ++ " has type " ++
				formatTypeForMessage param' ++ " (at " ++ formatRange (SLS.tagOfType param) ++ "), but that type has \
				\kind " ++ formatKindForMessage (SLR.kindOfType param') ++ ", and function parameters \
				\are supposed to have kind `*`.")
		return param'
		| (i, param) <- zip [1..] params]
	res' <- compileType typeScope res
	unless (SLR.kindOfType res' == SLR.KindType) $
		fail ("in function type at " ++ formatRange range ++ ": the return type is " ++ formatTypeForMessage res' ++
			" (at " ++ formatRange (SLS.tagOfType res) ++ "), which has kind " ++
			formatKindForMessage (SLR.kindOfType res') ++ ", but function return types are supposed to have \
			\kind `*`.")
	return (foldr SLR.TypeFun res' params')
compileType typeScope (SLS.TypeLazy range x) = do
	x' <- compileType typeScope x
	unless (SLR.kindOfType x' == SLR.KindType) $
		fail ("in `(lazy ...)` type at " ++ formatRange range ++ ": the inner type is " ++
			formatTypeForMessage x' ++ " (at " ++ formatRange (SLS.tagOfType x) ++ "), which has kind " ++
			formatKindForMessage (SLR.kindOfType x') ++ ", but it's supposed to have kind `*`.")
	return (SLR.TypeLazy x')

compileTerm :: Scope -> SLS.Term Range -> ErrorMonad SLR.Term
compileTerm scope (SLS.TermName range name typeParams) = do
	typeParams' <- mapM (compileType (typesInScope scope)) typeParams
	case (M.lookup name (termsInScope scope), typeParams') of
		(Just (NameTermInScope name' type_), []) -> return (SLR.TermName name' type_)
		(Just (NameTermInScope _ _), _:_) -> fail ("at " ++ formatRange range ++ ": term name `" ++
			SLS.unNameOfTerm name ++ "` expects no type parameters, but " ++ show (length typeParams) ++ " type \
			\parameters have been given.")
		(Just (DefinedTermInScope defn), _)
			| SLR.typeParamsOfTermDefn defn == map SLR.kindOfType typeParams' ->
				return (SLR.TermDefined defn typeParams')
			| otherwise -> fail ("at " ++ formatRange range ++ ": global definition `" ++ SLS.unNameOfTerm name ++ "` \
				\expects type parameters with the following kinds: " ++
				formatKindListForMessage (SLR.typeParamsOfTermDefn defn) ++ " but it got type parameters with the \
				\following kinds: " ++ formatKindListForMessage (map SLR.kindOfType typeParams') ++ ".")
		(Just (CtorTermInScope ctor), _)
			| expectedKinds == map SLR.kindOfType typeParams' ->
				return (SLR.TermData ctor typeParams')
			| otherwise -> fail ("at " ++ formatRange range ++ ": constructor `" ++ SLS.unNameOfTerm name ++ "` \
				\expects type parameters with the following kinds: " ++ formatKindListForMessage expectedKinds ++
				" but it got type parameters with the following kinds: " ++
				formatKindListForMessage (map SLR.kindOfType typeParams') ++ ".")
			where expectedKinds = SLR.typeParamsOfDataDefn (SLR.parentDataOfCtorDefn ctor)
		(Nothing, _) -> fail ("at " ++ formatRange range ++ ": term name `" ++ SLS.unNameOfTerm name ++ "` is not in \
			\scope")
compileTerm scope (SLS.TermApp range fun arg) = do
	fun' <- compileTerm scope fun
	arg' <- compileTerm scope arg
	case SLR.typeOfTerm fun' of
		SLR.TypeFun paramType _
			| paramType == SLR.typeOfTerm arg' ->
				return (SLR.TermApp fun' arg')
			| otherwise -> fail ("at " ++ formatRange range ++ ": the type " ++ formatTermForMessage fun' ++ " (at " ++
				formatRange (SLS.tagOfTerm fun) ++ "), expects an argument of type " ++
				formatTypeForMessage paramType ++ ", but the argument it was given is " ++ formatTermForMessage arg' ++
				" (at " ++ formatRange (SLS.tagOfTerm arg) ++ "), which has type " ++
				formatTypeForMessage (SLR.typeOfTerm arg') ++ ".")
		other -> fail ("at " ++ formatRange range ++ ": the term " ++ formatTermForMessage fun' ++ " (at " ++
			formatRange (SLS.tagOfTerm fun) ++ "), is being applied like a function, but it has type " ++
			formatTypeForMessage other ++ ".")
compileTerm scope (SLS.TermAbs range params body) = f (termsInScope scope) params
	where
		f :: M.Map SLS.NameOfTerm TermInScope -> [(SLS.NameOfTerm, SLS.Type Range)] -> ErrorMonad SLR.Term
		f termScope' [] = compileTerm (scope { termsInScope = termScope' }) body
		f termScope' ((paramName, paramType):paramsLeft) = do
			let paramName' = SLR.NameOfTerm (SLS.unNameOfTerm paramName)
			paramType' <- compileType (typesInScope scope) paramType
			let termScope'' = M.insert paramName (NameTermInScope paramName' paramType') termScope'
			body' <- f termScope'' paramsLeft
			return (SLR.TermAbs (paramName', paramType') body')
compileTerm scope (SLS.TermCase range subject clauses) = do
	subject' <- compileTerm scope subject
	clauses' <- sequence [do
		ctor <- case M.lookup ctorName (ctorsInScope scope) of
			Just ctor -> return ctor
			Nothing -> fail ("at " ++ formatRange range ++ ": constructor `" ++ SLS.unNameOfTerm ctorName ++ "` is not \
				\ in scope")
		ctorTypeArgs' <- mapM (compileType (typesInScope scope)) ctorTypeArgs
		let expectedKinds = SLR.typeParamsOfDataDefn (SLR.parentDataOfCtorDefn ctor)
		let actualKinds = map SLR.kindOfType ctorTypeArgs'
		unless (expectedKinds == actualKinds) $
			fail ("at " ++ formatRange range ++ ": constructor `" ++ SLS.unNameOfTerm ctorName ++ "` expects type \
				\parameters with the following kinds: " ++ formatKindListForMessage expectedKinds ++ " but it got \
				\type parameters with the following kinds: " ++ formatKindListForMessage actualKinds)
		let fieldTypes = map ($ ctorTypeArgs') (SLR.fieldTypesOfCtorDefn ctor)
		let fieldNames' = map (SLR.NameOfTerm . SLS.unNameOfTerm) fieldNames
		let termScope' = foldr
			(\ (name, name', type_) -> M.insert name (NameTermInScope name' type_))
			(termsInScope scope)
			(zip3 fieldNames fieldNames' fieldTypes)
		body' <- compileTerm (scope { termsInScope = termScope' }) body
		return (ctor, ctorTypeArgs', fieldNames', body')
		| (ctorName, ctorTypeArgs, fieldNames, body) <- clauses]
	return (SLR.TermCase subject' clauses')
compileTerm scope (SLS.TermWrap _ x) = do
	x' <- compileTerm scope x
	return (SLR.TermWrap x')
compileTerm scope (SLS.TermUnwrap range x) = do
	x' <- compileTerm scope x
	case SLR.typeOfTerm x' of
		SLR.TypeLazy _ -> return (SLR.TermUnwrap x')
		other -> fail ("at " ++ formatRange range ++ ": inner expression of `(unwrap ...)`, at " ++
			formatRange (SLS.tagOfTerm x) ++ ", should have type `(lazy ...)`, but instead it has type " ++
			formatTypeForMessage other ++ ".")

data Defns = Defns {
	dataDefns :: M.Map SLS.NameOfType SLR.DataDefn,
	ctorDefns :: M.Map SLS.NameOfTerm SLR.CtorDefn,
	termDefns :: M.Map SLS.NameOfTerm SLR.TermDefn
	}

scopeForDefns :: Defns -> Scope
scopeForDefns defns = Scope {
	typesInScope = M.map DefinedTypeInScope (dataDefns defns),
	ctorsInScope = ctorDefns defns,
	termsInScope = M.map DefinedTermInScope (termDefns defns)
		`M.union` M.map CtorTermInScope (ctorDefns defns)
	}

compileSLDirectives :: [SLS.Dir Range] -> ErrorMonad Defns
compileSLDirectives directives = do
	-- TODO: Detect name conflicts: type/type, term/term, ctor/ctor, term/ctor

	dataDefnsAndCtorDefnPromises <- sequence [do

		paramKinds' <- sequence [compileKind kind | (_, kind) <- params]

		let dataDefn = SLR.DataDefn {
			SLR.nameOfDataDefn = SLR.NameOfType (SLS.unNameOfType typeName),
			SLR.typeParamsOfDataDefn = paramKinds'
			}

		let
			ctorPromise :: M.Map SLS.NameOfType SLR.DataDefn -> ErrorMonad (M.Map SLS.NameOfTerm SLR.CtorDefn)
			ctorPromise dataDefns = do
				let runNameForParamName = SLR.NameOfType . SLS.unNameOfType
				let typeScope = M.fromList [(name, NameTypeInScope (runNameForParamName name) paramKind')
					| ((name, _), paramKind') <- zip params paramKinds']
					`M.union` M.map DefinedTypeInScope dataDefns
				liftM M.fromList $ sequence [do
					fieldTypes' <- mapM (compileType typeScope) fieldTypes
					let ctorDefn = SLR.CtorDefn {
						SLR.nameOfCtorDefn = SLR.NameOfTerm (SLS.unNameOfTerm ctorName),
						SLR.parentDataOfCtorDefn = dataDefn,
						SLR.fieldTypesOfCtorDefn = [\ paramValues ->
							let subs = M.fromList [
								(runNameForParamName n, SLR.simpleTypeSub v)
								| ((n, _), v) <- zip params paramValues]
							in runIdentity (SLR.substituteType subs fieldType')
							| fieldType' <- fieldTypes']
						}
					return (ctorName, ctorDefn)
					| (ctorName, fieldTypes) <- ctors]

		return (typeName, dataDefn, ctorPromise)
		| SLS.DirData _ typeName params ctors <- directives]

	let allDataDefns = M.fromList [(name, defn) | (name, defn, _) <- dataDefnsAndCtorDefnPromises]
	allCtorDefns <- liftM M.unions $ sequence [ctorPromise allDataDefns | (_, _, ctorPromise) <- dataDefnsAndCtorDefnPromises]

	-- termPromises :: M.Map SLS.NameOfTerm (M.Map SLS.NameOfTerm SLR.TermDefn -> (SLR.TermDefn, ErrorMonad ()))
	termPromises <- liftM M.fromList $ sequence [do

		typeParamKinds' <- sequence [compileKind kind | (_, kind) <- typeParams]
		let runNameForTypeParamName = SLR.NameOfType . SLS.unNameOfType

		let typeScope = M.fromList [(name, NameTypeInScope (runNameForTypeParamName name) typeParamKind')
			| ((name, _), typeParamKind') <- zip typeParams typeParamKinds']
			`M.union` M.map DefinedTypeInScope allDataDefns

		termParamTypes' <- sequence [compileType typeScope paramType | (_, paramType) <- termParams]
		let runNameForTermParamName = SLR.NameOfTerm . SLS.unNameOfTerm

		type_' <- compileType typeScope type_

		let
			promise :: M.Map SLS.NameOfTerm SLR.TermDefn -> (SLR.TermDefn, ErrorMonad ())
			promise allTermDefns = let

				scope1 = scopeForDefns (Defns allDataDefns allCtorDefns allTermDefns)
				paramTermsInScope = M.fromList [(name, NameTermInScope (runNameForTermParamName name) termParamType')
					| ((name, _), termParamType') <- zip termParams termParamTypes']
				scope2 = scope1 {
					typesInScope = typeScope,
					termsInScope = paramTermsInScope `M.union` termsInScope scope1
					}

				valueOrError = compileTerm scope2 value

				makeSubbed object typeParamValues =
					let subs = M.fromList [
								(runNameForTypeParamName n, SLR.simpleTypeSub v)
								| ((n, _), v) <- zip typeParams typeParamValues]
							in runIdentity (SLR.substituteType subs object)

				wholeType = foldr SLR.TypeFun type_' termParamTypes'

				termDefn = SLR.TermDefn {
					SLR.nameOfTermDefn = SLR.NameOfTerm (SLS.unNameOfTerm termName),
					SLR.typeParamsOfTermDefn = typeParamKinds',
					SLR.typeOfTermDefn = (\ typeParamValues ->
						let subs = M.fromList [
								(runNameForTypeParamName n, SLR.simpleTypeSub v)
								| ((n, _), v) <- zip typeParams typeParamValues]
						in runIdentity (SLR.substituteType subs wholeType)
						),
					SLR.valueOfTermDefn = (\ typeParamValues ->
						let
							subs = M.fromList [
								(runNameForTypeParamName n, SLR.simpleTypeSub v)
								| ((n, _), v) <- zip typeParams typeParamValues]
							realValue = case valueOrError of Success v -> v
						in runIdentity (SLR.substituteTerm (subs, M.empty) realValue)
						)
					}

				in (termDefn, valueOrError >> return ())

		return (termName, promise)

		| SLS.DirLet _ termName typeParams termParams type_ value <- directives]

	let
		allTermDefns :: M.Map SLS.NameOfTerm SLR.TermDefn
		allTermDefns = M.map (fst . ($ allTermDefns)) termPromises
	sequence [snd (promise allTermDefns) | promise <- M.elems termPromises]

	return $ Defns allDataDefns allCtorDefns allTermDefns

