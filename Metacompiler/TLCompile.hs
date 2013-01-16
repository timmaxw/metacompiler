module Metacompiler.TLCompile where

import Control.Monad.State
import qualified Data.Graph
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Metacompiler.Runtime as R
import Metacompiler.SExpr (Range, formatRange)
import qualified Metacompiler.SLCompile as SLC
import qualified Metacompiler.SLSyntax as SLS
import qualified Metacompiler.TLSyntax as TLS

data LocalState = LocalState {
{-
	nameSupplyOfLocalState :: [String],
	seenGlobalsOfLocalState :: M.Map JSGlobalUniqueId SeenGlobal,
	globalsToProcessOfLocalState :: [M.Map TLS.Name R.MetaObject -> StateT GlobalState (Either String) ()]
-}
	}

data MetaObjectInScope
	= MetaObjectInScopeGlobalPresent R.MetaObject
	| MetaObjectInScopeGlobalFuture
	| MetaObjectInScopeLocal R.Name R.MetaType

data Scope = Scope {
	metaObjectsInScope :: M.Map TLS.Name MetaObjectInScope,
	slObjectsInScope :: SLC.Scope
	}

compileMetaType :: Scope -> TLS.MetaType Range -> StateT LocalState (Either String) R.MetaType
compileMetaType scope (TLS.MTFun range params result) =
	compileAbstraction scope params (\scope' -> compileMetaType scope' result) R.MTFun
compileMetaType scope (TLS.MTSLType range slKind) = do
	slKind' <- lift $ SLC.compileSLKind slKind
	return (R.MTSLType slKind')
compileMetaType scope (TLS.MTSLTerm range slType) = do
	slType' <- compileMetaObject scope slType
	checkType slType' (R.MTSLType R.SLKindType)
	return (R.MTSLTerm slType')
{-
compileMetaType scope (TLS.MTJSEquivExprType range slType) = do
	slType' <- compileMetaObject scope slType
	checkType slType' R.MTSLType
	return (R.MTJSEquivExprType slEquiv')
compileMetaType scope (TLS.MTJSEquivExpr range slTerm jsEquivExprType) = do
	slTerm' <- compileMetaObject scope slTerm
	slType1 <- checkTypeSLExpr slTerm'
	jsEquivExprType' <- compileMetaObject scope jsEquivExprType
	slType2 <- checkTypeJSEquivExprType jsEquivExprType'
	case testSLTypesEqual slType1 slType2 of
		True -> return ()
		False -> Left ("at " ++ formatRange range ++ ": The type of the SL equivalent should be the SL equivalent of \
			\the JavaScript type.")
	return (R.MTJSEquivExpr slTerm' jsEquivExprType')
-}

compileMetaObject :: Scope -> TLS.MetaObject Range -> StateT LocalState (Either String) R.MetaObject
compileMetaObject scope (TLS.MOApp range fun arg) = do
	fun' <- compileMetaObject scope fun
	arg' <- compileMetaObject scope arg
	((_, paramType), _) <- checkTypeFun fun'
	checkType arg' paramType
	return (R.MOApp fun' arg')
compileMetaObject scope (TLS.MOAbs range params result) =
	compileAbstraction scope params (\scope' -> compileMetaObject scope' result) R.MOAbs
compileMetaObject scope (TLS.MOName range name) = case M.lookup name (metaObjectsInScope scope) of
	Just (MetaObjectInScopeGlobalPresent x) -> return x
	Just MetaObjectInScopeGlobalFuture -> error "top-sort should have prevented this"
	Just (MetaObjectInScopeLocal name' type_) -> return (R.MOName name' type_)
	Nothing -> lift $ Left ("at " ++ formatRange range ++ ": name `" ++ TLS.unName name ++ "` is not in scope")
compileMetaObject scope (TLS.MOSLTypeLiteral range code typeBindings) = do
	typeBindings' <- compileSLTypeBindings scope typeBindings
	lift (SLC.compileSLType
		(M.union typeBindings' (SLC.typesInScope $ slObjectsInScope $ scope))
		[] code
		)
compileMetaObject scope (TLS.MOSLTermLiteral range code typeBindings termBindings) = do
	typeBindings' <- compileSLTypeBindings scope typeBindings
	termBindings' <- compileSLTermBindings scope termBindings
	let slScope' = (slObjectsInScope scope) {
			SLC.typesInScope = typeBindings' `M.union` (SLC.typesInScope $ slObjectsInScope scope),
			SLC.termsInScope = termBindings' `M.union` (SLC.termsInScope $ slObjectsInScope scope)
			} 
	lift (SLC.compileSLTerm slScope' [] code)

{-
compileMetaObject scope (TLS.MOJSEquivExprLiteral range slTerm jsType jsExpr) = do
	...
-}

compileAbstraction :: Scope
                   -> [(TLS.Name, TLS.MetaType Range)]
                   -> (Scope -> StateT LocalState (Either String) a)
                   -> ((R.Name, R.MetaType) -> a -> a)
                   -> StateT LocalState (Either String) a
compileAbstraction scope [] base fun = base scope
compileAbstraction scope ((paramName, paramType):params) base fun = do
	let paramName' = R.Name (TLS.unName paramName)
	paramType' <- compileMetaType scope paramType
	let scope' = scope { metaObjectsInScope = M.insert paramName (MetaObjectInScopeLocal paramName' paramType') (metaObjectsInScope scope) }
	rest <- compileAbstraction scope' params base fun
	return (fun (paramName', paramType') rest)

compileBindings :: Ord n
                => Scope
                -> (Scope -> TLS.BindingParam Range -> StateT LocalState (Either String) (Scope, p))
                -> (Scope -> [p] -> TLS.MetaObject Range -> StateT LocalState (Either String) a)
                -> [TLS.Binding Range n]
                -> StateT LocalState (Either String) (M.Map n a)
compileBindings scope paramFun valueFun bindings = do
	bindings' <- sequence [do
		let
			f scope' [] params' =
				valueFun scope' params' (TLS.valueOfBinding binding)
			f scope' (param:params) paramsSoFar' = do
				(scope'', param') <- paramFun scope' param
				f scope'' params (paramsSoFar' ++ [param'])
		value' <- f scope (TLS.paramsOfBinding binding) []
		return (TLS.nameOfBinding binding, value')
		| binding <- bindings]
	-- TODO: Check for duplicates
	return (M.fromList bindings')

compileSLTypeBindings :: Scope
                      -> [TLS.Binding Range SLS.NameOfType]
                      -> StateT LocalState (Either String) (M.Map SLS.NameOfType SLC.TypeInScope)
compileSLTypeBindings scope bindings = compileBindings scope
	(\ subScope (TLS.BindingParam parts) -> do
		(name, type_) <- case parts of
			[(name, type_)] -> return (name, type_)
			_ -> lift $ Left ("Parameters to bindings in a `(sl-type ...)` construct should all have exactly one \
				\part.")
		let name' = R.Name (TLS.unName name)
		type_' <- compileMetaType scope type_
		slKind' <- case type_' of
			R.MTSLType slKind' -> return slKind'
			_ -> lift (Left "Parameters to bindings in a `(sl-type ...)` construct should all have type \
				\`(sl-type ...)`.")
		let subScope' = subScope { metaObjectsInScope = M.insert name (MetaObjectInScopeLocal name' type_') (metaObjectsInScope subScope) }
		return (subScope', (name', slKind'))
		)
	(\ subScope' params' value -> do
		value' <- compileMetaObject subScope' value
		checkTypeSLType value'
		return (SLC.TypeInScope {
			SLC.typeParamsOfTypeInScope = map snd params',
			SLC.valueOfTypeInScope = \ paramValues -> let
				metaObjectSubs = M.fromList (zip (map fst params') paramValues)
				in R.substituteMetaObject (R.Substitutions metaObjectSubs M.empty M.empty) value'
			})
		)
	bindings

compileSLTermBindings :: Scope
                      -> [TLS.Binding Range SLS.NameOfTerm]
                      -> StateT LocalState (Either String) (M.Map SLS.NameOfTerm SLC.TermInScope)
compileSLTermBindings scope bindings = compileBindings scope
	(\ subScope (TLS.BindingParam parts) -> do
		(name, type_) <- case parts of
			[(name, type_)] -> return (name, type_)
			_ -> lift (Left "Parameters to bindings in a `(sl-term ...)` construct should all have exactly one \
				\part.")
		let name' = R.Name (TLS.unName name)
		type_' <- compileMetaType scope type_
		slKindOrType' <- case type_' of
			R.MTSLType slKind' -> return (Left slKind')
			R.MTSLTerm slType' -> return (Right slType')
			_ -> lift (Left "Parameters to bindings in a `(sl-term ...)` construct should all have type \
				\`(sl-type ...)` or `(sl-term ...)`.")
		let subScope' = subScope { metaObjectsInScope =	M.insert name (MetaObjectInScopeLocal name' type_') (metaObjectsInScope subScope) }
		return (subScope', (name', slKindOrType'))
		)
	(\ subScope' params' value -> do
		let
			isLeft :: Either l r -> Bool
			isLeft (Left _) = True
			isLeft (Right _) = False
		when (any (isLeft . snd) $ dropWhile (isLeft . snd) params') $
			lift (Left "In a `(sl-term ...)` construct, all type-parameters must come before all term-parameters.")
		let typeParams' = [(name, slKind') | (name, Left slKind') <- params']
		let termParams' = [(name, slType') | (name, Right slType') <- params']
		value' <- compileMetaObject subScope' value
		checkTypeSLTerm value'
		return (SLC.TermInScope {
			SLC.typeParamsOfTermInScope = map snd typeParams',
			SLC.termParamsOfTermInScope = map snd termParams',
			SLC.valueOfTermInScope = \ typeParamValues termParamValues -> let
				metaObjectSubs = M.fromList (zip (map fst typeParams') typeParamValues ++ zip (map fst termParams') termParamValues)
				in R.substituteMetaObject (R.Substitutions metaObjectSubs M.empty M.empty) value'
			})
		)
	bindings

checkType :: R.MetaObject -> R.MetaType -> StateT LocalState (Either String) ()
checkType obj expectedType = if R.reduceMetaType (R.typeOfMetaObject obj) `R.equivalentMetaTypes` R.reduceMetaType expectedType
	then return ()
	else lift (Left "expected type `<not implemented>`, got something else")

checkTypeFun :: R.MetaObject -> StateT LocalState (Either String) ((R.Name, R.MetaType), R.MetaType)
checkTypeFun obj = case R.reduceMetaType (R.typeOfMetaObject obj) of
	R.MTFun (paramName, paramType) returnType -> return ((paramName, paramType), returnType)
	_ -> lift $ Left ("expected type `(fun ... -> ...)`, got something else")

checkTypeSLType :: R.MetaObject -> StateT LocalState (Either String) R.SLKind
checkTypeSLType obj = case R.reduceMetaType (R.typeOfMetaObject obj) of
	R.MTSLType slKind -> return slKind
	_ -> lift (Left "expected type `(sl-type ...)`, got something else")

checkTypeSLTerm :: R.MetaObject -> StateT LocalState (Either String) R.MetaObject
checkTypeSLTerm obj = case R.reduceMetaType (R.typeOfMetaObject obj) of
	R.MTSLTerm slType -> return slType
	_ -> lift (Left "expected type `(sl-term ...)`, got something else")

data GlobalResults = GlobalResults {
	scopeOfGlobalResults :: Scope,
	slTermValuesOfGlobalResults :: M.Map R.NameOfSLTerm R.MetaObject
	}

compileDirectives :: [TLS.Directive Range] -> Either String GlobalResults
compileDirectives directives = do
	let allSLDirs = concat [content | TLS.DSLCode _ content <- directives]
	(slScope, slTermValueScope) <- SLC.compileSLGlobals allSLDirs

	let
		freeNamesInLet :: TLS.Directive Range -> S.Set TLS.Name
		freeNamesInLet (TLS.DLet _ name params type_ value) = f params
			where
				f :: [(TLS.Name, TLS.MetaType Range)] -> S.Set TLS.Name
				f [] = maybe S.empty TLS.freeNamesInMetaType type_
					`S.union` TLS.freeNamesInMetaObject value
				f ((paramName, paramType):params) =
					TLS.freeNamesInMetaType paramType
					`S.union` S.delete paramName (f params)
	sortedLets <- sequence [case scc of
		Data.Graph.AcyclicSCC dlet -> return dlet
		Data.Graph.CyclicSCC _ -> Left ("mutual recursion detected")
		| scc <- Data.Graph.stronglyConnComp
			[(dlet, TLS.nameOfDLet dlet, S.toList (freeNamesInLet dlet))
			| dlet@(TLS.DLet { }) <- directives]
		]

	metaObjectScope <- foldM (\ metaObjectScope dlet -> do
		metaObject <- liftM fst $ flip runStateT LocalState $ compileAbstraction
			(Scope metaObjectScope slScope)
			(TLS.paramsOfDLet dlet)
			(\ scope -> do
				value' <- compileMetaObject scope (TLS.valueOfDLet dlet)
				case TLS.typeOfDLet dlet of
					Nothing -> return ()
					Just type_ -> do
						type_' <- compileMetaType scope type_
						checkType value' type_'
				return value'
			)
			R.MOAbs
		return (M.insert (TLS.nameOfDLet dlet) (MetaObjectInScopeGlobalPresent metaObject) metaObjectScope)
		) M.empty sortedLets

	return $ GlobalResults {
		scopeOfGlobalResults = Scope metaObjectScope slScope,
		slTermValuesOfGlobalResults = slTermValueScope
		}

