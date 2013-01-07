module Metacompiler.TLCompile where

import Metacompiler.Runtime
import Metacompiler.TLSyntax

data LocalState = LocalState {
{-
	nameSupplyOfLocalState :: [String],
	seenGlobalsOfLocalState :: M.Map JSGlobalUniqueId SeenGlobal,
	globalsToProcessOfLocalState :: [M.Map TLS.Name R.MetaObject -> StateT GlobalState (Either String) ()]
-}
	}

data InScope
	= InScopeGlobalPresent R.MetaObject
	| InScopeGlobalFuture
	| InScopeLocal R.Name R.MetaType
type Scope = M.Map TLS.Name InScope

compileMetaType :: Scope -> TLS.MetaType Range -> StateT LocalState (Either String) R.MetaType
compileMetaType scope (TLS.MTFun range params result) =
	compileAbstraction scope params (\scope' -> compileMetaType scope' result) R.MTFun
compileMetaType scope (TLS.MTSLType range slKind) = do
	slKind' <- lift $ SLC.compileKind slKind
	return (R.MTSLType slKind')
compileMetaType scope (TLS.MTSLTerm range slType) = do
	slType' <- compileMetaObject scope slType
	checkType slType' R.MTSLType
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
compileMetaObject scope (TLS.MOName range name) = case M.lookup scope name' of
	Just (InScopeGlobalPresent x) -> return x
	Just InScopeGlobalFuture -> error "top-sort should have prevented this"
	Just (InScopeLocal type_) -> return (R.MOName name' type_)
	where name' = R.Name (TLS.fromName name)
compileMetaObject scope (TLS.MOSLTypeLiteral range code typeBindings) = do
	typeBindings' <- compileSLTypeBindings scope typeBindings
	lift (SLC.compileSLType typeBindings' code)
compileMetaObject scope (TLS.MOSLTermLiteral range code typeBindings termBindings) = do
	typeBindings' <- compileSLTypeBindings scope typeBindings
	termBindings' <- compileSLTermBindings scope termBindings
	lift (SLC.compileSLType typeBindings' termBindings' code)
{-
compileMetaObject scope (TLS.MOJSEquivExprLiteral range slTerm jsType jsExpr) = do
	...
-}

{-
compileDirectives :: [TLS.Directive Range] -> StateT GlobalState (Either String) ()
...
-}

compileAbstraction :: Scope
                   -> [(TLS.Name, TLS.MetaType Range)]
                   -> (Scope -> StateT LocalState (Either String) a)
                   -> ((R.Name, R.Metatype) -> a -> a)
                   -> StateT LocalState (Either String) a
compileAbstraction scope [] base fun = base scope
compileAbstraction scope ((paramName, paramType):params) base fun = do
	let paramName' = R.Name (TLS.unName paramName)
	paramType' <- compileMetaType scope paramType
	let scope' = M.insert paramName' (InScopeLocal paramType') scope
	rest <- compileAbstraction scope' params base fun
	return (fun (paramName', paramType') rest)

compileBindings :: Scope
                -> (Scope -> TLS.ParamBinding Range -> StateT LocalState (Either String) (Scope, p))
                -> (Scope -> [p] -> TLS.MetaObject Range -> StateT LocalState (Either String) a)
                -> [TLS.Binding Range n]
                -> StateT LocalState (Either String) (M.Map n a)
compileBindings scope nameFun paramFun valueFun bindings = do
	bindings' <- sequence [do
		let
			f scope' [] params' =
				valueFun scope' params' (TLS.valueOfBinding binding)
			f scope' (param:params) paramsSoFar' = do
				(scope'', param') <- paramFun scope' param
				f scope'' params (paramsSoFar' ++ [param'])
		value' <- f scope (TLS.paramsOfBinding binding) []
		return (name, value')
		| binding <- bindings]
	-- TODO: Check for duplicates
	return (M.fromList bindings')

compileSLTypeBindings :: Scope
                      -> [TLS.Binding Range SLS.NameOfType]
                      -> StateT LocalState (Either String) (M.Map SLS.NameOfType SLC.TypeInScope)
compileSLTypeBindings scope bindings = compileBindings scope bindings
	(\ subScope (TLS.BindingParam parts) -> do
		(name, type_) <- case parts of
			[(name, type_)] -> return (name, type_)
			_ -> lift (Left ("Parameters to bindings in a `(sl-type ...)` construct should all have exactly one \
				\part.")
		let name' = R.Name (TLS.unName name)
		type_' <- compileMetaType scope type_
		slKind' <- case type_' of
			MTSLType slKind' -> return slKind'
			_ -> lift (Left "Parameters to bindings in a `(sl-type ...)` construct should all have type \
				\`(sl-type ...)`.")
		let subScope' = M.insert name' (InScopeLocal type_') subScope
		return (subScope', (name', slKind'))
		)
	(\ subScope' params' value -> do
		value' <- compileMetaObject scope' value
		checkTypeSLType value'
		return (SLC.TypeInScope {
			SLC.typeParamsOfTypeInScope = map snd params',
			SLC.valueOfTypeInScope = \ paramValues ->
				R.substituteMetaObject
					(M.fromList (zip (map fst params') paramValues))
					value'
			})
		)

compileSLTermBindings :: Scope
                      -> [TLS.Binding Range SLS.NameOfTerm]
                      -> StateT LocalState (Either String) (M.Map SLS.NameOfTerm SLC.TermInScope)
compileSLTermBindings scope bindings = compileBindings scope bindings
	(\ subScope (TLS.BindingParam parts) -> do
		(name, type_) <- case parts of
			[(name, type_)] -> return (name, type_)
			_ -> lift (Left "Parameters to bindings in a `(sl-term ...)` construct should all have exactly one \
				\part.")
		let name' = SLR.Name (SLS.unName name)
		type_' <- compileMetaType scope type_
		slKindOrType' <- case type_' of
			MTSLType slKind' -> return (Left slKind')
			MTSLTerm slType' -> return (Right slType')
			_ -> lift (Left "Parameters to bindings in a `(sl-term ...)` construct should all have type \
				\`(sl-type ...)` or `(sl-term ...)`.")
		let subScope' = M.insert name' (InScopeLocal type_') subScope
		return (subScope', (name', slKindOrType'))
		)
	(\ subScope' params' value -> do
		when (any (isLeft . snd) $ dropWhile (isLeft . snd) params') $
			lift (Left "In a `(sl-term ...)` construct, all type-parameters must come before all term-parameters.")
		let typeParams' = [(name, slKind') | (name, Left slKind') <- params']
		let termParams' = [(name, slType') | (name, Right slType') <- params']
		value' <- compileMetaObject scope' value
		checkTypeSLTerm value'
		return (SLC.TermInScope {
			SLC.typeParamsOfTermInScope = map snd typeParams',
			SLC.termParamsOfTermInScope = map snd termParams',
			SLC.valueOfTermInScope = \ typeParamValues termParamValues ->
				R.substituteMetaObject
					(M.fromList (zip (map fst typeParams') typeParamValues ++ zip (map fst termParams') termParamValues))
					value'
			})
		)

checkType :: MetaObject -> MetaType -> StateT LocalState (Either String) ()
checkType obj expectedType = if reduceMetaType (typeOfMetaObject obj) `equivalentMetaType` reduceMetaType expectedType
	then return ()
	else lift (Left "expected type `<not implemented>`, got something else")

checkTypeSLType :: MetaObject -> StateT LocalState (Either String) SLKind
checkTypeSLType obj = case reduceMetaType (typeOfMetaObject obj) of
	MTSLType slKind -> return slKind
	_ -> lift (Left "expected type `(sl-type ...)`, got something else")

checkTypeSLTerm :: MetaObject -> StateT LocalState (Either String) MetaObject
checkTypeSLTerm obj = case reduceMetaType (typeOfMetaObject obj) of
	MTSLTerm slType -> return slType
	_ -> lift (Left "expected type `(sl-term ...)`, got something else")

