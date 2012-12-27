module Metacompiler.TLCompile where

data LocalState = LocalState {
	nameSupplyOfLocalState :: [String],
	seenGlobalsOfLocalState :: M.Map JSGlobalUniqueId SeenGlobal,
	globalsToProcessOfLocalState :: [M.Map TLS.Name TLR.MetaObject -> StateT GlobalState (Either String) ()]
	}

data InScope
	= InScopeGlobalPresent TLR.MetaObject
	| InScopeGlobalFuture
	| InScopeLocal TLR.Name TLR.MetaType
type Scope = M.Map TLS.Name InScope

compileMetaType :: Scope -> TLS.MetaType Range -> StateT LocalState (Either String) TLR.MetaType
compileMetaType scope (TLS.MTFun range params result) =
	compileAbstraction scope params (\scope' -> compileMetaType scope' result) TLR.MTFun
compileMetaType scope (TLS.MTSLType range slKind) = do
	slKind' <- lift $ SLC.compileKind slKind
	return (TLR.MTSLType slKind')
compileMetaType scope (TLS.MTSLTerm range slType) = do
	slType' <- compileMetaObject scope slType
	checkType slType' TLR.MTSLType
	return (TLR.MTSLTerm slType')
compileMetaType scope (TLS.MTJSEquivExprType range slType) = do
	slType' <- compileMetaObject scope slType
	checkType slType' TLR.MTSLType
	return (TLR.MTJSEquivExprType slEquiv')
compileMetaType scope (TLS.MTJSEquivExpr range slTerm jsEquivExprType) = do
	slTerm' <- compileMetaObject scope slTerm
	slType1 <- checkTypeSLExpr slTerm'
	jsEquivExprType' <- compileMetaObject scope jsEquivExprType
	slType2 <- checkTypeJSEquivExprType jsEquivExprType'
	case testSLTypesEqual slType1 slType2 of
		True -> return ()
		False -> Left ("at " ++ formatRange range ++ ": The type of the SL equivalent should be the SL equivalent of \
			\the JavaScript type.")
	return (TLR.MTJSEquivExpr slTerm' jsEquivExprType')

compileMetaObject :: Scope -> TLS.MetaObject Range -> StateT LocalState (Either String) TLR.MetaObject
compileMetaObject scope (TLS.MOApp range fun arg) = do
	fun' <- compileMetaObject scope fun
	arg' <- compileMetaObject scope arg
	((_, paramType), _) <- checkTypeFun fun'
	checkType arg' paramType
	return (TLR.MOApp fun' arg')
compileMetaObject scope (TLS.MOAbs range params result) =
	compileAbstraction scope params (\scope' -> compileMetaObject scope' result) TLR.MOAbs
compileMetaObject scope (TLS.MOName range name) = case M.lookup scope name' of
	Just (InScopeGlobalPresent x) -> return x
	Just InScopeGlobalFuture -> error "top-sort should have prevented this"
	Just (InScopeLocal type_) -> return (TLR.MOName name' type_)
	where name' = TLR.Name (TLS.fromName name)
compileMetaObject scope (TLS.MOSLTypeLiteral range code typeBindings) = do
	typeBindings' <- compileSLTypeBindings scope typeBindings
	let slScope = SLC.Scope {
		SLC.typesInScope = M.map
			(\ (BindingSLType params value) -> let
				TLR.MTSLType finalKind = TLR.typeOfMetaObject value
				paramKinds = map snd params
				wholeKind = foldr SLR.KindFun finalKind paramKinds
				in SLC.TypeInScope wholeKind (length params)
				)
			typeBindings',
		SLC.termsInScope = M.empty
		}
	code' <- lift (SL.compileType slScope code)
	return (TLR.MOSLTypeLiteral code' typeBindings')
compileMetaObject scope (TLS.MOSLTermLiteral range code typeBindings termBindings) = do
	typeBindings' <- compileSLTypeBindings scope typeBindings
	termBindings' <- compileSLTermBindings scope termBindings
	let slScope = SLC.Scope {
		SLC.typesInScope = M.map
			(\ (BindingSLType params value) -> let
				TLR.MTSLType finalKind = TLR.typeOfMetaObject value
				paramKinds = map snd params
				wholeKind = foldr SLR.KindFun finalKind paramKinds
				in SLC.TypeInScope wholeKind (length params)
				)
			typeBindings',
		SLC.termsInScope = M.empty
		}
	code' <- lift (SL.compileType slScope code)
	return (TLR.MOSLTypeLiteral code' typeBindings')
		
compileMetaObject scope (TLS.MOJSEquivExprLiteral range slTerm jsType jsExpr) = do
	return (TLR.MOJSExprLiteral 

compileDirectives :: [TLS.Directive Range] -> StateT GlobalState (Either String) ()
...

compileAbstraction :: Scope
                   -> [(TLS.Name, TLS.MetaType Range)]
                   -> (Scope -> StateT LocalState (Either String) a)
                   -> ((TLR.Name, TLR.Metatype) -> a -> a)
                   -> StateT LocalState (Either String) a
compileAbstraction scope [] base fun = base scope
compileAbstraction scope ((paramName, paramType):params) base fun = do
	let paramName' = TLR.Name (TLS.unName paramName)
	paramType' <- compileMetaType scope paramType
	let scope' = M.insert paramName' (InScopeLocal paramType') scope
	rest <- compileAbstraction scope' params base fun
	return (fun (paramName', paramType') rest)

compileBindings :: Scope
                -> (n -> n')
                -> (Scope -> TLS.ParamBinding Range -> StateT LocalState (Either String) (Scope, p))
                -> (Scope -> [p] -> TLS.MetaObject Range -> StateT LocalState (Either String) a)
                -> [TLS.Binding Range n]
                -> StateT LocalState (Either String) (M.Map n' a)
compileBindings scope nameFun paramFun valueFun bindings = do
	bindings' <- sequence [do
		let name' = nameFun (TLS.nameOfBinding binding)
		let
			f scope' [] params' =
				valueFun scope' params' (TLS.valueOfBinding binding)
			f scope' (param:params) paramsSoFar' = do
				(scope'', param') <- paramFun scope' param
				f scope'' params (paramsSoFar' ++ [param'])
		value' <- f scope (TLS.paramsOfBinding binding) []
		return (name', value')
		| binding <- bindings]
	-- TODO: Check for duplicates
	return (M.fromList bindings')

compileSLTypeBindings :: Scope
                      -> [TLS.Binding Range SLS.NameOfType]
                      -> StateT LocalState (Either String) (M.Map SLR.NameOfType SLR.BindingSLType)
compileSLTypeBindings scope bindings = compileBindings scope bindings
	(SLR.NameOfType . SLS.unNameOfType)
	(\ subScope (TLS.BindingParam parts) -> do
		(name, type_) <- case parts of
			[(name, type_)] -> return (name, type_)
			_ -> lift (Left ("Parameters to bindings in a `(sl-type ...)` construct should all have exactly one \
				\part.")
		let name' = SLR.Name (SLS.unName name)
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
		return (TLR.BindingSLType params' value')
		)

compileSLTermBindings :: Scope
                      -> [TLS.Binding Range SLS.NameOfTerm]
                      -> StateT LocalState (Either String) (M.Map SLR.NameOfTerm SLR.BindingSLTerm)
compileSLTermBindings scope bindings = compileBindings scope bindings
	(SLR.NameOfTerm . SLS.unNameOfTerm)
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
		return (TLR.BindingSLTerm params' value')
		)

