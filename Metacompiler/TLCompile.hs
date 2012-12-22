module Metacompiler.TLCompile where

data LocalState = LocalState {
	nameSupplyOfLocalState :: [String],
	seenGlobalsOfLocalState :: M.Map JSGlobalUniqueId SeenGlobal,
	globalsToProcessOfLocalState :: [M.Map TLR.Var TLR.MetaObject -> StateT GlobalState (Either String) ()]
	}

data ScopeVar
	= ScopeVarGlobalPresent TLR.MetaObject
	| ScopeVarGlobalFuture
	| ScopeVarLocal TLR.MetaType

compileMetaType :: M.Map TLR.Var ScopeVar -> TLS.MetaType Range -> StateT LocalState (Either String) TLR.MetaType
compileMetaType scope (TLS.MTFun range params result) =
	compileAbstraction scope params (\scope' -> compileMetaType scope' result) TLR.MTFun
compileMetaType scope (TLS.MTSLType range) = do
	return TLR.MTSLType
compileMetaType scope (TLS.MTSLTerm range slType) = do
	slType' <- compileMetaObject scope slType
	checkType slType' TLR.MTSLType
	return (TLR.MTSLTerm slType')
compileMetaType scope (TLS.MTJSExpr range) = do
	return TLR.MTJSExpr
compileMetaType scope (TLS.MTJSStatement range) = do
	return TLR.MTJSStatement
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
			\the JavaScript type. This is false and/or difficult to prove.")
	return (TLR.MTJSEquivExpr slTerm' jsEquivExprType')

compileMetaObject :: M.Map TLR.Var ScopeVar -> TLS.MetaObject Range -> StateT LocalState (Either String) TLR.MetaObject
compileMetaObject scope (TLS.MOApp range fun arg) = do
	fun' <- compileMetaObject scope fun
	arg' <- compileMetaObject scope arg
	((_, paramType), _) <- checkTypeFun fun'
	checkType arg' paramType
	return (TLR.MOApp fun' arg')
compileMetaObject scope (TLS.MOAbs range params result) =
	compileAbstraction scope params (\scope' -> compileMetaObject scope' result) TLR.MOAbs
compileMetaObject scope (TLS.MOVar range name) = case M.lookup scope name' of
	Just (ScopeVarGlobalPresent x) -> return x
	Just ScopeVarGlobalFuture -> error "top-sort should have prevented this"
	Just (ScopeVarLocal type_) -> return (TLR.MOVar name' type_)
	where name' = TLR.Var (TLS.fromVar name)
compileMetaObject scope (TLS.
compileMetaObject scope (TLS.MOJSEquivExprLiteral range slTerm jsType jsExpr) = do
	return (TLR.MOJSExprLiteral 

compileDirectives :: [TLS.Directive Range] -> StateT GlobalState (Either String) ()
...

compileAbstraction :: M.Map TLR.Var ScopeVar
                   -> [(TLS.Var, TLS.MetaType Range)]
                   -> (M.Map TLR.Var ScopeVar -> StateT LocalState (Either String) a)
                   -> ((TLR.Var, TLR.Metatype) -> a -> a)
                   -> StateT LocalState (Either String) a
compileAbstraction scope [] base fun = base scope
compileAbstraction scope ((paramName, paramType):params) base fun = do
	let paramName' = TLR.Var (TLS.unVar paramName)
	paramType' <- compileMetaType scope paramType
	let scope' = M.insert paramName' (ScopeVarLocal paramType') scope
	rest <- compileAbstraction scope' params base fun
	return (fun (paramName', paramType') rest)

