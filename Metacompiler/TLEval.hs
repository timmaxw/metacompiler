module Metacompiler.TLEval where

import qualified Data.Graph   -- for `stronglyConnComp`

-- `RecursiveVar` represents a variable in scope that may or may not be safe to
-- evaluate.

data RecursiveVar
	-- A global variable that is safe to evaluate
	= RecursiveVarGlobalPast RMO

	-- A global variable that comes later in the top-sort than whatever we're
	-- currently evaluating. Only appears when evaluating part of a group of
	-- `(let ...)` directives.
	| RecursiveVarGlobalFuture

	-- A local variable
	| RecursiveVarLocal RMO

-- `errorContextStateT` puts the given message on top of any error messages as
-- they bubble up.

errorContextStateT :: String -> StateT s (Either String) a -> StateT s (Either String) a
errorContextStateT msg action = StateT (\s1 -> case runStateT action s1 of
	Left msgs -> Left (msg ++ "\n" ++ msgs)
	Right (s2, a) -> Right (s2, a)
	)

-- `GlobalsToProcess` is threaded through all expression evaluations. It's used
-- to record instances of `(js-global ...)` for further processing.

data GlobalsToProcess = GlobalsToProcess {
	nameSupplyOfGlobalsToProcess :: [String],
	mapOfGlobalsToProcess :: M.Map JSGlobalUniqueId GlobalToProcess
	}

data GlobalToProcess = GlobalToProcess {
	syntaxOfGlobalToProcess :: TL.MetaObject Range,
	nameOfGlobalToProcess :: String,
	transferrableLocalVarsOfGlobalToProcess :: [(String, RMT)],
	untransferrableLocalVarsOfGlobalToProcess :: M.Map String RMT,
	globalVarsOfGlobalToProcess :: S.Set String
	}

-- `evalRecursiveMetaType` converts a `TL.MetaType Range` into a `RMT`. It will
-- also check for validity of variable references and correctness of types as
-- it traverses the AST. It's called "recursive" because it can be used in a
-- group of mutually recursive definitions, where some variables may be in
-- scope but their values not available yet.

evalRecursiveMetaType :: M.Map String RecursiveVar -> TL.MetaType Range -> StateT GlobalsToProcess (Either String) RMT

evalRecursiveMetaType vars (TL.MTJSType tag) = do
	return RMTJSType

evalRecursiveMetaType vars (TL.MTJSTerm tag hasSL ty) = do
	ty' <- evalMetaObject vars ty
	case typeOfRMO ty' of
		RMTJSType -> return ()
		_ -> lift $ Left ("type at " ++ formatRange (TL.tagOfMetaObject ty) ++ " should \
			\have meta-type (js-type) but instead has meta-type " ++
			formatRMT (typeOfRMO ty'))
	return (RMTJSTerm ty' hasSL)

evalRecursiveMetaType vars (TL.MTFun tag params final) = do
	let
		f :: M.Map String RMO -> [(String, TL.MetaType Range)] -> StateT GlobalsToProcess (Either String) RMT
		f vars' [] = do
			evalMetaType vars' final
		f vars' ((paramName, paramMetaType):rest) = do
			paramRMT <- evalMetaType vars' paramMetaType
			let vars'' = M.insert paramName (VarLocal (RMOUnknown paramRMT (Just paramName))) vars'
			resultRMT <- f vars'' rest
			return (RMTFun (paramName, paramRMT) resultRMT)
	f vars params

-- `evalRecursiveMetaObject` converts a `TL.MetaObject Range` into a `RMO` and
-- also checks for validity of variable references and correctness of types.
-- It's called "recursive" because it can be used in a group of mutually
-- recursive definitions, where some variables may be in scope but their values
-- not available yet.

evalRecursiveMetaObject :: M.Map String RecursiveVar -> TL.MetaObject Range -> StateT GlobalsToProcess (Either String) RMO

evalRecursiveMetaObject vars (TL.MOApp tag fun arg) =
	errorContextStateT ("in application at " ++ formatRange tag) $ do
		funRMO <- evalMetaObject vars fun
		case typeOfRMO funRMO of
			RMTFun (argName, expectedArgRMT) retRMT -> do
				argRMO <- evalMetaObject vars arg
				lift $ checkCanCastRMT
					("the argument at " ++ formatRange (TL.tagOfMetaObject arg) ++
						"to the function at " ++ formatRange (TL.tagOfMetaObject fun))
					expectedArgRMT
					(typeOfRMO argRMO)
				case funRMO of
					RMOFun _ _ funBody ->
						return (funBody argRMO)
					RMOUnknown _ _ ->
						return (RMOUnknown (substituteRMT (M.singleton argName argRMO) retRMT) Nothing)
					_ -> error "checkCanCastRMT should have caught this"
			_ -> lift $ Left ("Term at " ++ formatRange (TL.tagOfMetaObject fun) ++ " is \
						\being applied like a function, but has type " ++
						formatRMT (typeOfRMO funRMO) ++ ".")

evalRecursiveMetaObject vars (TL.MOAbs tag params result) =
	errorContextStateT ("in abstraction at " ++ formatRange tag) $ do
		makeAbstraction vars params (\vars' -> evalMetaObject vars' result)

evalRecursiveMetaObject vars (TL.MOVar tag var) = case M.lookup var vars of
	Nothing -> lift $ Left ("variable \"" ++ var ++ "\" not in scope at " ++ formatRange tag)
	Just (RecursiveVarGlobalPast value) -> return value
	Just RecursiveVarGlobalFuture -> error "top-sort should have detected this loop"
	Just (RecursiveVarLocal value) -> return value

evalRecursiveMetaObject vars (TL.MOJSExpr tag code type_ spec subs) =
	errorContextStateT ("in (js-expr ...) at " ++ formatRange tag) $ do
		typeRMO <- evalMetaObject vars type_
		subs' <- sequence [errorContextStateT ("in substitution for variable " ++ show name) $ do
			value' <- evalMetaObject vars value
			lift $ checkCanCastRMTToJSTerm "the new value" False (typeOfRMO value')
			case value' of
				RMOJSTerm _ _ equiv ->
					return (Just (name, equiv))
				RMOUnknown (RMTJSTerm _ _) _ ->
					return Nothing
				_ -> error "checkCanCastRMTToJSTerm should have caught this"
			| (name, value) <- subs]
		case sequence subs' of
			Just subs'' -> do
				let jsEquivalent = do
					subs''' <- liftM M.fromList $ sequence [do
						js <- equiv
						js' <- JSUtils.revariableExpression M.empty js
						return (name, js')
						| (name, equiv) <- subs'']
					JSUtils.revariableExpression subs''' (JS.removeAnnotations code)
				return (RMOJSTerm typeRMO spec jsEquivalent)
			Nothing -> do
				return (RMOUnknown (RMTJSTerm typeRMO (Data.Maybe.isJust spec)) Nothing)

evalRecursiveMetaObject vars (TL.MOJSGlobal tag uniqueId content type_ spec) = do
	GlobalsToProcess oldNameSupply oldMap <- get
	gtp <- case M.lookup uniqueId oldMap of
		Just gtp ->
			return gtp
		Nothing -> do
			let name:newNameSupply = oldNameSupply
			let gtp = GlobalToProcess {
				syntaxOfGlobalToProcess = content,
				nameOfGlobalToProcess = name,
				transferrableLocalVarsOfGlobalToProcess =
					[(name, typeOfRMO rmo)
					| (name, RecursiveVarLocal rmo) <- M.toList vars,
					case typeOfRMO rmo of { RMTJSTerm _ _ -> True; _ -> False }],
				untransferrableLocalVarsOfGlobalToProcess =
					[(name, typeOfRMO rmo)
					| (name, RecursiveVarLocal rmo) <- M.toList vars,
					case typeOfRMO rmo of { RMTJSTerm _ _ -> False; _ -> True }],
				globalVarsOfGlobalToProcess =
					S.fromList [name | (name, RecursiveVarGlobalFuture) <- M.toList vars] `S.union`
					S.fromList [name | (name, RecursiveVarGlobalPast _) <- M.toList vars]
				}
			let newMap = M.insert (nameOfGlobalToProcess gtp) gtp oldMap
			put (GlobalsToProcess newNameSupply newMap)
			return gtp
	let paramEquivs = [case (M.!) vars name of
		RMOJSTerm _ _ equiv -> Just equiv
		RMOUnknown _ _ -> Nothing
		| (name, _) <- transferrableLocalVarsOfGlobalToProcess gtp]
	typeRMO <- evalMetaObject vars type_
	return $ case sequence paramEquivs of
		Just equivs -> do
			let jsEquivalent = do
				params <- sequence equivs
				return (JS.CallExpr ()
					(JS.VarRef () (Id () (nameOfGlobalToProcess gtp)))
					params
					)
			return (RMOJSTerm typeRMO spec jsEquivalent)
		Nothing -> do
			return (RMOUnknown (RMTJSTerm typeRMO (Data.Maybe.isJust spec)) Nothing)

-- `evalDirectives` processes a group of TL directives. It takes as input a
-- mapping of variable names to values, and it returns a mapping of
-- newly-defined variables. Directives within the group can refer to one
-- another recursively.

evalDirectives :: M.Map String RMO
               -> [TL.Directive Range]
               -> StateT JSGlobals (Either String) (M.Map String RMO)
evalDirectives vars directives = do
	let
		-- `findUsesInMetaObject` and `findUsesInMetaType` return lists of
		-- all of the names that the given metaobject refers to directly (i.e.
		-- not through `js-global`)
		findUsesInMetaObject :: TL.MetaObject Range -> S.Set String
		findUsesInMetaObject = ...

		findUsesInMetaType :: TL.MetaType Range -> S.Set String
		findUsesInMetaType = ...

		usesInAllLets :: M.Map String (S.Set String)
		usesInAllLets = M.fromList [let
			valueUses = findUsesInMetaObject value
			paramUses = S.unions (map (findUsesInMetaType . snd) 
			in (name, uses'')
			| TL.DLet tag name params type_ value <- directives]

	sequence [do
		| CyclicSCC namesInCycle <- DataGraph.stronglyConnectedComp
			[
		]
	


		newDefinitions :: M.Map String 

evalDirective (TL.DLet tag name params maybeType final) = do
	Results { definitionsInResults = vars } <- get

	valueRMO <- lift $ errorContext ("in (let ...) directive at " ++ formatRange tag) $ do
		makeAbstraction vars params $ \vars' -> do
			finalRMO <- evalMetaObject vars' final
			case maybeType of
				Nothing -> return ()
				Just type_ -> do
					rmt <- evalMetaType vars' type_
					lift $ checkCanCastRMT "the value (according to the type signature)" rmt (typeOfRMO finalRMO)
			return finalRMO

	makeDefinition name valueRMO

evalDirective (TL.DJSRepr tag name params spec) = do
	Results { definitionsInResults = vars } <- get

	valueRMO <- lift $ errorContext ("in (js-repr ...) directive at " ++ formatRange tag) $ do
		makeAbstraction vars params $ \vars' -> do
			return (RMOJSRepr name [(M.!) vars name | (name, _) <- params])

	makeDefinition name valueRMO

evalDirective (TL.DEmit tag code subs) = do
	Results { definitionsInResults = vars } <- get

	subs' <- lift $ errorContext ("in (emit ...) directive at " ++ formatRange tag) $ sequence [
		errorContext ("in substitution for variable " ++ name) $ do
			value' <- evalMetaObject vars value
			lift $ checkCanCastRMTToJSTerm "the new value" False (typeOfRMO value')
			let equiv = case value' of
				RMOJSTerm _ _ equiv -> equiv
				_ -> error "checkCanCastRMTToJSTerm should have caught this"
			return (name, equiv)
		| (name, value) <- subs]
	let jsEquivalent = do
		subs'' <- liftM M.fromList $ sequence [do
			js <- equiv
			js' <- JSUtils.revariableExpression M.empty js
			return (name, js')
			| (name, equiv) <- subs']
		JSUtils.revariableExpression subs'' (JS.removeAnnotations code)

	results <- get
	let (newEmittedCode, symbolRenamingState') = runState jsEquivalent (symbolRenamingStateAfterResults results)
	put (results {
		symbolRenamingStateAfterResults = symbolRenamingState',
		emittedCodeOfResults =
			emittedCodeOfResults results ++
			(if null (emittedCodeOfResults results) then "" else "\n\n") ++
			JS.renderExpression newEmittedCode
		})

-- `checkCanCastRMT` and `checkCanCastRMTToJSTerm` are convenience functions
-- for type-checking, to make sure that the error message for a wrong type is
-- the same everywhere.

checkCanCastRMT :: String -> RMT -> RMT -> Either String ()
checkCanCastRMT what expectedRMT actualRMT = case canCastRMT M.empty actualRMT expectedRMT of
	Provably True ->
		return ()
	Provably False ->
		Left (firstPart ++ " It is impossible to cast the latter to the former.")
	NotProvable ->
		Left (firstPart ++ " metacompiler cannot prove that it is possible to \
			\cast the latter to the former.")
	where
		firstPart = "The expected type of " ++ what ++ " is " ++
			formatRMT expectedRMT ++ ", but the actual type was " ++
			formatRMT actualRMT ++ "."

checkCanCastRMTToJSTerm :: String -> Bool -> RMT -> Either String ()
checkCanCastRMTToJSTerm what requireSL actualRMT = case actualRMT of
	RMTJSTerm _ hasSL | hasSL || not requireSL ->
		return ()
	otherType ->
		Left ("The expected type of " ++ what ++ " is " ++
			(if requireSL then "(js-sl-term ...)" else "(js-term ...)") ++
			", but the actual type was " ++ formatRMT actualRMT ++ ". It is \
			\impossible to cast the latter to the former.")

-- `makeAbstraction` is a common function used by anything that is
-- parameterized on a series of parameters.

makeAbstraction :: M.Map String RecursiveVar
                -> [(String, TL.MetaType Range)]
                -> (M.Map String RecursiveVar -> Either String RMO)
                -> StateT GlobalsToProcess (Either String) RMO
makeAbstraction vars params final = do
	let
		g :: M.Map String RecursiveVar -> [(String, TL.MetaType Range)] -> StateT GlobalsToProcess (Either String) ([RMT], RMT)
		g vars' [] = do
			res <- final vars'
			return ([], typeOfRMO res)
		g vars' ((paramName, paramType):params) = do
			paramRMT <- evalMetaType vars' paramType
			let vars'' = M.insert paramName (RecursiveVarLocal (RMOUnknown paramRMT (Just paramName))) vars'
			(paramRMTs, resultRMT) <- g vars'' params
			return (paramRMT:paramRMTs, resultRMT)
	(paramRMTs, resultRMT) <- g vars params
	gtp <- get
	let
		f :: M.Map String RecursiveVar -> [((String, TL.MetaType Range), RMT)] -> RMO
		f vars' [] = case runStateT (final vars') gtp of
			Left err -> error ("this should have been caught already: " ++ err)
			Right (_, rmo) -> rmo
		f vars' (((paramName, _), paramRMT):params) = let
			restRMT = foldr (\ ((pn, _), pt) rt -> RMTFun (pn, pt) rt) resultRMT params
			in RMOFun (paramName, paramRMT) restRMT (\param -> f (M.insert paramName param vars') params)
	return $ f vars (zip params paramRMTs)

