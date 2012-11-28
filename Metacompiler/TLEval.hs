module Metacompiler.TLEval where

-- `evalMetaType` converts a `TL.MetaType Range` into a `RMT`. It will
-- also check for validity of variable references and correctness of types as
-- it traverses the AST.

evalMetaType :: M.Map String RMO -> TL.MetaType Range -> Either String RMT

evalMetaType vars (TL.MTJSType tag) = do
	return RMTJSType

evalMetaType vars (TL.MTJSTerm tag hasSL ty) = do
	ty' <- evalMetaObject vars ty
	case typeOfRMO ty' of
		RMTJSType -> return ()
		_ -> Left ("type at " ++ formatRange (TL.tagOfMetaObject ty) ++ " should \
			\have meta-type (js-type) but instead has meta-type " ++
			formatRMT (typeOfRMO ty'))
	return (RMTJSTerm ty' hasSL)

evalMetaType vars (TL.MTFun tag params final) = do
	let
		f :: M.Map String RMO -> [(String, TL.MetaType Range)] -> Either String RMT
		f vars' [] = do
			evalMetaType vars' final
		f vars' ((paramName, paramMetaType):rest) = do
			paramRMT <- evalMetaType vars' paramMetaType
			let vars'' = M.insert paramName (RMOUnknown paramRMT (Just paramName)) vars'
			resultRMT <- f vars'' rest
			return (RMTFun (paramName, paramRMT) resultRMT)
	f vars params

-- `evalMetaObject` converts a `TL.MetaObject Range` into a `RMO`
-- and also checks for validity of variable references and correctness of
-- types.

evalMetaObject :: M.Map String RMO -> TL.MetaObject Range -> Either String RMO

evalMetaObject vars (TL.MOApp tag fun arg) =
	errorContext ("in application at " ++ formatRange tag) $ do
		funRMO <- evalMetaObject vars fun
		case typeOfRMO funRMO of
			RMTFun (argName, expectedArgRMT) retRMT -> do
				argRMO <- evalMetaObject vars arg
				checkCanCastRMT
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
			_ -> Left ("Term at " ++ formatRange (TL.tagOfMetaObject fun) ++ " is \
						\being applied like a function, but has type " ++
						formatRMT (typeOfRMO funRMO) ++ ".")

evalMetaObject vars (TL.MOAbs tag params result) =
	errorContext ("in abstraction at " ++ formatRange tag) $ do
		makeAbstraction vars params (\vars' -> evalMetaObject vars' result)

evalMetaObject vars (TL.MOVar tag var) = case M.lookup var vars of
	Just value -> return value
	Nothing -> Left ("variable \"" ++ var ++ "\" not in scope at " ++ formatRange tag)

evalMetaObject vars (TL.MOJSExpr tag code type_ spec subs) =
	errorContext ("in (js-expr ...) at " ++ formatRange tag) $ do
		typeRMO <- evalMetaObject vars type_
		subs' <- sequence [errorContext ("in substitution for variable " ++ show name) $ do
			value' <- evalMetaObject vars value
			checkCanCastRMTToJSTerm "the new value" False (typeOfRMO value')
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

-- `evalDirectives` processes a group of TL directives. It takes as input a
-- mapping of variable names to values, and it returns a mapping of
-- newly-defined variables. Directives within the group can refer to one
-- another recursively.

evalDirectives :: M.Map String RMO
               -> [TL.Directive Range]
               -> StateT JSGlobals (Either String) (M.Map String RMO)
evalDirectives directives = do
	

evalDirective (TL.DLet tag name params maybeType final) = do
	Results { definitionsInResults = vars } <- get

	valueRMO <- lift $ errorContext ("in (let ...) directive at " ++ formatRange tag) $ do
		makeAbstraction vars params $ \vars' -> do
			finalRMO <- evalMetaObject vars' final
			case maybeType of
				Nothing -> return ()
				Just type_ -> do
					rmt <- evalMetaType vars' type_
					checkCanCastRMT "the value (according to the type signature)" rmt (typeOfRMO finalRMO)
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
			checkCanCastRMTToJSTerm "the new value" False (typeOfRMO value')
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

makeAbstraction :: M.Map String RMO
                -> [(String, TL.MetaType Range)]
                -> (M.Map String RMO -> Either String RMO)
                -> Either String RMO
makeAbstraction vars params final = do
	let
		g :: M.Map String RMO -> [(String, TL.MetaType Range)] -> Either String ([RMT], RMT)
		g vars' [] = do
			res <- final vars'
			return ([], typeOfRMO res)
		g vars' ((paramName, paramType):params) = do
			paramRMT <- evalMetaType vars' paramType
			let vars'' = M.insert paramName (RMOUnknown paramRMT (Just paramName)) vars'
			(paramRMTs, resultRMT) <- g vars'' params
			return (paramRMT:paramRMTs, resultRMT)
	(paramRMTs, resultRMT) <- g vars params
	let
		f :: M.Map String RMO -> [((String, TL.MetaType Range), RMT)] -> RMO
		f vars' [] = case final vars' of
			Left err -> error ("this should have been caught already: " ++ err)
			Right rmo -> rmo
		f vars' (((paramName, _), paramRMT):params) = let
			restRMT = foldr (\ ((pn, _), pt) rt -> RMTFun (pn, pt) rt) resultRMT params
			in RMOFun (paramName, paramRMT) restRMT (\param -> f (M.insert paramName param vars') params)
	return $ f vars (zip params paramRMTs)

-- `makeDefinition` introduces a definition into the global scope

makeDefinition :: String -> RMO -> StateT Results (Either String) ()
makeDefinition name value = do
	results <- get
	case M.lookup name (definitionsInResults results) of
		Just _ -> lift (Left ("Name " ++ show name ++ " is already defined"))
		Nothing -> put (results {
			definitionsInResults = M.insert name value (definitionsInResults results)
			})

