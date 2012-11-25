module MetaCompiler.TLRuntime where

-- `RMT` stands for `reduced meta-type`. It's an internal de-sugared
-- representation of meta-types.

data RMT
	= RMTJSType
	| RMTJSTerm RMO Bool
	| RMTFun (String, RMT) RMT

-- `RMO` stands for `reduced meta-object`. Note that it's not necessarily in
-- weak head normal form; it may have unbound variables in it.

data RMO
	= RMOVar RMT String
	| RMOJSRepr String [RMO]
	| RMOJSTerm RMO (Maybe SL.Term) (JSUtils.RenameSymbols (JS.Expression ()))
	| RMOFun (String, RMT) RMT (RMO -> RMO)

-- `equivalentRMO` returns `True` if the two `RMO`s given are guaranteed to be
-- equal as long as all of the pairs of variables in the first parameter are
-- equal.

equivalentRMO :: S.Set (String, String) -> RMO -> RMO -> Bool
equivalentRMO vars (RMOJSTerm _) _ = error "not comparable"
equivalentRMO vars _ (RMOJSTerm _) = error "not comparable"
equivalentRMO vars (RMOFun _) _ = error "not comparable"
equivalentRMO vars _ (RMOFun _) = error "not comparable"
equivalentRMO vars (RMOVar vty1 var1) (RMOVar vty2 var2) =
	(var1, var2) `S.member` vars || var1 == var2 && all (\ (v1, v2) -> v1 /= var1 && v2 /= var2) vars
equivalentRMO vars (RMOJSRepr name1 params1) (RMOJSRepr name2 params2) =
	name1 == name2 &&
	and (zipWith (equivalentRMO vars) params1 params2)
equivalentRMO vars _ _ = False

-- `canCastRMT` returns `True` if it's safe to pass a value of the first `RMT`
-- where a value of the second `RMT` is expected as long as all of the pairs of
-- variables in the first parameter are equal.

canCastRMT :: S.Set (String, String) -> RMT -> RMT -> Bool
canCastRMT vars (RMTJSType) (RMTJSType) = True
canCastRMT vars (RMTJSTerm ty1 hasSL1) (RMTJSTerm ty2 hasSL2) =
	equivalentRMO vars ty1 ty2 &&
	(hasSL1 || not hasSL2)
canCastRMT vars (RMTFun (var1, arg1) ret1) (RMTFun (var2, arg2) ret2) =
	canCastRMT vars arg2 arg1 &&
	canCastRMT (S.insert (var1, var2) $ S.filter (\ (v1, v2) -> v1 /= var1 && v2 /= var2) $ vars) ret1 ret2
canCastRMT vars _ _ = False

-- `typeOfRMO` returns the `RMT` representing the type of the given `RMO`

typeOfRMO :: RMO -> RMT
typeOfRMO (RMOVar ty _) = ty
typeOfRMO (RMOJSRepr _ _) = RMTJSType
typeOfRMO (RMOJSTerm ty (Just _) _) = RMTJSTerm ty True
typeOfRMO (RMOJSTerm ty Nothing _) = RMTJSTerm ty False
typeOfRMO (RMOFun (name, argTy) retTy _) = RMTFun (name, argTy) retTy

-- `reduceMetaType` converts a `Syntax.MetaType Range` into a `RMT`. It will
-- also check for validity of variable references and correctness of types as
-- it traverses the AST.

reduceMetaType :: M.Map String RMO -> Syntax.MetaType Range -> Either String RMT

reduceMetaType vars (Syntax.MTJSType tag) = do
	return RMTJSType

reduceMetaType vars (Syntax.MTJSTerm tag ty) = do
	ty' <- reduceMetaTerm vars ty
	case typeOfRMO ty' of
		RMTJSType -> return ()
		_ -> Left ("type at " ++ formatRange (tagOfMetaObject ty) ++ " should \
			\have meta-type (js-type) but instead has meta-type " ++
			show (typeOfRMO ty'))
	return (RMTJSTerm ty')

reduceMetaType vars (Syntax.MTFun tag params final) = do
	let
		f :: M.Map String RMO -> [(String, Syntax.MetaType Range)] -> Either String RMT
		f vars' [] = do
			reduceMetaType vars' final
		f vars' ((paramName, paramMetaType):rest) = do
			paramRMT <- reduceType vars' paramMetaType
			let vars'' = M.insert paramName (RMOVar paramRMT paramName) vars'
			f vars'' rest
	f vars params

-- `reduceMetaObject` converts a `Syntax.MetaObject Range` into a `RMO`
-- and also checks for validity of variable references and correctness of
-- types.

reduceMetaObject :: M.Map String RMO -> Syntax.MetaObject Range -> Either String RMO

reduceMetaObject vars (Syntax.MOApp tag fun arg) = do
	funRMO <- reduceMetaObject vars fun
	(argName, expectedArgRMT, retRMT, funBody) <- case funRMO of
		RMOFun (n, t1) t2 b -> return (n, t1, t2, b)
		_ -> Left ("term at " ++ formatRange (tagOfMetaObject fun) ++ " is \
			\being applied like a function, but has type " ++
			show (typeOfRMO funRMO) ++ ".")
	argRMO <- reduceMetaObject vars arg
	unless (canCastRMT S.empty (typeOfRMO argRMO) expectedArgRMT) $
		Left ("argument at " ++ formatRange (tagOfMetaObject arg) ++ " has \
			\type " ++ show (typeOfRMO argRMO) ++ ", but function at " ++
			formatRange (tagOfMetaObject fun) ++ " expects its argument to \
			\have type " ++ show expectedArgRMT ++ ".")
	return (funBody argRMO)

reduceMetaObject vars (Syntax.MOAbs tag params result) = do
	let
		g :: M.Map String RMO -> [(String, Syntax.MetaType Range)] -> Either String ([RMT], RMT)
		g vars' [] = do
			res <- reduceMetaObject vars' result
			return ([], typeOfRMO res)
		g vars' ((paramName, paramType):params) = do
			paramRMT <- reduceMetaType vars' paramType
			let vars'' = M.insert paramName (RMOVar paramRMT paramName) vars'
			(paramRMTs, resultRMT) <- g vars'' params
			return (paramRMT:paramRMTs, resultRMT)
	(paramRMTs, resultRMT) <- g vars params
	let
		f :: M.Map String RMO -> [((String, Syntax.MetaType Range), RMT)] -> RMO
		f vars' [] = case reduceMetaObject vars' result of
			Left err -> error ("this should have been caught already: " ++ err)
			Right rmo -> rmo
		f vars' (((paramName, _), paramRMT):params) = let
			restRMT = foldr (\ ((pn, _), pt) rt -> RMTFun (pn, pt) rt) resultRMT params
			in RMOFun (paramName, paramRMT) restRMT (\param -> f (M.insert paramName param) params)
	return $ f vars (zip params paramRMTs)

reduceMetaObject vars (Syntax.MOVar tag var) = case M.lookup var vars of
	Just value -> return value
	Nothing -> Left ("variable \"" ++ var ++ "\" not in scope at " ++ formatRange tag)

reduceMetaObject vars (Syntax.MOJSExpr tag code type_ spec subs) = do
	typeRMO <- reduceMetaObject vars type_
	subs' <- sequence [do
		value' <- reduceMetaObject vars value
		equiv <- case value' of
			RMOJSTerm _ _ equiv -> return equiv
			_ -> Left ("in (js-expr ...) block at " ++ formatRange tag ++ ": \
				\value for variable " ++ name ++ " at " ++
				formatRange (tagOfMetaObject value) ++ " should have type \
				\(js-term ...) but instead had type " ++
				show (typeOfRMO value') ++ ".")
		return (name, equiv)
		| (name, value) <- subs]
	let jsEquivalent = do
		subs'' <- liftM M.fromList $ sequence [do
			js <- equiv
			js' <- JSUtils.revariableExpression M.empty js
			return (name, js')
			| (name, equiv) <- subs']
		JSUtils.revariableExpression subs'' (JS.removeAnnotations code)
	return (RMOJSTerm typeRMO spec jsEquivalent)

-- `processDirective` processes a single TL directive. As input, it takes the
-- map of globally defined meta-objects before the directive. As output, it
-- returns the map of globally defined meta-objects after the directive.

processDirective :: M.Map String RMO -> Directive Range -> Either String (M.Map String RMO)
processDirective vars directive@(DLet { }) =
	errorContext ("in (let ...) directive at " ++ formatRange (tagOfDirective directive)) $ do
		resultType <- computeMetaType vars (valueOfDirective directive)
		let
			checkParams :: M.Map String (NormedMetaType, ReducedMetaObject) -> 
		let
			applyParams vars' [] =
				reduce vars' (valueOfDirective directive)
			applyParams vars' ((n, t):rest) = RMOFun $ \ value ->
				applyParams (safeInsert n value vars') rest
		let newValue = applyParams (M.map snd vars) (paramsOfDirective directive)
		return $ safeInsert (nameOfDirective directive) (type_, newValue) vars
processDirective vars directive@(DJSRepr { }) =
	errorContext ("in (js-repr ...) directive at " ++ formatRange (tagOfDirective directive)) $ do
		let
			checkParams :: M.Map String (NormedMetaType, ReducedMetaObject) -> [(String, MetaType Range)] -> Either String ()
			checkParams vars' [] = return ()
			checkParams vars' ((n, t):rest) = do
				shouldBeJSType <- computeMetaType vars' t
				unless (shouldBeJSType == NMTJSType) $
					Left ("It's illegal to parameterize a (js-repr ...) on \
						\anything but a (js-type).")
				-- TODO: this will crash if two parameters have the same name; it should
				-- error or work correctly instead
				checkParams (safeInsert n (NMTJSType, RMOJSType (JSTypePlaceholder n)) vars') rest
		checkParams vars (paramsOfDirective directive)
		let
			applyParams :: [JSType] -> [(String, MetaType Range)] -> ReducedMetaObject
			applyParams paramValues [] =
				RMOJSType (JSType {
					nameOfJSType = (nameOfDirective directive),
					paramsOfJSType = paramValues
					})
			applyParams paramValues ((n, t):rest) =
				RMOFun $ \ (RMOJSType value) -> applyParams (paramValues ++ [value]) rest
		let newValue = applyParams (M.map snd vars) (paramsOfDirective directive)
		return $ safeInsert (nameOfDirective directive) (type_, newValue) vars

