module Metacompiler.TLEval2 where

{-

The general procedure for compiling is as follows:

 1. Top-sort the `(let ...)` and `(js-repr ...)` clauses according to which
    ones directly reference which others. If `a` directly references `b`, then
    `b` must be compiled before `a`. If there exist cycles, this is an error.
    Note that references within `(js-global ...)` are not direct references.

 2. "Proto-compile" each `(let ...)` and `(js-repr ...)`, in the order
    determined by the top-sort. Proto-compilation does everything except for
    processing `(js-global ...)` meta-objects. Variables that will be defined
    later in the top-sort will be noted as such. If a `(js-global ...)` is
    encountered, it will be recorded but not processed.

 3. Once all `(let ...)`s and `(js-repr ...)`s have been proto-compiled, an
    actual `RMO` can be found for every value in scope.

 4. Proto-compile the recorded `(js-global ...)`s, then any `(js-global ...)`s
    that were inside those, and so on until all `(js-global ...)`s have been
    processed.

 5. Finally, process any `(emit ...)` directives.
 
-}

-- `ProtoCompileVar` is used to keep track of variables during
-- proto-compilation. A global variable that came earlier in the top-sort will
-- be represented as `ProtoCompileVarPresent`, with its actual value present.
-- A global variable that comes later in the top-sort, or the global variable
-- that we're currently processing, will be represented as
-- `ProtoCompileVarFuture`. Local variables will be represented as
-- `ProtoCompileVarLocal`.

data ProtoCompileVar
	= ProtoCompileVarPresent RMO
	| ProtoCompileVarFuture
	| ProtoCompileVarLocal RMT

-- `ProtoCompileState` is used ot keep track of `(js-global ...)`s that were
-- encountered while proto-compiling.

data ProtoCompileState = ProtoCompileState {
	nameSupplyOfProtoCompileState :: [String],
	globalsOfProtoCompileState :: M.Map JSGlobalUniqueId ProtoCompileGlobal
	}

data ProtoCompileGlobal = ProtoCompileGlobal {
	nameOfProtoCompileGlobal :: String,
	syntaxOfProtoCompileGlobal :: MetaObject Range,
	transferrableLocalVarsOfProtoCompileGlobal :: [(String, RMT)],
	untransferrableLocalVarsOfProtoCompileGlobal :: M.Map String RMT,
	globalVarsOfProtoCompileGlobal :: S.Set String
	}

-- `protoCompileMetaType` proto-compiles the given meta-type. It takes a
-- `ProtoCompileVar` for each variable in scope, and uses this to make sure
-- that variables exist and to check types. It returns a function that will
-- return an actual `RMT` given actual values for all of the variables.

protoCompileMetaType :: M.Map String ProtoCompileVar
                     -> MetaType Range
                     -> StateT ProtoCompileState (Either String) (M.Map String RMO -> RMT)

protoCompileMetaType varTypes (MTJSType tag) =
	return (const RMTJSType)

protoCompileMetaType varTypes (MTJSTerm tag hasSL type_) =
	errorContextStateT ("in `(js-term ...)` clause at " ++ formatRange tag) $ do
		typeFun <- protoCompileMetaObject varTypes type_
		return (\varValues -> RMTJSTerm (typeFun varValues) hasSL)

protoCompileMetaType varTypes (MTFun tag params result) = let
	f :: M.Map String ProtoCompileVar -> [(String, MetaType Range)] -> Either String (M.Map String RMO -> RMT)
	f varTypes' [] =
		errorContextStateT ("in return type of `(fun ...)` at " ++ formatRange tag) $
		protoCompileMetaType varTypes' result
	f varTypes' ((paramName, paramType):rest) = do
		paramTypeFun <-
			errorContextStateT ("in type of parameter `" ++ paramName ++ "` \
				\to `(fun ...)` at " ++ formatRange tag) $
			protoCompileMetaType varTypes' paramType
		let paramTypeDummy = paramTypeFun (makeDummies varTypes')
		let varTypes'' = M.insert paramName (ProtoCompileVarLocal paramTypeDummy) 
		resultTypeFun <- f varTypes'' rest
		return (\varValues -> RMTFun (paramName, paramTypeFun varValues) (resultTypeFun varValues))
	in f varTypes params

-- `protoCompileMetaObject` is like `protoCompileMetaType` but for meta-objects
-- instead of meta-types.

protoCompileMetaObject :: M.Map String ProtoCompileVar
                  -> MetaObject Range
                  -> StateT ProtoCompileState (Either String) (M.Map String RMO -> RMO)

protoCompileMetaObject varTypes (MOApp tag fun arg) = do
	funFun <-
		errorContextStateT ("in function being called in application at " ++
			formatRange tag) $
		protoCompileMetaObject varTypes fun
	let funDummy = funFun (makeDummies varTypes)
	case typeOfRMO funDummy of
		RMTFun (paramName, paramType) resultType -> do
			argFun <-
				errorContextStateT ("in argument in application at " ++
					formatRange tag) $
				protoCompileMetaObject varTypes arg
			let argDummy = argFun (makeDummies varTypes)
			lift $ checkCanCastRMT
				("the argument at " ++ formatRange (TL.tagOfMetaObject arg) ++
					"to the function at " ++ formatRange (TL.tagOfMetaObject fun))
				paramType
				(typeOfRMO argDummy)
			return (\varValues -> case funFun varValues of
				RMOFun _ _ funBody ->
					return (funBody (argFun varValues))
				RMOUnknown _ _ ->
					return (RMOUnknown (substituteRMT (M.singleton paramName (argFun varValues)) resultType) Nothing)
				_ -> error "checkCanCastRMT should have caught this"
				)
		_ -> lift $ Left ("Term at " ++ formatRange (TL.tagOfMetaObject fun) ++ " is \
			\being applied like a function, but has type " ++
			formatRMT (typeOfRMO funDummy) ++ ".")

protoCompileMetaObject varTypes (MOAbs tag params result) = do
	errorContextStateT ("in abstraction at " ++ formatRange tag) $
		makeAbstraction varTypes params (\ varTypes -> protoCompileMetaObject result)

protoCompileMetaObject varTypes (MOVar tag name) =
	case M.lookup name varTypes of
		Just (ProtoCompileVarPresent rmo) ->
			return (const rmo)
		Just (ProtoCompileVarFuture) ->
			error "top-sort should have prevented this"
		Just (ProtoCompileVarLocal) ->
			return (\varValues -> (M.!) varValues name)
		Nothing ->
			lift $ Left ("name `" ++ name ++ "` is not in scope at " ++
				formatRange tag)

protoCompileMetaObject varTypes (MOJSExpr tag code type_ spec subs) =
	errorContextStateT ("in (js-expr ...) at " ++ formatRange tag) $ do
		typeFun <-
			errorContextStateT ("in (type ...) clause") $
			protoCompileMetaObject varTypes type_
		subFuns <- sequence [errorContextStateT ("in substitution for variable " ++ show name) $ do
			valueFun <- protoCompileMetaObject varTypes value
			let valueDummy = valueFun (makeDummies varTypes)
			lift $ checkCanCastRMTToJSTerm "the new value" False (typeOfRMO valueDummy)
			return (name, valueFun)
			| (name, value) <- subs]
		return (\varValues -> let
			typeRMT = typeFun varValues
			subMaybeEquivs = [case subFun varValues of
				RMOJSTerm _ _ subEquiv -> Just (name, subEquiv)
				RMOUnknown _ _ -> Nothing
				_ -> error "checkCanCastRMTToJSTerm should have caught this"
				| (name, subFun) <- subFuns]
			in case sequence subMaybeEquivs of
				Just subEquivs -> let
					wholeEquiv = do
						subJSs <- liftM fromList $ sequence [do
							subJS <- subEquiv
							return (name, subJS)
							| (name, subEquiv) <- subEquivs]
						JSUtils.revariableExpression subJSs (JS.removeAnnotations code)
					in (RMOJSTerm typeRMO spec wholeEquiv)
				Nothing ->
					RMOUnknown (RMTJSTerm typeRMO (Data.Maybe.isJust spec)) Nothing
			)

protoCompileMetaObject varTypes (MOJSGlobal tag uniqueId content type_ spec) =
	errorContextStateT ("in `(js-global ...)` at " ++ formatRange tag) $ do
		typeFun <-
			errorContextStateT ("in (type ...) clause") $
			protoCompileMetaObject type_

		ProtoCompileState oldNameSupply oldMap <- get
		pcg <- case M.lookup uniqueId oldMap of
			Just existingPCG ->
				return existingPCG
			Nothing -> let
				name:newNameSupply = oldNameSupply

				isTransferrable :: RMT -> Bool
				transferrableLocalVars = [(name, ty)
					| (name, ProtoCompileVarLocal ty) <- M.toList varTypes
					, isTransferrable ty]
				untransferrableLocalVars = M.fromList [(name, ty)
					| (name, ProtoCompileVarLocal ty) <- M.toList varTypes
					, not (isTransferrable ty)]
				globalVars = S.fromList [name | (name, ProtoCompileVarPresent _) <- M.toList varTypes]
					`S.union` S.fromList [name | (name, ProtoCompileVarFuture) <- M.toList varTypes]

				pcg = ProtoCompileGlobal {
					nameOfProtoCompileGlobal = name,
					syntaxOfProtoCompileGlobal = content,
					transferrableLocalVarsOfProtoCompileGlobal = transferrableLocalVars,
					untransferrableLocalVarsOfProtoCompileGlobal = untransferrableLocalVars,
					globalVarsOfProtoCompileGlobal = globalVars
					}

				newMap = M.insert uniqueId pcg oldMap

				in do
					put (ProtoCompileState newNameSupply newMap)
					return pcg

		return (\varValues -> let
			typeRMO = typeFun varValues
			varMaybeEquivs = [
				case (M.!) varValues name of
					RMOUnknown _ _ -> Nothing
					RMOJSTerm _ _ varEquiv -> Just varEquiv
					_ -> error "isTransferrable should have caught this"
				| (name, _) <- transferrableLocalVarsOfProtoCompileGlobal pcg]
			in case sequence varMaybeEquivs of
				Just varEquivs -> let
					wholeEquiv = do
						varJSs <- sequence varEquivs
						return (JS.CallExpr ()
							(JS.VarRef () (Id () (nameOfProtoCompileGlobal pcg)))
							varJSs
							)
					in (RMOJSTerm typeRMO spec wholeEquiv)
				Nothing ->
					RMOUnknown typeRMO Nothing
			)

-- `makeDummies` turns `ProtoCompileVar`s into `RMO`s by replacing local variables
-- with `RMOUnknown`

makeDummies :: M.Map String ProtoCompileVar -> M.Map String RMO
makeDummies = M.mapWithKey (\name var -> case var of
	ProtoCompileVarPresent rmo ->
		rmo
	ProtoCompileVarFuture ty ->
		-- This relies on the fact that map values are lazy
		error "top-sort should have prevented this"
	ProtoCompileVarLocal ty ->
		RMOUnknown ty (Just name)
	)

-- `errorContextStateT` puts the given message on top of any error messages as
-- they bubble up.

errorContextStateT :: String -> StateT s (Either String) a -> StateT s (Either String) a
errorContextStateT msg action = StateT (\s1 -> case runStateT action s1 of
	Left msgs -> Left (msg ++ "\n" ++ msgs)
	Right (s2, a) -> Right (s2, a)
	)

-- `makeAbstraction` is used to implement `(\ ... -> ...)`, `(let ...)`, and
-- `(js-repr ...)`; in other words, any situation where an RMO is parameterized
-- on a list of parameters.

makeAbstraction :: M.Map String ProtoCompileVar
                -> [(String, MetaType Range)]
                -> (M.Map String ProtoCompileVar -> StateT ProtoCompileState (Either String) (M.Map String RMO -> RMO))
                -> StateT ProtoCompileState (Either String) (M.Map String RMO -> RMO)

makeAbstraction varTypes [] final =
	final varTypes

makeAbstraction varTypes ((paramName, paramType):rest) = do
	paramTypeFun <-
		errorContextStateT ("in type of parameter `" ++ paramName ++ "`") $
		protoCompileMetaType varTypes paramType
	let paramTypeDummy = paramTypeFun (makeDummies varTypes)
	let varTypes' = M.insert paramName (ProtoCompileVarLocal paramTypeDummy) 
	resultFun <- f varTypes' rest
	return (\varValues -> let
		paramRMT = paramTypeFun varValues
		paramDummy = RMOUnknown paramRMT (Just paramName)
		returnTypeDummy = typeOfRMO (resultFun (M.insert paramName paramDummy varValues))
		RMOFun (paramName, paramRMT) returnTypeDummy (\argRMO -> resultFun (M.insert paramName argRMO varValues))
		)

