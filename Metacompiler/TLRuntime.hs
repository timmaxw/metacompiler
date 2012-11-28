module Metacompiler.TLRuntime where

import Control.Monad (liftM, unless)
import Control.Monad.State
import Control.Monad.Trans (lift)
import qualified Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Maybe (isJust)
import qualified Language.ECMAScript3.PrettyPrint as JS
import qualified Language.ECMAScript3.Syntax as JS
import qualified Language.ECMAScript3.Syntax.Annotations as JS
import qualified Metacompiler.JSUtils as JSUtils
import Metacompiler.SExpr (Range, formatRange)
import Metacompiler.SExprToSL (errorContext)   -- TODO: Move `errorContext` into its own file
import qualified Metacompiler.SLSyntax as SL
import qualified Metacompiler.TLSyntax as TL

-- `RMT` stands for `reduced meta-type`. It's an internal de-sugared
-- representation of meta-types.

data RMT
	= RMTJSType
	| RMTJSTerm RMO Bool
	| RMTFun (String, RMT) RMT

formatRMT :: RMT -> String
formatRMT RMTJSType = "js-type"
formatRMT (RMTJSTerm ty True) = "(js-sl-term " ++ formatRMO ty ++ ")"
formatRMT (RMTJSTerm ty False) = "(js-term " ++ formatRMO ty ++ ")"
formatRMT (RMTFun (name, arg) res) = "(fun (" ++ name ++ " :: " ++ formatRMT arg ++ ") -> " ++ formatRMT res ++ ")"

-- `RMO` stands for `reduced meta-object`. Note that it's not necessarily in
-- weak head normal form because of `RMOUnknown`.

data RMO
	-- `RMOUnknown ty _` represents an unknown value of type `ty`. These arise
	-- when we try to evaluate a term which contains a variable whose value is
	-- not known; this happens when we are reasoning about dependent types.
	-- 
	-- If the `RMOUnknown` corresponds exactly to a single variable, then the
	-- second field will be `(Just name)` where `name` is the name of the
	-- variable. This allows for comparison of dependent types in simple cases.
	-- If the second field is `Nothing`, then the `RMOUnknown` could have any
	-- value.
	= RMOUnknown RMT (Maybe String)

	-- `RMOJSRepr`, `RMOJSTerm`, and `RMOFun` are in weak head normal form.
	| RMOJSRepr String [RMO]
	| RMOJSTerm RMO (Maybe (SL.Term Range)) (State JSUtils.SymbolRenaming (JS.Expression ()))
	| RMOFun (String, RMT) RMT (RMO -> RMO)

formatRMO :: RMO -> String
formatRMO (RMOUnknown ty (Just name)) = name
formatRMO (RMOUnknown ty Nothing) = "<unknown " ++ formatRMT ty ++ ">"
formatRMO (RMOJSRepr name params) = "(" ++ Data.List.intercalate " " (name:map formatRMO params) ++ ")"
formatRMO unprintable = "<" ++ formatRMT (typeOfRMO unprintable) ++ ">"

-- `equivalentRMO` checks if two `RMO`s are equivalent. `canCastRMT` checks if
-- it's safe to cast from one `RMT` to another. Both return one of three
-- outcomes: `Provably True`, `NotProvable`, or `Provably False`. Both take a
-- mapping from pairs of variable names to `Provability Bool`, which indicates
-- which variables in the first expression's context are provably equal /
-- provably not equal to variables in the second expression's context.

data Provability a = Provably a | NotProvable

provabilityAnd :: Provability Bool -> Provability Bool -> Provability Bool
provabilityAnd (Provably True) (Provably True) = Provably True
provabilityAnd (Provably False) _ = Provably False
provabilityAnd _ (Provably False) = Provably False
provabilityAnd _ _ = NotProvable

equivalentRMO :: M.Map (String, String) (Provability Bool) -> RMO -> RMO -> Provability Bool
equivalentRMO vars (RMOUnknown _ (Just var1)) (RMOUnknown _ (Just var2)) =
	case M.lookup (var1, var2) vars of
		Just res -> res
		Nothing
			| var1 == var2 && varsNotMentioned -> Provably True
			| otherwise -> NotProvable
			where varsNotMentioned = all (\ (v1, v2) -> v1 /= var1 && v2 /= var2) (M.keys vars)
equivalentRMO vars (RMOJSRepr name1 params1) (RMOJSRepr name2 params2) =
	if name1 == name2
		then foldr provabilityAnd (Provably True) (zipWith (equivalentRMO vars) params1 params2)
		else Provably False
equivalentRMO vars _ _ = NotProvable

canCastRMT :: M.Map (String, String) (Provability Bool) -> RMT -> RMT -> Provability Bool
canCastRMT vars (RMTJSType) (RMTJSType) = Provably True
canCastRMT vars (RMTJSTerm ty1 hasSL1) (RMTJSTerm ty2 hasSL2) =
	if (hasSL1 || not hasSL2)
		then equivalentRMO vars ty1 ty2
		else Provably False
canCastRMT vars (RMTFun (var1, arg1) ret1) (RMTFun (var2, arg2) ret2) =
	canCastRMT vars arg2 arg1
	`provabilityAnd`
	canCastRMT
		(M.insert (var1, var2) (Provably True) $ M.filterWithKey (\ (v1, v2) _ -> v1 /= var1 && v2 /= var2) $ vars)
		ret1 ret2
canCastRMT vars _ _ = Provably False

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

-- `typeOfRMO` returns the `RMT` representing the type of the given `RMO`

typeOfRMO :: RMO -> RMT
typeOfRMO (RMOUnknown ty _) = ty
typeOfRMO (RMOJSRepr _ _) = RMTJSType
typeOfRMO (RMOJSTerm ty (Just _) _) = RMTJSTerm ty True
typeOfRMO (RMOJSTerm ty Nothing _) = RMTJSTerm ty False
typeOfRMO (RMOFun (name, argTy) retTy _) = RMTFun (name, argTy) retTy

-- `substituteRMT` and `substituteRMO` assign to the value of a variable in the
-- given RMO.

substituteRMT :: M.Map String RMO -> RMT -> RMT
substituteRMT vars RMTJSType =
	RMTJSType
substituteRMT vars (RMTJSTerm ty hasSL) =
	RMTJSTerm (substituteRMO vars ty) hasSL
substituteRMT vars (RMTFun (argName, argType) retType) =
	RMTFun
		(argName, substituteRMT vars argType)
		(substituteRMT (M.delete argName vars) retType)

substituteRMO :: M.Map String RMO -> RMO -> RMO
substituteRMO vars (RMOUnknown ty (Just name)) | name `M.member` vars =
	(M.!) vars name
substituteRMO vars (RMOUnknown ty val) =
	RMOUnknown (substituteRMT vars ty) val
substituteRMO vars (RMOJSRepr name params) =
	RMOJSRepr name (map (substituteRMO vars) params)
substituteRMO vars (RMOJSTerm ty maybeSL jsEquivalent) =
	RMOJSTerm
		(substituteRMO vars ty)
		(fmap (const (error "SL not supported yet")) maybeSL)
		jsEquivalent
substituteRMO vars (RMOFun (argName, argType) retType impl) =
	RMOFun
		(argName, substituteRMT vars argType)
		(substituteRMT (M.delete argName vars) retType)
		(substituteRMO vars . impl)

-- `reduceMetaType` converts a `TL.MetaType Range` into a `RMT`. It will
-- also check for validity of variable references and correctness of types as
-- it traverses the AST.

reduceMetaType :: M.Map String RMO -> TL.MetaType Range -> Either String RMT

reduceMetaType vars (TL.MTJSType tag) = do
	return RMTJSType

reduceMetaType vars (TL.MTJSTerm tag hasSL ty) = do
	ty' <- reduceMetaObject vars ty
	case typeOfRMO ty' of
		RMTJSType -> return ()
		_ -> Left ("type at " ++ formatRange (TL.tagOfMetaObject ty) ++ " should \
			\have meta-type (js-type) but instead has meta-type " ++
			formatRMT (typeOfRMO ty'))
	return (RMTJSTerm ty' hasSL)

reduceMetaType vars (TL.MTFun tag params final) = do
	let
		f :: M.Map String RMO -> [(String, TL.MetaType Range)] -> Either String RMT
		f vars' [] = do
			reduceMetaType vars' final
		f vars' ((paramName, paramMetaType):rest) = do
			paramRMT <- reduceMetaType vars' paramMetaType
			let vars'' = M.insert paramName (RMOUnknown paramRMT (Just paramName)) vars'
			resultRMT <- f vars'' rest
			return (RMTFun (paramName, paramRMT) resultRMT)
	f vars params

-- `reduceMetaObject` converts a `TL.MetaObject Range` into a `RMO`
-- and also checks for validity of variable references and correctness of
-- types.

reduceMetaObject :: M.Map String RMO -> TL.MetaObject Range -> Either String RMO

reduceMetaObject vars (TL.MOApp tag fun arg) =
	errorContext ("in application at " ++ formatRange tag) $ do
		funRMO <- reduceMetaObject vars fun
		case typeOfRMO funRMO of
			RMTFun (argName, expectedArgRMT) retRMT -> do
				argRMO <- reduceMetaObject vars arg
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

reduceMetaObject vars (TL.MOAbs tag params result) =
	errorContext ("in abstraction at " ++ formatRange tag) $ do
		makeAbstraction vars params (\vars' -> reduceMetaObject vars' result)

reduceMetaObject vars (TL.MOVar tag var) = case M.lookup var vars of
	Just value -> return value
	Nothing -> Left ("variable \"" ++ var ++ "\" not in scope at " ++ formatRange tag)

reduceMetaObject vars (TL.MOJSExpr tag code type_ spec subs) =
	errorContext ("in (js-expr ...) at " ++ formatRange tag) $ do
		typeRMO <- reduceMetaObject vars type_
		subs' <- sequence [errorContext ("in substitution for variable " ++ show name) $ do
			value' <- reduceMetaObject vars value
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

-- `processDirectives` processes a group of TL directives. It works in a
-- `StateT Results ...` monad so that it can make definitions and such at the
-- global scope. Directives that are processed as part of the same group can
-- refer to one another recursively.

data Results = Results {
	definitionsInResults :: M.Map String RMO,
	emittedCodeOfResults :: String,
	globalTableOfResults :: M.Map (JSGlobalUniqueId, M.Map String IndexRMO) 
	symbolRenamingStateAfterResults :: JSUtils.SymbolRenaming
	}

emptyResults :: Results

emptyResults = Results {
	definitionsInResults = M.empty,
	emittedCodeOfResults = "",
	symbolRenamingStateAfterResults = JSUtils.initialSymbolRenaming
	}

processDirectives :: [TL.Directive Range] -> StateT Results (Either String) ()
processDirectives directives = do
	

processDirective (TL.DLet tag name params maybeType final) = do
	Results { definitionsInResults = vars } <- get

	valueRMO <- lift $ errorContext ("in (let ...) directive at " ++ formatRange tag) $ do
		makeAbstraction vars params $ \vars' -> do
			finalRMO <- reduceMetaObject vars' final
			case maybeType of
				Nothing -> return ()
				Just type_ -> do
					rmt <- reduceMetaType vars' type_
					checkCanCastRMT "the value (according to the type signature)" rmt (typeOfRMO finalRMO)
			return finalRMO

	makeDefinition name valueRMO

processDirective (TL.DJSRepr tag name params spec) = do
	Results { definitionsInResults = vars } <- get

	valueRMO <- lift $ errorContext ("in (js-repr ...) directive at " ++ formatRange tag) $ do
		makeAbstraction vars params $ \vars' -> do
			return (RMOJSRepr name [(M.!) vars name | (name, _) <- params])

	makeDefinition name valueRMO

processDirective (TL.DEmit tag code subs) = do
	Results { definitionsInResults = vars } <- get

	subs' <- lift $ errorContext ("in (emit ...) directive at " ++ formatRange tag) $ sequence [
		errorContext ("in substitution for variable " ++ name) $ do
			value' <- reduceMetaObject vars value
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
			paramRMT <- reduceMetaType vars' paramType
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

