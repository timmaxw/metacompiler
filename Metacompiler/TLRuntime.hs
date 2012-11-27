module Metacompiler.TLRuntime where

import Control.Monad (liftM, unless)
import Control.Monad.State
import Control.Monad.Trans (lift)
import qualified Data.Foldable
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
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
-- weak head normal form; it may have unbound variables in it.

data RMO
	= RMOVar RMT String
	| RMOJSRepr String [RMO]
	| RMOJSTerm RMO (Maybe (SL.Term Range)) (State JSUtils.SymbolRenaming (JS.Expression ()))
	| RMOFun (String, RMT) RMT (RMO -> RMO)

formatRMO :: RMO -> String
formatRMO (RMOVar _ name) = name
formatRMO (RMOJSRepr name params) = "(" ++ L.intercalate " " (name:map formatRMO params) ++ ")"
formatRMO (RMOJSTerm _ _ _) = "<js-term>"
formatRMO (RMOFun _ _ _) = "<function>"

-- `equivalentRMO` returns `True` if the two `RMO`s given are guaranteed to be
-- equal as long as all of the pairs of variables in the first parameter are
-- equal.

equivalentRMO :: S.Set (String, String) -> RMO -> RMO -> Bool
equivalentRMO vars (RMOJSTerm _ _ _) _ = error "not comparable"
equivalentRMO vars _ (RMOJSTerm _ _ _) = error "not comparable"
equivalentRMO vars (RMOFun _ _ _) _ = error "not comparable"
equivalentRMO vars _ (RMOFun _ _ _) = error "not comparable"
equivalentRMO vars (RMOVar vty1 var1) (RMOVar vty2 var2) =
	(var1, var2) `S.member` vars || var1 == var2 && Data.Foldable.all (\ (v1, v2) -> v1 /= var1 && v2 /= var2) vars
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
			let vars'' = M.insert paramName (RMOVar paramRMT paramName) vars'
			f vars'' rest
	f vars params

-- `reduceMetaObject` converts a `TL.MetaObject Range` into a `RMO`
-- and also checks for validity of variable references and correctness of
-- types.

reduceMetaObject :: M.Map String RMO -> TL.MetaObject Range -> Either String RMO

reduceMetaObject vars (TL.MOApp tag fun arg) = do
	funRMO <- reduceMetaObject vars fun
	(argName, expectedArgRMT, retRMT, funBody) <- case funRMO of
		RMOFun (n, t1) t2 b -> return (n, t1, t2, b)
		_ -> Left ("term at " ++ formatRange (TL.tagOfMetaObject fun) ++ " is \
			\being applied like a function, but has type " ++
			formatRMT (typeOfRMO funRMO) ++ ".")
	argRMO <- reduceMetaObject vars arg
	unless (canCastRMT S.empty (typeOfRMO argRMO) expectedArgRMT) $
		Left ("argument at " ++ formatRange (TL.tagOfMetaObject arg) ++ " has \
			\type " ++ formatRMT (typeOfRMO argRMO) ++ ", but function at " ++
			formatRange (TL.tagOfMetaObject fun) ++ " expects its argument to \
			\have type " ++ formatRMT expectedArgRMT ++ ".")
	return (funBody argRMO)

reduceMetaObject vars (TL.MOAbs tag params result) =
	makeAbstraction vars params (\vars' -> reduceMetaObject vars' result)

reduceMetaObject vars (TL.MOVar tag var) = case M.lookup var vars of
	Just value -> return value
	Nothing -> Left ("variable \"" ++ var ++ "\" not in scope at " ++ formatRange tag)

reduceMetaObject vars (TL.MOJSExpr tag code type_ spec subs) = do
	typeRMO <- reduceMetaObject vars type_
	subs' <- sequence [do
		value' <- reduceMetaObject vars value
		equiv <- case value' of
			RMOJSTerm _ _ equiv -> return equiv
			_ -> Left ("in (js-expr ...) block at " ++ formatRange tag ++ ": \
				\value for variable " ++ name ++ " at " ++
				formatRange (TL.tagOfMetaObject value) ++ " should have type \
				\(js-term ...) but instead had type " ++
				formatRMT (typeOfRMO value') ++ ".")
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

data Results = Results {
	definitionsInResults :: M.Map String RMO,
	emittedCodeOfResults :: String,
	symbolRenamingStateAfterResults :: JSUtils.SymbolRenaming
	}

emptyResults :: Results

emptyResults = Results {
	definitionsInResults = M.empty,
	emittedCodeOfResults = "",
	symbolRenamingStateAfterResults = JSUtils.initialSymbolRenaming
	}

processDirective :: TL.Directive Range -> StateT Results (Either String) ()

processDirective (TL.DLet tag name params maybeType final) = do
	Results { definitionsInResults = vars } <- get

	valueRMO <- lift $ errorContext ("in (let ...) directive at " ++ formatRange tag) $ do
		makeAbstraction vars params $ \vars' -> do
			finalRMO <- reduceMetaObject vars' final
			case maybeType of
				Nothing -> return ()
				Just type_ -> do
					rmt <- reduceMetaType vars' type_
					unless (canCastRMT S.empty (typeOfRMO finalRMO) rmt) $ do
						Left ("type signature says value should have type " ++
							formatRMT rmt ++ ", but value actually has type " ++
							formatRMT (typeOfRMO finalRMO))
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

	subs' <- lift $ sequence [do
		value' <- reduceMetaObject vars value
		equiv <- case value' of
			RMOJSTerm _ _ equiv -> return equiv
			_ -> Left ("in (emit ...) block at " ++ formatRange tag ++ ": \
				\value for variable " ++ name ++ " at " ++
				formatRange (TL.tagOfMetaObject value) ++ " should have type \
				\(js-term ...) but instead had type " ++
				formatRMT (typeOfRMO value') ++ ".")
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
		emittedCodeOfResults = emittedCodeOfResults results ++ "\n\n" ++ JS.renderExpression newEmittedCode
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
			let vars'' = M.insert paramName (RMOVar paramRMT paramName) vars'
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
		Just _ -> lift (Left ("name " ++ show name ++ " is already defined"))
		Nothing -> put (results {
			definitionsInResults = M.insert name value (definitionsInResults results)
			})

