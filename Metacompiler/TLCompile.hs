module Metacompiler.TLCompile where

import Control.Monad.State
import qualified Data.Map as M
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
type Scope = M.Map TLS.Name InScope

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
compileMetaObject scope (TLS.MOName range name) = case M.lookup name scope of
	Just (InScopeGlobalPresent x) -> return x
	Just InScopeGlobalFuture -> error "top-sort should have prevented this"
	Just (InScopeLocal name' type_) -> return (R.MOName name' type_)
compileMetaObject scope (TLS.MOSLTypeLiteral range code typeBindings) = do
	typeBindings' <- compileSLTypeBindings scope typeBindings
	lift (SLC.compileSLType typeBindings' [] code)
compileMetaObject scope (TLS.MOSLTermLiteral range code typeBindings termBindings) = do
	typeBindings' <- compileSLTypeBindings scope typeBindings
	termBindings' <- compileSLTermBindings scope termBindings
	lift (SLC.compileSLTerm typeBindings' M.empty termBindings' [] code)
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
                   -> ((R.Name, R.MetaType) -> a -> a)
                   -> StateT LocalState (Either String) a
compileAbstraction scope [] base fun = base scope
compileAbstraction scope ((paramName, paramType):params) base fun = do
	let paramName' = R.Name (TLS.unName paramName)
	paramType' <- compileMetaType scope paramType
	let scope' = M.insert paramName' (InScopeLocal paramType') scope
	rest <- compileAbstraction scope' params base fun
	return (fun (paramName', paramType') rest)

compileBindings :: Scope
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
compileSLTypeBindings scope bindings = compileBindings scope bindings
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
		let subScope' = M.insert name' (InScopeLocal type_') subScope
		return (subScope', (name', slKind'))
		)
	(\ subScope' params' value -> do
		value' <- compileMetaObject subScope' value
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
		let name' = R.NameOfSLTerm (SLS.unNameOfTerm name)
		type_' <- compileMetaType scope type_
		slKindOrType' <- case type_' of
			R.MTSLType slKind' -> return (Left slKind')
			R.MTSLTerm slType' -> return (Right slType')
			_ -> lift (Left "Parameters to bindings in a `(sl-term ...)` construct should all have type \
				\`(sl-type ...)` or `(sl-term ...)`.")
		let subScope' = M.insert name' (InScopeLocal type_') subScope
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
			SLC.valueOfTermInScope = \ typeParamValues termParamValues ->
				R.substituteMetaObject
					(M.fromList (zip (map fst typeParams') typeParamValues ++ zip (map fst termParams') termParamValues))
					value'
			})
		)

checkType :: R.MetaObject -> R.MetaType -> StateT LocalState (Either String) ()
checkType obj expectedType = if R.reduceMetaType (R.typeOfMetaObject obj) `R.equivalentMetaTypes` R.reduceMetaType expectedType
	then return ()
	else lift (Left "expected type `<not implemented>`, got something else")

checkTypeFun :: R.MetaObject -> StateT LocalState (Either String) ((R.Name, R.MetaType), R.MetaType)
checkTypeFun obj = case R.reduceMetaType (R.typeOfMetaObject obj) of
	R.MTFun (paramName, paramType) returnType -> ((paramName, paramType), returnType)
	_ -> lift $ Left ("expected type `(fun ... -> ...)`, got something else")

checkTypeSLType :: R.MetaObject -> StateT LocalState (Either String) R.SLKind
checkTypeSLType obj = case R.reduceMetaType (R.typeOfMetaObject obj) of
	R.MTSLType slKind -> return slKind
	_ -> lift (Left "expected type `(sl-type ...)`, got something else")

checkTypeSLTerm :: R.MetaObject -> StateT LocalState (Either String) R.MetaObject
checkTypeSLTerm obj = case R.reduceMetaType (R.typeOfMetaObject obj) of
	R.MTSLTerm slType -> return slType
	_ -> lift (Left "expected type `(sl-term ...)`, got something else")

