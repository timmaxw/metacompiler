module Metacompiler.Compile.CompileTL where

import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Graph
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Language.ECMAScript3.Syntax.Annotations as JS
import qualified Metacompiler.JS as JS
import qualified Metacompiler.Runtime as R
import Metacompiler.SExpr.Types (Range, formatRange)
import qualified Metacompiler.Compile.CompileSL as CSL
import qualified Metacompiler.SL.Syntax as SL
import qualified Metacompiler.TL.Syntax as TL

data MetaObjectInScope
	= MetaObjectInScopeGlobalPresent R.MetaObject
	| MetaObjectInScopeGlobalFuture
	| MetaObjectInScopeLocal R.Name R.MetaType
	| MetaObjectInScopeCantTransfer R.MetaType

data Scope = Scope {
	metaObjectsInScope :: M.Map TL.Name MetaObjectInScope,
	slObjectsInScope :: CSL.Scope
	}

newtype LoopBreakerToProcess
	= LoopBreakerToProcess (M.Map TL.Name R.MetaObject -> StateT (S.Set (JS.Id ())) (Either String) [JS.Statement ()])

type LocalCompileMonad a = WriterT [LoopBreakerToProcess] (StateT (S.Set (JS.Id ())) (Either String)) a

embedEither :: Either String a -> LocalCompileMonad a
embedEither = lift . lift

compileMetaType :: Scope -> TL.MetaType Range -> LocalCompileMonad R.MetaType
compileMetaType scope (TL.MTFun range params result) =
	compileAbstraction scope params (\_ scope' -> compileMetaType scope' result) R.MTFun
compileMetaType scope (TL.MTSLType range slKind) = do
	slKind' <- embedEither $ CSL.compileSLKind slKind
	return (R.MTSLType slKind')
compileMetaType scope (TL.MTSLTerm range slType) = do
	slType' <- compileMetaObject scope slType
	embedEither $ checkType (R.typeOfMetaObject slType') (R.MTSLType R.SLKindType)
	return (R.MTSLTerm slType')
compileMetaType scope (TL.MTJSExprType range slType) = do
	slType' <- compileMetaObject scope slType
	embedEither $ checkType (R.typeOfMetaObject slType') (R.MTSLType R.SLKindType)
	return (R.MTJSExprType slType')
compileMetaType scope (TL.MTJSExpr range jsType slTerm) = do
	jsType' <- compileMetaObject scope jsType
	slType1 <- embedEither $ checkTypeJSExprType (R.typeOfMetaObject jsType')
	slTerm' <- compileMetaObject scope slTerm
	slType2 <- embedEither $ checkTypeSLTerm (R.typeOfMetaObject slTerm')
	case R.equivalentMetaObjects (R.reduceMetaObject slType1) (R.reduceMetaObject slType2) of
		True -> return ()
		False -> embedEither $ Left ("at " ++ formatRange range ++ ": The type of the SL equivalent should be the SL \
			\equivalent of the JavaScript type.")
	return (R.MTJSExpr jsType' slTerm')

compileMetaObject :: Scope -> TL.MetaObject Range -> LocalCompileMonad R.MetaObject

compileMetaObject scope (TL.MOApp range fun arg) = do
	fun' <- compileMetaObject scope fun
	arg' <- compileMetaObject scope arg
	((_, paramType), _) <- embedEither $ checkTypeFun (R.typeOfMetaObject fun')
	embedEither $ checkType (R.typeOfMetaObject arg') paramType
	return (R.MOApp fun' arg')

compileMetaObject scope (TL.MOAbs range params result) =
	compileAbstraction scope params (\_ scope' -> compileMetaObject scope' result) R.MOAbs

compileMetaObject scope (TL.MOName range name) = case M.lookup name (metaObjectsInScope scope) of
	Just (MetaObjectInScopeGlobalPresent x) ->
		return x
	Just (MetaObjectInScopeGlobalFuture) ->
		error "top-sort should have prevented this"
	Just (MetaObjectInScopeLocal name' type_) ->
		return (R.MOName name' type_)
	Just (MetaObjectInScopeCantTransfer type_) ->
		embedEither $ Left ("can't transfer variable of that type across a JS loop breaker")
	Nothing ->
		embedEither $ Left ("at " ++ formatRange range ++ ": name `" ++ TL.unName name ++ "` is not in scope")

compileMetaObject scope (TL.MOSLTypeLiteral range code typeBindings) = do
	typeBindings' <- compileSLTypeBindings scope typeBindings
	embedEither $ CSL.compileSLType
		(M.union typeBindings' (CSL.typesInScope $ slObjectsInScope $ scope))
		[] code

compileMetaObject scope (TL.MOSLTermLiteral range code typeBindings termBindings) = do
	typeBindings' <- compileSLTypeBindings scope typeBindings
	termBindings' <- compileSLTermBindings scope termBindings
	let slScope' = (slObjectsInScope scope) {
			CSL.typesInScope = typeBindings' `M.union` (CSL.typesInScope $ slObjectsInScope scope),
			CSL.termsInScope = termBindings' `M.union` (CSL.termsInScope $ slObjectsInScope scope)
			} 
	embedEither $ CSL.compileSLTerm slScope' [] code

compileMetaObject scope (TL.MOJSExprLiteral range equiv type_ expr bindings) = do
	equiv' <- compileMetaObject scope equiv
	type_' <- compileMetaObject scope type_
	bindings' <- compileJSExprBindings scope bindings
	return (R.MOJSExprLiteral equiv' type_' (JS.removeAnnotations expr) bindings')

compileMetaObject scope (TL.MOJSExprLoopBreak range equiv type_ content) = do
	equiv' <- compileMetaObject scope equiv
	type_' <- compileMetaObject scope type_

	oldGlobalNamesInUse <- lift get
	let Just globalName = find (`S.notMember` oldGlobalNamesInUse) [JS.Id () ("g" ++ show i) | i <- [1..]]
	lift (put (S.insert globalName oldGlobalNamesInUse))

	let params = [(synName, runName, eq, ty)
		| (synName, MetaObjectInScopeLocal runName (R.MTJSExpr eq ty)) <- M.toList (metaObjectsInScope scope)
		, synName `S.member` freeNames]
		where freeNames = TL.freeNamesInMetaObject content

	let loopBreakerToProcess = LoopBreakerToProcess $ \globalDefns -> do

		let
			processVar :: TL.Name -> MetaObjectInScope -> MetaObjectInScope
			processVar name (MetaObjectInScopeGlobalPresent x) = MetaObjectInScopeGlobalPresent x
			processVar name MetaObjectInScopeGlobalFuture = case M.lookup name globalDefns of
				Just x -> MetaObjectInScopeGlobalPresent x
				Nothing -> error "promised global definition never found"
			processVar name (MetaObjectInScopeLocal runName runType) = case runType of
				R.MTFun _ _ -> MetaObjectInScopeCantTransfer runType
				_ -> MetaObjectInScopeLocal runName runType
			processVar name (MetaObjectInScopeCantTransfer runType) =
				MetaObjectInScopeCantTransfer runType
			scope' = scope { metaObjectsInScope = M.mapWithKey processVar (metaObjectsInScope scope) }

		(content', subLoopBreakers) <- runWriterT $ compileMetaObject scope' content
		lift $ checkType (R.typeOfMetaObject content') (R.MTJSExpr type_' equiv')

		let
			-- TODO: This is susceptible to name collisions.
			convertName :: TL.Name -> JS.Id ()
			convertName (TL.Name n) = JS.Id () n

			subs = M.fromList [(runName, R.MOJSExprLiteral eq ty (JS.VarRef () (convertName synName)) M.empty)
				| (synName, runName, eq, ty) <- params]
			content'' = R.substituteMetaObject (R.Substitutions subs M.empty M.empty) content'
			content''' = R.reduceMetaObject content''
			contentAsJS = case content''' of
				R.MOJSExprLiteral _ _ expr subs | M.null subs -> expr
				_ -> error "reduction of loop breaker didn't work completely"

			emit = JS.FunctionStmt () globalName [convertName n | (n, _, _, _) <- params] [JS.ReturnStmt () (Just contentAsJS)]

		subEmits <- liftM concat $ sequence [f globalDefns | LoopBreakerToProcess f <- subLoopBreakers]

		return (subEmits ++ [emit])
	tell [loopBreakerToProcess]

	let paramJSNames = [JS.Id () n | (TL.Name n, _, _, _) <- params]
	let expr = JS.CallExpr () (JS.VarRef () globalName) [JS.CallExpr () (JS.VarRef () p) [] | p <- paramJSNames]
	let bindings = M.fromList [(jsName, R.JSExprBinding [] (R.MOName runName (R.MTJSExpr eq ty)))
		| (jsName, (synName, runName, eq, ty)) <- zip paramJSNames params]
	return $ R.MOJSExprLiteral equiv' type_' expr bindings

compileAbstraction :: Scope
                   -> [(TL.Name, TL.MetaType Range)]
                   -> ([(R.Name, R.MetaType)] -> Scope -> LocalCompileMonad a)
                   -> ((R.Name, R.MetaType) -> a -> a)
                   -> LocalCompileMonad a
compileAbstraction scope [] base fun = base [] scope
compileAbstraction scope ((paramName, paramType):params) base fun = do
	let paramName' = R.Name (TL.unName paramName)
	paramType' <- compileMetaType scope paramType
	let scope' = scope { metaObjectsInScope = M.insert paramName (MetaObjectInScopeLocal paramName' paramType') (metaObjectsInScope scope) }
	let base' = base . ((paramName', paramType'):)
	rest <- compileAbstraction scope' params base' fun
	return (fun (paramName', paramType') rest)

compileBindings :: Ord n
                => Scope
                -> (Scope -> TL.BindingParam Range -> LocalCompileMonad (Scope, p))
                -> (Scope -> [p] -> TL.MetaObject Range -> LocalCompileMonad a)
                -> [TL.Binding Range n]
                -> LocalCompileMonad (M.Map n a)
compileBindings scope paramFun valueFun bindings = do
	bindings' <- sequence [do
		let
			f scope' [] params' =
				valueFun scope' params' (TL.valueOfBinding binding)
			f scope' (param:params) paramsSoFar' = do
				(scope'', param') <- paramFun scope' param
				f scope'' params (paramsSoFar' ++ [param'])
		value' <- f scope (TL.paramsOfBinding binding) []
		return (TL.nameOfBinding binding, value')
		| binding <- bindings]
	-- TODO: Check for duplicates
	return (M.fromList bindings')

compileSLTypeBindings :: Scope
                      -> [TL.Binding Range SL.NameOfType]
                      -> LocalCompileMonad (M.Map SL.NameOfType CSL.TypeInScope)
compileSLTypeBindings scope bindings = compileBindings scope
	(\ subScope (TL.BindingParam parts) -> do
		(name, type_) <- case parts of
			[(name, type_)] -> return (name, type_)
			_ -> embedEither $ Left ("Parameters to bindings in a `(sl-type ...)` construct should all have exactly one \
				\part.")
		let name' = R.Name (TL.unName name)
		type_' <- compileMetaType scope type_
		slKind' <- case type_' of
			R.MTSLType slKind' -> return slKind'
			_ -> embedEither $ Left "Parameters to bindings in a `(sl-type ...)` construct should all have type \
				\`(sl-type ...)`."
		let subScope' = subScope { metaObjectsInScope = M.insert name (MetaObjectInScopeLocal name' type_') (metaObjectsInScope subScope) }
		return (subScope', (name', slKind'))
		)
	(\ subScope' params' value -> do
		value' <- compileMetaObject subScope' value
		embedEither $ checkTypeSLType (R.typeOfMetaObject value')
		return (CSL.TypeInScope {
			CSL.typeParamsOfTypeInScope = map snd params',
			CSL.valueOfTypeInScope = \ paramValues -> let
				metaObjectSubs = M.fromList (zip (map fst params') paramValues)
				in R.substituteMetaObject (R.Substitutions metaObjectSubs M.empty M.empty) value'
			})
		)
	bindings

compileSLTermBindings :: Scope
                      -> [TL.Binding Range SL.NameOfTerm]
                      -> LocalCompileMonad (M.Map SL.NameOfTerm CSL.TermInScope)
compileSLTermBindings scope bindings = compileBindings scope
	(\ subScope (TL.BindingParam parts) -> do
		(name, type_) <- case parts of
			[(name, type_)] -> return (name, type_)
			_ -> embedEither (Left "Parameters to bindings in a `(sl-term ...)` construct should all have exactly one \
				\part.")
		let name' = R.Name (TL.unName name)
		type_' <- compileMetaType scope type_
		slKindOrType' <- case type_' of
			R.MTSLType slKind' -> return (Left slKind')
			R.MTSLTerm slType' -> return (Right slType')
			_ -> embedEither $ Left "Parameters to bindings in a `(sl-term ...)` construct should all have type \
				\`(sl-type ...)` or `(sl-term ...)`."
		let subScope' = subScope { metaObjectsInScope =	M.insert name (MetaObjectInScopeLocal name' type_') (metaObjectsInScope subScope) }
		return (subScope', (name', slKindOrType'))
		)
	(\ subScope' params' value -> do
		let
			isLeft :: Either l r -> Bool
			isLeft (Left _) = True
			isLeft (Right _) = False
		when (any (isLeft . snd) $ dropWhile (isLeft . snd) params') $
			embedEither $ Left "In a `(sl-term ...)` construct, all type-parameters must come before all term-parameters."
		let typeParams' = [(name, slKind') | (name, Left slKind') <- params']
		let termParams' = [(name, slType') | (name, Right slType') <- params']
		value' <- compileMetaObject subScope' value
		embedEither $ checkTypeSLTerm (R.typeOfMetaObject value')
		return (CSL.TermInScope {
			CSL.typeParamsOfTermInScope = map snd typeParams',
			CSL.termParamsOfTermInScope = map snd termParams',
			CSL.valueOfTermInScope = \ typeParamValues termParamValues -> let
				metaObjectSubs = M.fromList (zip (map fst typeParams') typeParamValues ++ zip (map fst termParams') termParamValues)
				in R.substituteMetaObject (R.Substitutions metaObjectSubs M.empty M.empty) value'
			})
		)
	bindings

compileJSExprBindings :: Scope
                      -> [TL.Binding Range (JS.Id ())]
                      -> LocalCompileMonad (M.Map (JS.Id ()) R.JSExprBinding)
compileJSExprBindings scope bindings = compileBindings scope
	(\ subScope (TL.BindingParam parts) -> do
		(name1, type1, name2, type2) <- case parts of
			[(name1, type1), (name2, type2)] -> return (name1, type1, name2, type2)
			_ -> embedEither (Left "JavaScript expression binding parameters should all have exactly two parts.")

		let name1' = R.Name (TL.unName name1)
		type1' <- compileMetaType scope type1
		slType <- embedEither $ checkTypeSLTerm type1'

		let scopeForType2 = scope { metaObjectsInScope =
			M.insert name1 (MetaObjectInScopeLocal name1' type1') $
			metaObjectsInScope scope
			}
		let name2' = R.Name (TL.unName name2)
		type2' <- compileMetaType scopeForType2 type2
		jsType <- case R.reduceMetaType type2' of
			R.MTJSExpr jsType (R.MOName n _) | n == name1' -> return jsType
			_ -> embedEither $ Left "Type of second binding parameter should be `js-expr <type> <first binding parameter>`"

		let subScope' = subScope { metaObjectsInScope =
			M.insert name2 (MetaObjectInScopeLocal name2' type2') $
			M.insert name1 (MetaObjectInScopeLocal name1' type1') $
			metaObjectsInScope subScope
			}

		return (subScope', R.JSExprBindingParam name1' slType name2' jsType)
		)

	(\ subScope' params' value -> do
		value' <- compileMetaObject subScope' value
		embedEither $ checkTypeJSExpr (R.typeOfMetaObject value')
		return (R.JSExprBinding params' value')
		)

	bindings

checkType :: R.MetaType -> R.MetaType -> Either String ()
checkType actualType expectedType = if actualType `R.equivalentMetaTypes` expectedType
	then return ()
	else Left ("expected type `<not implemented>`, got something else")

checkTypeFun :: R.MetaType -> Either String ((R.Name, R.MetaType), R.MetaType)
checkTypeFun actualType = case R.reduceMetaType actualType of
	R.MTFun (paramName, paramType) returnType -> return ((paramName, paramType), returnType)
	_ -> Left ("expected type `(fun ... -> ...)`, got something else")

checkTypeSLType :: R.MetaType -> Either String R.SLKind
checkTypeSLType actualType = case R.reduceMetaType actualType of
	R.MTSLType slKind -> return slKind
	_ -> Left "expected type `(sl-type ...)`, got something else"

checkTypeSLTerm :: R.MetaType -> Either String R.MetaObject
checkTypeSLTerm actualType = case R.reduceMetaType actualType of
	R.MTSLTerm slType -> return slType
	_ -> Left "expected type `(sl-term ...)`, got something else"

checkTypeJSExprType :: R.MetaType -> Either String R.MetaObject
checkTypeJSExprType actualType = case R.reduceMetaType actualType of
	R.MTJSExprType slType -> return slType
	_ -> Left "expected type `(js-expr-type ...)`, got something else"

checkTypeJSExpr :: R.MetaType -> Either String (R.MetaObject, R.MetaObject)
checkTypeJSExpr actualType = case R.reduceMetaType actualType of
	R.MTJSExpr jsType slTerm -> return (jsType, slTerm)
	_ -> Left "expected type `(js-expr ...)`, got something else"

data GlobalResults = GlobalResults {
	slDefnsOfGlobalResults :: CSL.Defns,
	tlDefnsOfGlobalResults :: M.Map TL.Name R.MetaObject,
	emitsOfGlobalResults :: [JS.Statement ()]
	}

compileDirectives :: [TL.Directive Range] -> Either String GlobalResults
compileDirectives directives = flip evalStateT S.empty $ do
	let allSLDirs = concat [content | TL.DSLCode _ content <- directives]
	slDefns <- lift $ CSL.compileSLDirectives allSLDirs

	-- `let` and `js-expr-type` directives are collectively referred to as "definition directives" because they
	-- introduce new names into scope. They also both may refer to names already in scope; this means that they have to
	-- be top-sorted before processing them.
	let unsortedDefnDirs = concat [case dir of
			TL.DLet _ name _ _ _ -> [((name, dir), name, S.toList (TL.freeNamesInDirective dir))]
			TL.DJSExprType _ name _ _ -> [((name, dir), name, S.toList (TL.freeNamesInDirective dir))]
			_ -> []
			| dir <- directives]
	sortedDefnDirs <- sequence [case scc of
		Data.Graph.AcyclicSCC (name, dir) -> return (name, dir)
		Data.Graph.CyclicSCC _ -> lift $ Left ("mutual recursion detected")
		| scc <- Data.Graph.stronglyConnComp unsortedDefnDirs]

	(tlDefns, loopBreakersFromDefnDirs) <- runWriterT $ do

		let
			compileDefnDir :: Scope -> TL.Directive Range -> LocalCompileMonad R.MetaObject
			compileDefnDir scope (TL.DLet _ _ params maybeType value) =
				compileAbstraction scope params (\_ scope' -> do
					value' <- compileMetaObject scope' value
					case maybeType of
						Nothing -> return ()
						Just type_ -> do
								type_' <- compileMetaType scope' type_
								embedEither $ checkType (R.typeOfMetaObject value') type_'
					return value'
					) R.MOAbs
			compileDefnDir scope (TL.DJSExprType _ name params slEquiv) =
				compileAbstraction scope params (\params' scope' -> do
					slEquiv' <- compileMetaObject scope' slEquiv
					embedEither $ checkType (R.typeOfMetaObject slEquiv') (R.MTSLType R.SLKindType)
					let defn = R.JSExprTypeDefn {
						R.nameOfJSExprTypeDefn = R.Name (TL.unName name),
						R.paramsOfJSExprTypeDefn = map snd params',
						R.slEquivOfJSExprTypeDefn = \paramValues -> let
							subs = M.fromList $ zip (map fst params') paramValues
							in R.substituteMetaObject (R.Substitutions subs M.empty M.empty) slEquiv'
						}
					return (R.MOJSExprTypeDefn defn [R.MOName paramName paramType | (paramName, paramType) <- params'])
					) R.MOAbs

		foldM (\ tlDefns (name, defnDir) -> do
			let scope = Scope (M.map MetaObjectInScopeGlobalPresent tlDefns) (CSL.scopeForDefns slDefns)
			value <- compileDefnDir scope defnDir
			return $ M.insert name value tlDefns
			)
			M.empty
			sortedDefnDirs

	(emitsFromEmitDirs, loopBreakersFromEmitDirs) <- runWriterT $
		liftM concat $ sequence [do
			let scope = Scope (M.map MetaObjectInScopeGlobalPresent tlDefns) (CSL.scopeForDefns slDefns)
			bindings' <- compileJSExprBindings scope bindings
			let substs = M.map (\binding ->
				case R.tryReduceJSExprBindingToJSSubst S.empty binding of
					Just f -> f M.empty
					Nothing -> error "cannot reduce binding, for no good reason"
				) bindings'
			return $ map (JS.substituteStatement substs M.empty . JS.removeAnnotations) code
			| TL.DJSEmit _ code bindings <- directives]

	let allLoopBreakers = loopBreakersFromDefnDirs ++ loopBreakersFromEmitDirs
	emitsFromLoopBreakers <- liftM concat $
		sequence [fun tlDefns | LoopBreakerToProcess fun <- allLoopBreakers]

	-- Emits from loop breakers must go before emits from `emit` directives so that user-supplied statements can access
	-- automatically-generated global definitions
	let allEmits = emitsFromLoopBreakers ++ emitsFromEmitDirs 

	return $ GlobalResults {
		slDefnsOfGlobalResults = slDefns,
		tlDefnsOfGlobalResults = tlDefns,
		emitsOfGlobalResults = allEmits
		}

