module Metacompiler.Compile.CompileTL where

import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Graph
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Language.ECMAScript3.Syntax.Annotations as JS
import qualified Metacompiler.JS as JS
import Metacompiler.Error
import qualified Metacompiler.Runtime as R
import qualified Metacompiler.Compile.FormatSL as FSL
import qualified Metacompiler.Compile.FormatTL as FTL
import qualified Metacompiler.Compile.CompileSL as CSL
import qualified Metacompiler.SL.Syntax as SL
import qualified Metacompiler.TL.Syntax as TL

data MetaObjectInScope
	= MetaObjectInScopeGlobalPresent R.MetaObject
	| MetaObjectInScopeGlobalFuture
	| MetaObjectInScopeLocal R.NameOfMetaObject R.MetaType
	| MetaObjectInScopeCantTransfer R.MetaType Range

data Scope = Scope {
	metaObjectsInScope :: M.Map TL.Name MetaObjectInScope,
	slObjectsInScope :: CSL.Scope
	}

newtype LoopBreakerToProcess
	= LoopBreakerToProcess (M.Map TL.Name R.MetaObject -> StateT (S.Set (JS.Id ())) (ErrorMonad) [JS.Statement ()])

type LocalCompileMonad a = WriterT [LoopBreakerToProcess] (StateT (S.Set (JS.Id ())) (ErrorMonad)) a

embedEither :: ErrorMonad a -> LocalCompileMonad a
embedEither = lift . lift

formatMTForMessage :: R.MetaType -> String
formatMTForMessage t = "`" ++ FTL.formatMetaTypeAsString t ++ "`"

formatMOForMessage :: R.MetaObject -> String
formatMOForMessage o = "`" ++ FTL.formatMetaObjectAsString o ++ "`"

compileMetaType :: Scope -> TL.MetaType Range -> LocalCompileMonad R.MetaType
compileMetaType scope (TL.MTFun range params result) =
	compileAbstraction scope params (\_ scope' -> compileMetaType scope' result) R.MTFun
compileMetaType scope (TL.MTSLType range slKind) = do
	slKind' <- embedEither $ CSL.compileSLKind slKind
	return (R.MTSLType slKind')
compileMetaType scope (TL.MTSLTerm range slType) = do
	slType' <- compileMetaObject scope slType
	case R.reduceMetaType (R.typeOfMetaObject slType') of
		R.MTSLType R.SLKindType -> return ()
		otherType -> fail ("in `(sl-term ...)` type at " ++ formatRange range ++ ": the SL type is given as " ++
			formatMOForMessage slType' ++ " (at " ++ formatRange (TL.tagOfMetaObject slType) ++ "), which has \
			\meta-type " ++ formatMTForMessage otherType ++ ", but it's supposed to have meta-type `(sl-type \
			\\"*\")`.")
	return (R.MTSLTerm slType')
compileMetaType scope (TL.MTJSExprType range slType) = do
	slType' <- compileMetaObject scope slType
	case R.reduceMetaType (R.typeOfMetaObject slType') of
		R.MTSLType R.SLKindType -> return ()
		otherType -> fail ("in `(js-expr-type ...)` type at " ++ formatRange range ++ ": the SL equivalent is given \
			\as " ++ formatMOForMessage slType' ++ " (at " ++ formatRange (TL.tagOfMetaObject slType) ++ "), which \
			\has meta-type " ++ formatMTForMessage otherType ++ ", but it's supposed to have meta-type `(sl-type \
			\\"*\")`.")
	return (R.MTJSExprType slType')
compileMetaType scope (TL.MTJSExpr range jsType slTerm) = do
	let msgPrefix = "in `(js-expr ...)` type at " ++ formatRange range ++ ": "
	jsType' <- compileMetaObject scope jsType
	slType1 <- case R.reduceMetaType (R.typeOfMetaObject jsType') of
		R.MTJSExprType equiv -> return equiv
		otherType -> fail (msgPrefix ++ "the JavaScript type is given as " ++ formatMOForMessage jsType' ++ " (at " ++
			formatRange (TL.tagOfMetaObject jsType) ++ "), which has meta-type " ++ formatMTForMessage otherType ++
			", but it's supposed to have meta-type `(js-expr-type ...)`.")
	slTerm' <- compileMetaObject scope slTerm
	slType2 <- case R.reduceMetaType (R.typeOfMetaObject slTerm') of
		R.MTSLTerm ty -> return ty
		otherType -> fail (msgPrefix ++ "the SL equivalent term is given as " ++ formatMOForMessage slTerm' ++
			" (at " ++ formatRange (TL.tagOfMetaObject slTerm) ++ "), which has meta-type " ++
			formatMTForMessage otherType ++ ", but it's supposed to have meta-type `(sl-term ...)`.")
	unless (R.equivalentMetaObjects (R.reduceMetaObject slType1) (R.reduceMetaObject slType2)) $
		fail (msgPrefix ++ "the JavaScript type is " ++ formatMOForMessage jsType' ++ " (at " ++
			formatRange (TL.tagOfMetaObject jsType) ++ "), which has SL equivalent " ++
			CSL.formatTypeForMessage slType1 ++ ", and the SL term equivalent is " ++ formatMOForMessage slTerm' ++
			" (at " ++ formatRange (TL.tagOfMetaObject slTerm) ++ "), which has SL type " ++
			CSL.formatTypeForMessage slType2 ++ ". The SL equivalent of the JavaScript type is supposed to be the \
			\same as the SL type of the SL equivalent term.")
	return (R.MTJSExpr jsType' slTerm')

compileMetaObject :: Scope -> TL.MetaObject Range -> LocalCompileMonad R.MetaObject

compileMetaObject scope (TL.MOApp range fun arg) = do
	let msgPrefix = "in application of function at " ++ formatRange (TL.tagOfMetaObject fun) ++ " to argument at " ++
			formatRange (TL.tagOfMetaObject arg) ++ ": "
	fun' <- compileMetaObject scope fun
	arg' <- compileMetaObject scope arg
	paramType <- case R.reduceMetaType (R.typeOfMetaObject fun') of
		R.MTFun (_, paramType) _ -> return paramType
		otherType -> fail (msgPrefix ++ "the function has type " ++ formatMTForMessage otherType ++ ", so it cannot \
			\be used as a function.")
	unless (R.typeOfMetaObject arg' `R.equivalentMetaTypes` paramType) $
		fail (msgPrefix ++ "the function expects an argument of type " ++ formatMTForMessage paramType ++ ", but the \
			\argument has type " ++ formatMTForMessage (R.typeOfMetaObject arg') ++ ".")
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
	Just (MetaObjectInScopeCantTransfer type_ loopBreakerRange) ->
		fail ("variable `" ++ TL.unName name ++ "` was defined outside of the `(js-loop-breaker ...)` construct at " ++
			formatRange loopBreakerRange ++ ", so it's illegal to use it at " ++ formatRange range ++", because it \
			\has type " ++ formatMTForMessage type_ ++ ". Only variables of type `(sl-type ...)`, `(sl-term ...)`, \
			\`(js-type ...)`, or `(js-term ...)` can be transferred from outside of a `(js-loop-breaker ...)` to \
			\inside.")
	Nothing ->
		fail ("at " ++ formatRange range ++ ": name `" ++ TL.unName name ++ "` is not in scope")

compileMetaObject scope (TL.MOSLTypeLiteral range code typeBindings) = do
	typeBindings' <- compileSLTypeBindings
		("in `(sl-type ...)` literal at " ++ formatRange range ++ ": ")
		scope typeBindings
	embedEither $ CSL.compileSLType
		(M.union typeBindings' (CSL.typesInScope $ slObjectsInScope $ scope))
		[] code

compileMetaObject scope (TL.MOSLTermLiteral range code typeBindings termBindings) = do
	typeBindings' <- compileSLTypeBindings
		("in `(sl-term ...)` literal at " ++ formatRange range ++ ": ")
		scope typeBindings
	termBindings' <- compileSLTermBindings
		("in `(sl-term ...)` literal at " ++ formatRange range ++ ": ")
		scope termBindings
	let slScope' = (slObjectsInScope scope) {
			CSL.typesInScope = typeBindings' `M.union` (CSL.typesInScope $ slObjectsInScope scope),
			CSL.termsInScope = termBindings' `M.union` (CSL.termsInScope $ slObjectsInScope scope)
			} 
	embedEither $ CSL.compileSLTerm slScope' [] code

compileMetaObject scope (TL.MOJSExprLiteral range equiv type_ expr bindings) = do
	let msgPrefix = "in `(js-expr ...)` literal at " ++ formatRange range ++ ": "
	equiv' <- compileMetaObject scope equiv
	slType1 <- case R.reduceMetaType (R.typeOfMetaObject equiv') of
		R.MTSLTerm ty -> return ty
		otherType -> fail (msgPrefix ++ "the SL equivalent given at " ++ formatRange (TL.tagOfMetaObject equiv) ++
			" has meta-type " ++ formatMTForMessage otherType ++ ", but it should have meta-type `(sl-term ...)`.")
	type_' <- compileMetaObject scope type_
	slType2 <- case R.reduceMetaType (R.typeOfMetaObject type_') of
		R.MTJSExprType equiv -> return equiv
		otherType -> fail (msgPrefix ++ "the JavaScript type given at " ++ formatRange (TL.tagOfMetaObject equiv) ++
			" has meta-type " ++ formatMTForMessage otherType ++ ", but it should have meta-type \
			\`(js-expr-type ...)`.")
	unless (slType1 `R.equivalentMetaObjects` slType2) $
		fail (msgPrefix ++ "the SL equivalent given at " ++ formatRange (TL.tagOfMetaObject equiv) ++ " has SL \
			\type " ++ CSL.formatTypeForMessage slType1 ++ ", but the JavaScript type given at " ++
			formatRange (TL.tagOfMetaObject equiv) ++ " has SL equivalent type " ++
			CSL.formatTypeForMessage slType2 ++ ". Those are supposed to be the same, but they aren't.")
	bindings' <- compileJSExprBindings msgPrefix scope bindings
	return (R.MOJSExprLiteral equiv' type_' (JS.removeAnnotations expr) bindings')

compileMetaObject scope (TL.MOJSExprLoopBreak range equiv type_ content) = do
	let msgPrefix = "in `(js-loop-break ...)` construct at " ++ formatRange range ++ ": "
	equiv' <- compileMetaObject scope equiv
	slType1 <- case R.reduceMetaType (R.typeOfMetaObject equiv') of
		R.MTSLTerm ty -> return ty
		otherType -> fail (msgPrefix ++ "the SL equivalent given at " ++ formatRange (TL.tagOfMetaObject equiv) ++
			" has meta-type " ++ formatMTForMessage otherType ++ ", but it should have meta-type `(sl-term ...)`.")
	type_' <- compileMetaObject scope type_
	slType2 <- case R.reduceMetaType (R.typeOfMetaObject type_') of
		R.MTJSExprType equiv -> return equiv
		otherType -> fail (msgPrefix ++ "the JavaScript type given at " ++ formatRange (TL.tagOfMetaObject equiv) ++
			" has meta-type " ++ formatMTForMessage otherType ++ ", but it should have meta-type \
			\`(js-expr-type ...)`.")
	unless (slType1 `R.equivalentMetaObjects` slType2) $
		fail (msgPrefix ++ "the SL equivalent given at " ++ formatRange (TL.tagOfMetaObject equiv) ++ " has SL \
			\type " ++ CSL.formatTypeForMessage slType1 ++ ", but the JavaScript type given at " ++
			formatRange (TL.tagOfMetaObject equiv) ++ " has SL equivalent type " ++
			CSL.formatTypeForMessage slType2 ++ ". Those are supposed to be the same, but they aren't.")
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
				R.MTFun _ _ -> MetaObjectInScopeCantTransfer runType range
				_ -> MetaObjectInScopeLocal runName runType
			processVar name (MetaObjectInScopeCantTransfer runType loopBreakerRange) =
				MetaObjectInScopeCantTransfer runType loopBreakerRange
			scope' = scope { metaObjectsInScope = M.mapWithKey processVar (metaObjectsInScope scope) }

		(content', subLoopBreakers) <- runWriterT $ compileMetaObject scope' content
		unless (R.typeOfMetaObject content' `R.equivalentMetaTypes` R.MTJSExpr type_' equiv') $
			fail (msgPrefix ++ "content at " ++ formatRange (TL.tagOfMetaObject content) ++ " has meta-type " ++
				formatMTForMessage (R.typeOfMetaObject content') ++ ", but it's supposed to have meta-type " ++
				formatMTForMessage (R.MTJSExpr type_' equiv') ++ ".")

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
                   -> ([(R.NameOfMetaObject, R.MetaType)] -> Scope -> LocalCompileMonad a)
                   -> ((R.NameOfMetaObject, R.MetaType) -> a -> a)
                   -> LocalCompileMonad a
compileAbstraction scope [] base fun = base [] scope
compileAbstraction scope ((paramName, paramType):params) base fun = do
	let paramName' = R.NameOfMetaObject (TL.unName paramName)
	paramType' <- compileMetaType scope paramType
	let scope' = scope { metaObjectsInScope = M.insert paramName (MetaObjectInScopeLocal paramName' paramType') (metaObjectsInScope scope) }
	let base' = base . ((paramName', paramType'):)
	rest <- compileAbstraction scope' params base' fun
	return (fun (paramName', paramType') rest)

compileBindings :: Ord n
                => Scope
                -> (Scope -> TL.Binding Range n -> TL.BindingParam Range -> LocalCompileMonad (Scope, p))
                -> (Scope -> TL.Binding Range n -> [p] -> TL.MetaObject Range -> LocalCompileMonad a)
                -> [TL.Binding Range n]
                -> LocalCompileMonad (M.Map n a)
compileBindings scope paramFun valueFun bindings = do
	bindings' <- sequence [do
		let
			f scope' [] params' =
				valueFun scope' binding params' (TL.valueOfBinding binding)
			f scope' (param:params) paramsSoFar' = do
				(scope'', param') <- paramFun scope' binding param
				f scope'' params (paramsSoFar' ++ [param'])
		value' <- f scope (TL.paramsOfBinding binding) []
		return (TL.nameOfBinding binding, value')
		| binding <- bindings]
	-- TODO: Check for duplicates
	return (M.fromList bindings')

compileSLTypeBindings :: String
                      -> Scope
                      -> [TL.Binding Range SL.NameOfType]
                      -> LocalCompileMonad (M.Map SL.NameOfType CSL.TypeInScope)
compileSLTypeBindings msgPrefix1 scope bindings = compileBindings scope
	(\ subScope binding (TL.BindingParam paramRange parts) -> do
		let msgPrefix2 = msgPrefix1 ++ "in binding of name `" ++ SL.unNameOfType (TL.nameOfBinding binding) ++
			"` (at " ++ formatRange (TL.tagOfBinding binding) ++ "): "
		(name, type_) <- case parts of
			[(name, type_)] -> return (name, type_)
			_ -> fail (msgPrefix2 ++ "parameter at " ++ formatRange paramRange ++ " has two or more parts, which is \
				\illegal.")
		let name' = R.NameOfMetaObject (TL.unName name)
		type_' <- compileMetaType scope type_
		slKind' <- case type_' of
			R.MTSLType slKind' -> return slKind'
			_ -> fail (msgPrefix2 ++ "parameter `" ++ TL.unName name ++ "` (at " ++ formatRange paramRange ++ ") has \
				\meta-type " ++ formatMTForMessage type_' ++ " (at " ++ formatRange (TL.tagOfMetaType type_) ++ "), \
				\but all parameters in a `(sl-type ...)` literal should have meta-type `(sl-type ...)`.")
		let subScope' = subScope {
			metaObjectsInScope = M.insert name (MetaObjectInScopeLocal name' type_') (metaObjectsInScope subScope)
			}
		return (subScope', (name', slKind'))
		)
	(\ subScope' binding params' value -> do
		value' <- compileMetaObject subScope' value
		case R.typeOfMetaObject value' of
			R.MTSLType _ -> return ()
			otherType -> fail (msgPrefix1 ++ "in binding of name `" ++ SL.unNameOfType (TL.nameOfBinding binding) ++
				"` (at " ++ formatRange (TL.tagOfBinding binding) ++ ": value at " ++
				formatRange (TL.tagOfMetaObject value) ++ " has meta-type " ++ formatMTForMessage otherType ++ ", but \
				\it's supposed to have meta-type `(sl-type ...)`.")
		return (CSL.TypeInScope {
			CSL.typeParamsOfTypeInScope = map snd params',
			CSL.valueOfTypeInScope = \ paramValues -> let
				metaObjectSubs = M.fromList (zip (map fst params') paramValues)
				in R.substituteMetaObject (R.Substitutions metaObjectSubs M.empty M.empty) value'
			})
		)
	bindings

compileSLTermBindings :: String
                      -> Scope
                      -> [TL.Binding Range SL.NameOfTerm]
                      -> LocalCompileMonad (M.Map SL.NameOfTerm CSL.TermInScope)
compileSLTermBindings msgPrefix1 scope bindings = compileBindings scope
	(\ subScope binding (TL.BindingParam paramRange parts) -> do
		let msgPrefix2 = msgPrefix1 ++ "in binding of name `" ++ SL.unNameOfTerm (TL.nameOfBinding binding) ++
			"` (at " ++ formatRange (TL.tagOfBinding binding) ++ "): "
		(name, type_) <- case parts of
			[(name, type_)] -> return (name, type_)
			_ -> fail (msgPrefix2 ++ "parameter at " ++ formatRange paramRange ++ " has two or more parts, which is \
				\illegal.")
		let name' = R.NameOfMetaObject (TL.unName name)
		type_' <- compileMetaType scope type_
		slKindOrType' <- case R.reduceMetaType type_' of
			R.MTSLType slKind' -> return (Left slKind')
			R.MTSLTerm slType' -> return (Right slType')
			otherType -> fail (msgPrefix2 ++ "parameter `" ++ TL.unName name ++ "` (at " ++ formatRange paramRange ++
				") has meta-type " ++ formatMTForMessage type_' ++ ", but all parameters in a `(sl-term ...)` literal \
				\are supposed to have meta-type `(sl-type ...)` or `(sl-term ...)`.")
		let subScope' = subScope {
			metaObjectsInScope =	M.insert name (MetaObjectInScopeLocal name' type_') (metaObjectsInScope subScope)
			}
		return (subScope', (name', slKindOrType'))
		)
	(\ subScope' binding params' value -> do
		let msgPrefix2 = msgPrefix1 ++ "in binding of name `" ++ SL.unNameOfTerm (TL.nameOfBinding binding) ++
			"` (at " ++ formatRange (TL.tagOfBinding binding) ++ "): "
		let
			isLeft :: Either l r -> Bool
			isLeft (Left _) = True
			isLeft (Right _) = False
		when (any (isLeft . snd) $ dropWhile (isLeft . snd) params') $
			fail (msgPrefix2 ++ "all type-parameters must come before all term-parameters.")
		let typeParams' = [(name, slKind') | (name, Left slKind') <- params']
		let termParams' = [(name, slType') | (name, Right slType') <- params']
		value' <- compileMetaObject subScope' value
		case R.typeOfMetaObject value' of
			R.MTSLTerm _ -> return ()
			otherType -> fail ("value at " ++ formatRange (TL.tagOfMetaObject value) ++ " has meta-type " ++
				formatMTForMessage otherType ++ ", but it's supposed to have meta-type `(sl-term ...)`.")
		return (CSL.TermInScope {
			CSL.typeParamsOfTermInScope = map snd typeParams',
			CSL.termParamsOfTermInScope = map snd termParams',
			CSL.valueOfTermInScope = \ typeParamValues termParamValues -> let
				metaObjectSubs = M.fromList (zip (map fst typeParams') typeParamValues ++ zip (map fst termParams') termParamValues)
				in R.substituteMetaObject (R.Substitutions metaObjectSubs M.empty M.empty) value'
			})
		)
	bindings

compileJSExprBindings :: String
                      -> Scope
                      -> [TL.Binding Range (JS.Id ())]
                      -> LocalCompileMonad (M.Map (JS.Id ()) R.JSExprBinding)
compileJSExprBindings msgPrefix1 scope bindings = compileBindings scope
	(\ subScope binding (TL.BindingParam paramRange parts) -> do
		let msgPrefix2 = msgPrefix1 ++ "in binding of name `" ++ JS.unId (TL.nameOfBinding binding) ++ "` (at " ++
			formatRange (TL.tagOfBinding binding) ++ "): "
		
		(name1, type1, name2, type2) <- case parts of
			[(name1, type1), (name2, type2)] -> return (name1, type1, name2, type2)
			_ -> fail (msgPrefix2 ++ "JavaScript expression binding parameters are supposed to have two parts, like \
				\`(x :: sl-term ... | y :: js-expr ... x)`.")

		let msgPrefix3 = msgPrefix2 ++ "in parameter pair `" ++ TL.unName name1 ++ "` and `" ++ TL.unName name2 ++
			"` (at " ++ formatRange paramRange ++ "): "

		let name1' = R.NameOfMetaObject (TL.unName name1)
		type1' <- compileMetaType scope type1
		slType <- case R.reduceMetaType type1' of
			R.MTSLTerm ty -> return ty
			otherType -> fail (msgPrefix3 ++ "the meta-type of the first parameter `" ++ TL.unName name1 ++ "` is \
				\given as " ++ formatMTForMessage otherType ++ " (at " ++ formatRange (TL.tagOfMetaType type1) ++
				"), but it is supposed to be something of the form `(sl-term ...)`.")

		let scopeForType2 = scope { metaObjectsInScope =
			M.insert name1 (MetaObjectInScopeLocal name1' type1') $
			metaObjectsInScope scope
			}
		let name2' = R.NameOfMetaObject (TL.unName name2)
		type2' <- compileMetaType scopeForType2 type2
		jsType <- case R.reduceMetaType type2' of
			R.MTJSExpr jsType (R.MOName n _) | n == name1' -> return jsType
			otherType -> fail (msgPrefix3 ++ "the meta-type of the second parameter `" ++ TL.unName name2 ++ "` is \
				\given as " ++ formatMTForMessage otherType ++ " (at " ++ formatRange (TL.tagOfMetaType type2) ++
				"), but it is supposed to be something of the form `(js-expr ... " ++ TL.unName name1 ++ ")`.")

		let subScope' = subScope { metaObjectsInScope =
			M.insert name2 (MetaObjectInScopeLocal name2' type2') $
			M.insert name1 (MetaObjectInScopeLocal name1' type1') $
			metaObjectsInScope subScope
			}

		return (subScope', R.JSExprBindingParam name1' slType name2' jsType)
		)

	(\ subScope' binding params' value -> do
		let msgPrefix2 = msgPrefix1 ++ "in binding of name `" ++ JS.unId (TL.nameOfBinding binding) ++ "` (at " ++
			formatRange (TL.tagOfBinding binding) ++ "): "
		value' <- compileMetaObject subScope' value
		case R.typeOfMetaObject value' of
			R.MTJSExpr _ _ -> return ()
			otherType -> fail (msgPrefix2 ++ "value at " ++ formatRange (TL.tagOfMetaObject value) ++ " has \
				\meta-type " ++ formatMTForMessage otherType ++ ", but it's supposed to have meta-type \
				\`(js-expr ...)`.")
		return (R.JSExprBinding params' value')
		)

	bindings

data GlobalResults = GlobalResults {
	slDefnsOfGlobalResults :: CSL.Defns,
	tlDefnsOfGlobalResults :: M.Map TL.Name R.MetaObject,
	emitsOfGlobalResults :: [JS.Statement ()]
	}

compileDirectives :: [TL.Directive Range] -> ErrorMonad GlobalResults
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
		Data.Graph.CyclicSCC _ -> lift $ fail ("mutual recursion detected")
		| scc <- Data.Graph.stronglyConnComp unsortedDefnDirs]

	(tlDefns, loopBreakersFromDefnDirs) <- runWriterT $ do

		let
			compileDefnDir :: Scope -> TL.Directive Range -> LocalCompileMonad R.MetaObject
			compileDefnDir scope (TL.DLet range _ params maybeType value) =
				compileAbstraction scope params (\_ scope' -> do
					value' <- compileMetaObject scope' value
					case maybeType of
						Nothing -> return ()
						Just type_ -> do
							type_' <- compileMetaType scope' type_
							unless (R.typeOfMetaObject value' `R.equivalentMetaTypes` type_') $
								fail ("in `(let ...)` directive at " ++ formatRange range ++ ": the type signature \
									\given at " ++ formatRange (TL.tagOfMetaType type_) ++ " says the type should \
									\be " ++ formatMTForMessage type_' ++ ", but the actual value at " ++
									formatRange (TL.tagOfMetaObject value) ++ " has type " ++
									formatMTForMessage (R.typeOfMetaObject value') ++ ".")
					return value'
					) R.MOAbs
			compileDefnDir scope (TL.DJSExprType range name params slEquiv) =
				compileAbstraction scope params (\params' scope' -> do
					slEquiv' <- compileMetaObject scope' slEquiv
					case R.typeOfMetaObject slEquiv' of
						R.MTSLType R.SLKindType -> return ()
						otherType -> fail ("in `(js-expr-type ...)` directive at " ++ formatRange range ++ ": the SL \
							\equivalent given at " ++ formatRange (TL.tagOfMetaObject slEquiv) ++ " has meta-type " ++
							formatMTForMessage otherType ++ ", but it's supposed to have meta-type `(sl-type ...)`.")
					let defn = R.JSExprTypeDefn {
						R.nameOfJSExprTypeDefn = R.NameOfMetaObject (TL.unName name),
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
			bindings' <- compileJSExprBindings
				("in `(js-emit ...)` directive at " ++ formatRange range ++ ": ")
				scope bindings
			let substs = M.map (\binding ->
				case R.tryReduceJSExprBindingToJSSubst S.empty binding of
					Just f -> f M.empty
					Nothing -> error "cannot reduce binding, for no good reason"
				) bindings'
			return $ map (JS.substituteStatement substs M.empty . JS.removeAnnotations) code
			| TL.DJSEmit range code bindings <- directives]

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

