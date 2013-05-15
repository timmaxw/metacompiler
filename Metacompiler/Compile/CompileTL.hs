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
import qualified Metacompiler.TL.FreeNames as TL
import qualified Metacompiler.TL.Syntax as TL

data Scope = Scope {
	metaObjectsInScope :: M.Map TL.Name R.MetaObject,
	slObjectsInScope :: CSL.Scope
	}

formatMTForMessage :: R.MetaType -> String
formatMTForMessage t = "`" ++ FTL.formatMetaTypeAsString t ++ "`"

formatMOForMessage :: R.MetaObject -> String
formatMOForMessage o = "`" ++ FTL.formatMetaObjectAsString o ++ "`"

compileMetaType :: Scope -> TL.MetaType Range -> ErrorMonad R.MetaType
compileMetaType scope (TL.MTFun range params result) =
	compileAbstraction scope params (\_ scope' -> compileMetaType scope' result) R.MTFun
compileMetaType scope (TL.MTSLType range slKind) = do
	slKind' <- CSL.compileSLKind slKind
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

compileMetaObject :: Scope -> TL.MetaObject Range -> ErrorMonad R.MetaObject

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
	Just x -> return x
	Nothing -> fail ("at " ++ formatRange range ++ ": name `" ++ TL.unName name ++ "` is not in scope")

compileMetaObject scope (TL.MOSLTypeLiteral range type_ typeBindings) = do
	let msgPrefix = "in `(sl-type ...)` literal at " ++ formatRange range ++ ": "
	typeBindings' <- compileSLTypeBindings msgPrefix scope typeBindings
	let typeScopeAdditions = M.mapWithKey (\ (SL.NameOfType name) value -> case R.typeOfMetaObject value of
		R.MTSLType kind -> CSL.NamedTypeInScope (SLR.NameOfType name) kind
		_ -> error "binding should have type (sl-type ...)"
		) typeBindings'
	type_' <- errorContext msgPrefix $
		CSL.compileSLType
			(typeBindingsInSLScope `M.union` CSL.typesInScope (slObjectsInScope scope))
			type_
	return (R.MOSLType type_' typeBindings')

compileMetaObject scope (TL.MOSLTermLiteral range term typeBindings termBindings) = do
	let msgPrefix = "in `(sl-term ...)` literal at " ++ formatRange range ++ ": "
	typeBindings' <- compileSLTypeBindings msgPrefix scope typeBindings
	termBindings' <- compileSLTermBindings msgPrefix scope termBindings
	let
		termScopeMaker :: State
				(M.Map SLR.NameOfType R.SLTypeBinding)
				(M.Map SL.NameOfTerm CSL.TermInScope)
		termScopeMaker = liftM M.fromList $ sequence [do
			let
				internType :: R.MetaObject
				           -> SLR.NameOfType
				           -> State (M.Map SLR.NameOfType R.SLTypeBinding) SLR.SLType
				internType suggestedName typeAsMO = case R.reduceMetaObject typeAsMO of
					R.MOSLType typeAsSL bindings -> do
						subs <- liftM M.fromList $ sequence [do
							value' <- internType name value
							return (TypeSub (Identity .foldl SLR.SLTypeApp value'))
							| (name, R.SLTypeBinding value) <- M.toList bindings]
						return (runIdentity (substituteSLType subs typeAsSL))
					other -> do
						existingBindings <- get
						name' <- case find (\(_, v) -> R.equivalentMetaObjects v other) (M.toList existingBindings) of
							Nothing -> do
								let forbiddenNames = M.keysSet existingBindings
										`S.union` S.fromList [case tis of
											CSL.NamedTypeInScope name _ -> name
											CSL.DefinedTypeInScope defn -> nameOfSLDataDefn defn
											| (_, tis) <- CSL.typesInScope (slObjectsInScope scope)]
								let candidateNames =
									[SLR.NameOfType (SLR.unNameOfType suggestedName ++ replicate i '\'') | i <- [0..]]
								let Just name' = find (`S.notMember` forbiddenNames) candidateNames
								put (M.insert name' (R.SLTypeBinding other) existingBindings)
								return name'
							Just (n, _) -> return n
						let kind = case R.typeOfMetaObject other of
								R.MTSLType k -> k
								_ -> error "this should have type (sl-type ...)"
						return (SLR.SLTypeName name' kind)
			paramTypes <- sequence [
				internType type_ (SLR.NameOfType (name ++ "Type"))
				| (R.NameOfMetaObject name, type_) <- params]
			valueType <- case R.typeOfMetaObject value of
				R.MTSLTerm type_ -> internType value suggestedName
					where suggestedName = SLR.NameOfType (SLR.unNameOfTerm name ++ "Type")
				_ -> error "this should have type (sl-term ...)"
			let wholeType = foldr SLR.SLTermFun valueType paramTypes
			return (name, CSL.NameTermInScope (SLR.NameOfTerm (SL.unNameOfTerm name)) wholeType)
			| (name, SLTermBinding params value) <- M.toList termBindings']
	let (termScopeAdditions, typeBindings'') = runState termScopeMaker typeBindings'
	let typeScopeAdditions = M.mapWithKey (\ (SL.NameOfType name) value -> case R.typeOfMetaObject value of
		R.MTSLType kind -> CSL.NamedTypeInScope (SLR.NameOfType name) kind
		_ -> error "binding should have type (sl-type ...)"
		) typeBindings''
	term' <- errorContext msgPrefix $
		CSL.compileSLTerm
			(slObjectsInScope scope) {
				CSL.typesInScope = typeBindings'' `M.union` (CSL.typesInScope $ slObjectsInScope scope),
				CSL.termsInScope = termScopeAdditions `M.union` (CSL.termsInScope $ slObjectsInScope scope)
				} 
			term
	return (R.MOSLTerm term' typeBindings'' termBindings')

compileMetaObject scope (TL.MOJSExprLiteral range equiv type_ expr bindings) = do
	let msgPrefix = "in `(js-expr ...)` literal at " ++ formatRange range ++ ": "
	equiv' <- compileMetaObject scope equiv
	slType1 <- case R.reduceMetaType (R.typeOfMetaObject equiv') of
		R.MTSLTerm ty -> return ty
		otherType -> fail (msgPrefix ++ "the SL equivalent given at " ++ formatRange (TL.tagOfMetaObject equiv) ++
			" has meta-type " ++ formatMTForMessage otherType ++ ", but it should have meta-type `(sl-term ...)`.")
	type_' <- compileMetaObject scope type_
	slType2 <- case R.reduceMetaType (R.typeOfMetaObject type_') of
		R.MTJSExprType ty -> return ty
		otherType -> fail (msgPrefix ++ "the JavaScript type given at " ++ formatRange (TL.tagOfMetaObject type_) ++
			" has meta-type " ++ formatMTForMessage otherType ++ ", but it should have meta-type \
			\`(js-expr-type ...)`.")
	unless (slType1 `R.equivalentMetaObjects` slType2) $
		fail (msgPrefix ++ "the SL equivalent given at " ++ formatRange (TL.tagOfMetaObject equiv) ++ " has SL \
			\type " ++ CSL.formatTypeForMessage slType1 ++ ", but the JavaScript type given at " ++
			formatRange (TL.tagOfMetaObject type_) ++ " has SL equivalent type " ++
			CSL.formatTypeForMessage slType2 ++ ". Those are supposed to be the same, but they aren't.")
	bindings' <- compileJSExprBindings msgPrefix scope bindings
	return (R.MOJSExprLiteral equiv' type_' (JS.removeAnnotations expr) bindings')

compileMetaObject scope (TL.MOJSExprConvertEquiv range inEquiv outEquiv content) = do
	let msgPrefix = "in `(js-expr-convert-equiv ...)` construct at " ++ formatRange range ++ ": "
	inEquiv' <- compileMetaObject scope inEquiv
	slType1 <- case R.reduceMetaType (R.typeOfMetaObject inEquiv') of
		R.MTSLTerm ty -> return ty
		otherType -> fail (msgPrefix ++ "the `in-equiv` given at " ++ formatRange (TL.tagOfMetaObject inEquiv) ++
			" has meta-type " ++ formatMTForMessage otherType ++ ", but it should have meta-type `(sl-term ...)`.")
	outEquiv' <- compileMetaObject scope outEquiv
	slType2 <- case R.reduceMetaType (R.typeOfMetaObject outEquiv') of
		R.MTSLTerm ty -> return ty
		otherType -> fail (msgPrefix ++ "the `out-equiv` given at " ++ formatRange (TL.tagOfMetaObject outEquiv) ++
			" has meta-type " ++ formatMTForMessage otherType ++ ", but it should have meta-type `(sl-term ...)`.")
	unless (slType1 `R.equivalentMetaObjects` slType2) $
		fail (msgPrefix ++ "the `in-equiv` given at " ++ formatRange (TL.tagOfMetaObject inEquiv) ++ " has SL \
			\type " ++ CSL.formatTypeForMessage slType1 ++ ", but the `out-equiv` given at " ++
			formatRange (TL.tagOfMetaObject outEquiv) ++ " has SL type " ++ CSL.formatTypeForMessage slType2 ++
			". Because they have different types, they can't possibly be equivalent expressions.")
	content' <- compileMetaObject scope content
	case R.reduceMetaType (R.typeOfMetaObject content') of
		R.MTJSExpr _ equiv ->
			unless (equiv `R.equivalentMetaObjects` inEquiv') $
				fail (msgPrefix ++ "the `content` given at " ++ formatRange (TL.tagOfMetaObject content) ++
					" has SL equivalent " ++ CSL.formatTermForMessage equiv ++ ", but the `in-equiv` given at " ++
					formatRange (TL.tagOfMetaObject inEquiv) ++ " is " ++ CSL.formatTermForMessage inEquiv' ++ ".")
		otherType -> fail (msgPrefix ++ "the `content` given at " ++ formatRange (TL.tagOfMetaObject content) ++
			" has meta-type " ++ formatMTForMessage otherType ++ ", but it should have meta-type `(js-expr ...)`.")
	return $ R.MOJSExprConvertEquiv outEquiv' content'

compileAbstraction :: Scope
                   -> [(TL.Name, TL.MetaType Range)]
                   -> ([(R.NameOfMetaObject, R.MetaType)] -> Scope -> ErrorMonad a)
                   -> ((R.NameOfMetaObject, R.MetaType) -> a -> a)
                   -> ErrorMonad a
compileAbstraction scope [] base fun = base [] scope
compileAbstraction scope ((paramName, paramType):params) base fun = do
	let paramName' = R.NameOfMetaObject (TL.unName paramName)
	paramType' <- compileMetaType scope paramType
	let scope' = scope { metaObjectsInScope = M.insert paramName (R.MOName paramName' paramType') (metaObjectsInScope scope) }
	let base' = base . ((paramName', paramType'):)
	rest <- compileAbstraction scope' params base' fun
	return (fun (paramName', paramType') rest)

compileBindings :: Ord n
                => Scope
                -> (Scope -> TL.Binding Range n -> TL.BindingParam Range -> ErrorMonad (Scope, p))
                -> (Scope -> TL.Binding Range n -> [p] -> TL.MetaObject Range -> ErrorMonad a)
                -> [TL.Binding Range n]
                -> ErrorMonad (M.Map n a)
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
                      -> ErrorMonad (M.Map SL.NameOfType R.SLTypeBinding)
compileSLTypeBindings msgPrefix1 scope bindings = compileBindings scope
	(\ subScope binding (TL.BindingParam paramRange parts) -> do
		let msgPrefix2 = msgPrefix1 ++ "in binding of name `" ++ SL.unNameOfType (TL.nameOfBinding binding) ++
			"` (at " ++ formatRange (TL.tagOfBinding binding) ++ "): "
		fail (msgPrefix2 ++ "type bindings are not allowed to have parameters")
		)
	(\ subScope binding [] value -> do
		value' <- compileMetaObject subScope value
		case R.typeOfMetaObject value' of
			R.MTSLType _ -> return (R.SLTypeBinding value')
			otherType -> fail (msgPrefix1 ++ "in binding of name `" ++ SL.unNameOfType (TL.nameOfBinding binding) ++
				"` (at " ++ formatRange (TL.tagOfBinding binding) ++ ": value at " ++
				formatRange (TL.tagOfMetaObject value) ++ " has meta-type " ++ formatMTForMessage otherType ++ ", but \
				\it's supposed to have meta-type `(sl-type ...)`.")
		)
	bindings

compileSLTermBindings :: String
                      -> Scope
                      -> [TL.Binding Range SL.NameOfTerm]
                      -> ErrorMonad (M.Map SL.NameOfTerm R.SLTermBinding)
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
		slType' <- case R.reduceMetaType type_' of
			R.MTSLTerm slType' -> return slType'
			otherType -> fail (msgPrefix2 ++ "parameter `" ++ TL.unName name ++ "` (at " ++ formatRange paramRange ++
				") has meta-type " ++ formatMTForMessage type_' ++ ", but all parameters in a `(sl-term ...)` literal \
				\are supposed to have meta-type `(sl-term ...)`.")
		let subScope' = subScope {
			metaObjectsInScope = M.insert name (R.MOName name' type_') (metaObjectsInScope subScope)
			}
		return (subScope', (name', slType'))
		)
	(\ subScope' binding params' value -> do
		let msgPrefix2 = msgPrefix1 ++ "in binding of name `" ++ SL.unNameOfTerm (TL.nameOfBinding binding) ++
			"` (at " ++ formatRange (TL.tagOfBinding binding) ++ "): "
		value' <- compileMetaObject subScope' value
		case R.typeOfMetaObject value' of
			R.MTSLTerm _ -> return (R.SLTermBinding params' value')
			otherType -> fail ("value at " ++ formatRange (TL.tagOfMetaObject value) ++ " has meta-type " ++
				formatMTForMessage otherType ++ ", but it's supposed to have meta-type `(sl-term ...)`.")
		)
	bindings

compileJSExprBindings :: String
                      -> Scope
                      -> [TL.Binding Range (JS.Id ())]
                      -> ErrorMonad (M.Map (JS.Id ()) R.JSExprBinding)
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
			M.insert name1 (R.MOName name1' type1') $
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
			M.insert name2 (R.MOName name2' type2') $
			M.insert name1 (R.MOName name1' type1') $
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
compileDirectives directives = do

	let allSLDirs = concat [content | TL.DSLCode _ content <- directives]
	slDefns <- CSL.compileSLDirectives allSLDirs

	-- `let`, `js-expr-type`, and `js-expr-global` directives are collectively referred to as "definition directives"
	-- because they introduce new names into scope. They also both may refer to names already in scope; this means that
	-- they have to be top-sorted before processing them.
	let unsortedDefnDirs = concat [case dir of
			TL.DLet _ name params type_ value -> [((name, dir), name, S.toList freeNames)]
				where freeNames = TL.freeNamesInAbstraction params $
						maybe S.empty TL.freeNamesInMetaType type_
						`S.union` TL.freeNamesInMetaObject value
			TL.DJSExprType _ name params spec -> [((name, dir), name, S.toList freeNames)]
				where freeNames = TL.freeNamesInAbstraction params $
						TL.freeNamesInMetaObject spec
			TL.DJSExprGlobal _ name params _ type_ spec _ -> [((name, dir), name, S.toList freeNames)]
				where freeNames = TL.freeNamesInAbstraction params $
						TL.freeNamesInMetaObject type_
						`S.union` TL.freeNamesInMetaObject spec
			_ -> []
			| dir <- directives]
	sortedDefnDirs <- sequence [case scc of
		Data.Graph.AcyclicSCC (name, dir) -> return (name, dir)
		Data.Graph.CyclicSCC things -> fail ("the following top-level directive(s) form an illegal recursive loop: " ++ 
			Data.List.intercalate ", "
				["`" ++ TL.unName name ++ "` (at " ++ formatRange (TL.tagOfDirective dir) ++ ")"
				| (name, dir) <- things] ++ ".")
		| scc <- Data.Graph.stronglyConnComp unsortedDefnDirs]

	let
		compileDefnDir :: Scope
		               -> TL.Directive Range
		               -> ErrorMonad (R.MetaObject, [Scope -> ErrorMonad [JS.Statement ()]])

		compileDefnDir scope (TL.DLet range _ params maybeType value) = do
			value <- compileAbstraction scope params (\_ scope' -> do
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
			return (value, [])

		compileDefnDir scope (TL.DJSExprType range name params slEquiv) = do
			value <- compileAbstraction scope params (\params' scope' -> do
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
			return (value, [])

		compileDefnDir scope dir@(TL.DJSExprGlobal range globalName params runtimeArgs type_ equiv body) = do
			let msgPrefix = "in `(js-expr-global ...)` directive named `" ++ TL.unName globalName ++ "` at " ++
					formatRange range ++ ": "
			compileAbstraction scope params (\params' scope' -> do
				equiv' <- compileMetaObject scope' equiv
				slType1 <- case R.reduceMetaType (R.typeOfMetaObject equiv') of
					R.MTSLTerm ty -> return ty
					otherType -> fail (msgPrefix ++ "the SL equivalent given at " ++ formatRange (TL.tagOfMetaObject equiv) ++
						" has meta-type " ++ formatMTForMessage otherType ++ ", but it should have meta-type `(sl-term ...)`.")
				type_' <- compileMetaObject scope' type_
				slType2 <- case R.reduceMetaType (R.typeOfMetaObject type_') of
					R.MTJSExprType equiv -> return equiv
					otherType -> fail (msgPrefix ++ "the JavaScript type given at " ++ formatRange (TL.tagOfMetaObject type_) ++
						" has meta-type " ++ formatMTForMessage otherType ++ ", but it should have meta-type \
						\`(js-expr-type ...)`.")
				unless (slType1 `R.equivalentMetaObjects` slType2) $
					fail (msgPrefix ++ "the SL equivalent given at " ++ formatRange (TL.tagOfMetaObject equiv) ++ " has SL \
						\type " ++ CSL.formatTypeForMessage slType1 ++ ", but the JavaScript type given at " ++
						formatRange (TL.tagOfMetaObject type_) ++ " has SL equivalent type " ++
						CSL.formatTypeForMessage slType2 ++ ". Those are supposed to be the same, but they aren't.")
				let paramTypesMap = M.fromList [(name, type_) | ((name, _), (_, type_)) <- zip params params']
				let paramNamesMap = M.fromList [(name, name') | ((name, _), (name', _)) <- zip params params']
				runtimeArgsTypesAndEquivs <- sequence [
					case M.lookup argName paramTypesMap of
						Nothing -> fail ("the name `" ++ TL.unName argName ++ "` appears in the `(args ...)` clause, \
							\but it is not the name of a parameter.")
						Just (R.MTJSExpr t e) -> return (t, e)
						Just otherType -> fail  ("the name `" ++ TL.unName argName ++ "` appears in the `(args ...)` \
							\clause, but that parameter has type " ++ formatMTForMessage otherType ++ ", and only \
							\JavaScript expressions can be passed to JavaScript functions.")
					| argName <- runtimeArgs]
				let runtimeArgsJSNames = [JS.Id () ("arg_" ++ show i ++ "_" ++ TL.unName name)
						| (name, i) <- zip runtimeArgs [1..]]
				unless (runtimeArgs == nub runtimeArgs) $
					fail (msgPrefix ++ "the following name(s) appear more than once in the `(args ...)` clause: " ++
						intercalate ", " (map TL.unName (nub ((Data.List.\\) runtimeArgs (nub runtimeArgs)))))
				let globalJSName = JS.Id () ("global_" ++ TL.unName globalName)
				let jsEquiv = JS.CallExpr ()
						(JS.VarRef () globalJSName)
						[JS.CallExpr () (JS.VarRef () jsName) [] | jsName <- runtimeArgsJSNames]
				let value = R.MOJSExprLiteral
					equiv' type_'
					jsEquiv
					(M.fromList [
						(jsName, R.JSExprBinding [] (R.MOName ((M.!) paramNamesMap name) ((M.!) paramTypesMap name)))
						| (name, jsName) <- zip runtimeArgs runtimeArgsJSNames
						])
				let globalFunctions = [\ scope'' -> do
					let scope''' = scope'' {
						metaObjectsInScope = M.fromList [
								(name, R.MOName ((M.!) paramNamesMap name) ((M.!) paramTypesMap name))
								| (name, _) <- params]
								`M.union` (metaObjectsInScope scope'')
						}
					body' <- compileMetaObject scope''' body
					unless (R.typeOfMetaObject body' `R.equivalentMetaTypes` R.MTJSExpr type_' equiv') $
						fail (msgPrefix ++ "the body has type " ++ formatMTForMessage (R.typeOfMetaObject body') ++
							" but the `(spec ...)` and `(type ...)` clauses specify that the type of the body should \
							\be " ++ formatMTForMessage (R.MTJSExpr type_' equiv') ++ ".")
					let runtimeArgsJSNames = [JS.Id () ("arg_" ++ show i ++ "_" ++ TL.unName name)
							| (name, i) <- zip runtimeArgs [1..]]
					let runtimeArgsMetaObjects = [R.MOJSExprLiteral e t (JS.VarRef () n) M.empty
							| ((e, t), n) <- zip runtimeArgsTypesAndEquivs runtimeArgsJSNames]
					let body'' = R.substituteMetaObject
							(R.Substitutions
								(M.fromList (zip
									[(M.!) paramNamesMap name | name <- runtimeArgs]
									runtimeArgsMetaObjects
									))
								M.empty M.empty)
							body'
					body''' <- case R.reduceMetaObject body'' of
						R.MOJSExprLiteral _ _ expr subs | M.null subs -> return expr
						_ -> fail (msgPrefix ++ "the body depends in an illegal way on a parameter that is not in \
							\`(args ...)` clause.")
					return [JS.FunctionStmt () globalJSName runtimeArgsJSNames [JS.ReturnStmt () (Just body''')]]
					]
				return (value, globalFunctions)
				)
				(\ (paramName, paramType) (value, globalFunctions) ->
					(R.MOAbs (paramName, paramType) value, globalFunctions)
					)

	(tlDefns, globalFunctions) <- foldM (\ (tlDefns, globalFunctions) (name, defnDir) -> do
		let scope = Scope
			(tlDefns `M.union`
				M.fromList [(name, error "loop, not caught by top-sort")
				| (name, _) <- sortedDefnDirs, name `M.notMember` tlDefns])
			(CSL.scopeForDefns slDefns)
		(value, newGlobalFunctions) <- compileDefnDir scope defnDir
		let tlDefns' = M.insert name value tlDefns
		let globalFunctions' = globalFunctions ++ newGlobalFunctions
		return (tlDefns', globalFunctions')
		)
		(M.empty, [])
		sortedDefnDirs

	let scope = Scope tlDefns (CSL.scopeForDefns slDefns)

	emitsFromJSExprGlobalDirs <- liftM concat $ mapM ($ scope) globalFunctions

	emitsFromJSEmitDirs <- liftM concat $ sequence [do
		bindings' <- compileJSExprBindings
			("in `(js-emit ...)` directive at " ++ formatRange range ++ ": ")
			scope bindings
		let substs = M.map (\binding ->
			case R.tryReduceJSExprBindingToJSSubst S.empty binding of
				Just f -> f M.empty
				Nothing -> let
					R.JSExprBinding _ value = binding
					in error ("for some reason, cannot reduce binding: " ++ formatMOForMessage value)
			) bindings'
		return $ map (JS.substituteStatement substs M.empty . JS.removeAnnotations) code
		| TL.DJSEmit range code bindings <- directives]

	-- Emits from `js-expr-global` directives must go before emits from `js-emit` directives so that user-supplied
	-- statements can access automatically-generated global definitions
	let allEmits = emitsFromJSExprGlobalDirs ++ emitsFromJSEmitDirs 

	return $ GlobalResults {
		slDefnsOfGlobalResults = slDefns,
		tlDefnsOfGlobalResults = tlDefns,
		emitsOfGlobalResults = allEmits
		}

