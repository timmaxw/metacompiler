module Metacompiler.TLCompile.Compile where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Graph
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Metacompiler.JS.JS as JS
import Metacompiler.Error
import qualified Metacompiler.SLCompile.Compile as SLC
import qualified Metacompiler.SLCompile.Format as SLF
import qualified Metacompiler.SLRuntime.Substitute as SLR
import qualified Metacompiler.SLRuntime.Types as SLR
import qualified Metacompiler.SLSyntax.Types as SLS
import qualified Metacompiler.TLCompile.Format as TLF
import qualified Metacompiler.TLRuntime.TLRuntime as TLR
import qualified Metacompiler.TLSyntax.FreeNames as TLS
import qualified Metacompiler.TLSyntax.Types as TLS

data Scope = Scope {
	metaObjectsInScope :: M.Map TLS.Name TLR.MetaObject,
	slObjectsInScope :: SLC.Scope
	}

formatMTForMessage :: TLR.MetaType -> String
formatMTForMessage t = "`" ++ TLF.formatMetaTypeAsString t ++ "`"

formatMOForMessage :: TLR.MetaObject -> String
formatMOForMessage o = "`" ++ TLF.formatMetaObjectAsString o ++ "`"

compileMetaType :: Scope -> TLS.MetaType Range -> ErrorMonad TLR.MetaType
compileMetaType scope (TLS.MTFun range params result) =
	compileAbstraction scope params (\_ scope' -> compileMetaType scope' result) TLR.MTFun
compileMetaType scope (TLS.MTSLType range slKind) = do
	slKind' <- SLC.compileKind slKind
	return (TLR.MTSLType slKind')
compileMetaType scope (TLS.MTSLTerm range slType) = do
	slType' <- compileMetaObject scope slType
	case TLR.reduceMetaType (TLR.typeOfMetaObject slType') of
		TLR.MTSLType SLR.KindType -> return ()
		otherType -> fail ("in `(sl-term ...)` type at " ++ formatRange range ++ ": the SL type is given as " ++
			formatMOForMessage slType' ++ " (at " ++ formatRange (TLS.tagOfMetaObject slType) ++ "), which has \
			\meta-type " ++ formatMTForMessage otherType ++ ", but it's supposed to have meta-type `(sl-type \
			\\"*\")`.")
	return (TLR.MTSLTerm slType')
compileMetaType scope (TLS.MTJSExprType range slType) = do
	slType' <- compileMetaObject scope slType
	case TLR.reduceMetaType (TLR.typeOfMetaObject slType') of
		TLR.MTSLType SLR.KindType -> return ()
		otherType -> fail ("in `(js-expr-type ...)` type at " ++ formatRange range ++ ": the SL equivalent is given \
			\as " ++ formatMOForMessage slType' ++ " (at " ++ formatRange (TLS.tagOfMetaObject slType) ++ "), which \
			\has meta-type " ++ formatMTForMessage otherType ++ ", but it's supposed to have meta-type `(sl-type \
			\\"*\")`.")
	return (TLR.MTJSExprType slType')
compileMetaType scope (TLS.MTJSExpr range jsType slTerm) = do
	let msgPrefix = "in `(js-expr ...)` type at " ++ formatRange range ++ ": "
	jsType' <- compileMetaObject scope jsType
	slType1 <- case TLR.reduceMetaType (TLR.typeOfMetaObject jsType') of
		TLR.MTJSExprType equiv -> return equiv
		otherType -> fail (msgPrefix ++ "the JavaScript type is given as " ++ formatMOForMessage jsType' ++ " (at " ++
			formatRange (TLS.tagOfMetaObject jsType) ++ "), which has meta-type " ++ formatMTForMessage otherType ++
			", but it's supposed to have meta-type `(js-expr-type ...)`.")
	slTerm' <- compileMetaObject scope slTerm
	slType2 <- case TLR.reduceMetaType (TLR.typeOfMetaObject slTerm') of
		TLR.MTSLTerm ty -> return ty
		otherType -> fail (msgPrefix ++ "the SL equivalent term is given as " ++ formatMOForMessage slTerm' ++
			" (at " ++ formatRange (TLS.tagOfMetaObject slTerm) ++ "), which has meta-type " ++
			formatMTForMessage otherType ++ ", but it's supposed to have meta-type `(sl-term ...)`.")
	unless (TLR.equivalentMetaObjects (TLR.reduceMetaObject slType1) (TLR.reduceMetaObject slType2)) $
		fail (msgPrefix ++ "the JavaScript type is " ++ formatMOForMessage jsType' ++ " (at " ++
			formatRange (TLS.tagOfMetaObject jsType) ++ "), which has SL equivalent " ++
			formatMOForMessage slType1 ++ ", and the SL term equivalent is " ++ formatMOForMessage slTerm' ++
			" (at " ++ formatRange (TLS.tagOfMetaObject slTerm) ++ "), which has SL type " ++
			formatMOForMessage slType2 ++ ". The SL equivalent of the JavaScript type is supposed to be the \
			\same as the SL type of the SL equivalent term.")
	return (TLR.MTJSExpr jsType' slTerm')

compileMetaObject :: Scope -> TLS.MetaObject Range -> ErrorMonad TLR.MetaObject

compileMetaObject scope (TLS.MOApp range fun arg) = do
	let msgPrefix = "in application of function at " ++ formatRange (TLS.tagOfMetaObject fun) ++ " to argument at " ++
			formatRange (TLS.tagOfMetaObject arg) ++ ": "
	fun' <- compileMetaObject scope fun
	arg' <- compileMetaObject scope arg
	paramType <- case TLR.reduceMetaType (TLR.typeOfMetaObject fun') of
		TLR.MTFun (_, paramType) _ -> return paramType
		otherType -> fail (msgPrefix ++ "the function has type " ++ formatMTForMessage otherType ++ ", so it cannot \
			\be used as a function.")
	unless (TLR.typeOfMetaObject arg' `TLR.equivalentMetaTypes` paramType) $
		fail (msgPrefix ++ "the function expects an argument of type " ++ formatMTForMessage paramType ++ ", but the \
			\argument has type " ++ formatMTForMessage (TLR.typeOfMetaObject arg') ++ ".")
	return (TLR.MOApp fun' arg')

compileMetaObject scope (TLS.MOAbs range params result) =
	compileAbstraction scope params (\_ scope' -> compileMetaObject scope' result) TLR.MOAbs

compileMetaObject scope (TLS.MOName range name) = case M.lookup name (metaObjectsInScope scope) of
	Just x -> return x
	Nothing -> fail ("at " ++ formatRange range ++ ": name `" ++ TLS.unName name ++ "` is not in scope")

compileMetaObject scope (TLS.MOSLTypeLiteral range type_ typeBindings) = do
	let msgPrefix = "in `(sl-type ...)` literal at " ++ formatRange range ++ ": "
	typeBindings' <- compileSLTypeBindings msgPrefix scope typeBindings
	let typeScopeAdditions = M.mapWithKey (\ name (TLR.SLTypeBinding value) ->
		case TLR.typeOfMetaObject value of
			TLR.MTSLType kind -> SLC.NameTypeInScope (SLR.NameOfType (SLS.unNameOfType name)) kind
			_ -> error "binding should have type (sl-type ...)"
		) typeBindings'
	type_' <- errorContext msgPrefix $
		SLC.compileType
			(typeScopeAdditions `M.union` SLC.typesInScope (slObjectsInScope scope))
			type_
	-- TODO: Make sure subs are applied appropriately in `type_'`
	let typeBindings'' = M.mapKeys (SLR.NameOfType . SLS.unNameOfType) typeBindings'
	return (TLR.MOSLType type_' typeBindings'')

compileMetaObject scope (TLS.MOSLTermLiteral range term typeBindings termBindings) = do
	let msgPrefix = "in `(sl-term ...)` literal at " ++ formatRange range ++ ": "

	typeBindings' <- compileSLTypeBindings msgPrefix scope typeBindings
	let typeScopeAdditions = M.mapWithKey (\ name (TLR.SLTypeBinding value) -> case TLR.typeOfMetaObject value of
		TLR.MTSLType kind -> SLC.NameTypeInScope (SLR.NameOfType (SLS.unNameOfType name)) kind
		_ -> error "binding should have type (sl-type ...)"
		) typeBindings'
	let typeBindings'' = M.mapKeys (SLR.NameOfType . SLS.unNameOfType) typeBindings'

	termBindings' <- compileSLTermBindings msgPrefix scope termBindings
	let
		termScopeMaker :: State
				(M.Map SLR.NameOfType TLR.SLTypeBinding)
				(M.Map SLS.NameOfTerm SLC.TermInScope)
		termScopeMaker = liftM M.fromList $ sequence [do
			let
				internType :: SLR.NameOfType
				           -> TLR.MetaObject
				           -> State (M.Map SLR.NameOfType TLR.SLTypeBinding) SLR.Type
				internType suggestedName typeAsMO = case TLR.reduceMetaObject typeAsMO of
					TLR.MOSLType typeAsSL bindings -> do
						subs <- liftM M.fromList $ sequence [do
							value' <- internType name value
							return (name, SLR.simpleTypeSub value')
							| (name, TLR.SLTypeBinding value) <- M.toList bindings]
						return (runIdentity (SLR.substituteType subs typeAsSL))
					other -> do
						existingBindings <- get
						name' <- case find
								(\(_, TLR.SLTypeBinding v) -> TLR.equivalentMetaObjects v other)
								(M.toList existingBindings)
								of
							Nothing -> do
								let forbiddenNames = M.keysSet existingBindings
										`S.union` S.fromList [case tis of
											SLC.NameTypeInScope name _ -> name
											SLC.DefinedTypeInScope defn -> SLR.nameOfDataDefn defn
											| (_, tis) <- M.toList (SLC.typesInScope (slObjectsInScope scope))]
								let candidateNames =
									[SLR.NameOfType (SLR.unNameOfType suggestedName ++ replicate i '\'') | i <- [0..]]
								let Just name' = find (`S.notMember` forbiddenNames) candidateNames
								put (M.insert name' (TLR.SLTypeBinding other) existingBindings)
								return name'
							Just (n, _) -> return n
						let kind = case TLR.typeOfMetaObject other of
								TLR.MTSLType k -> k
								_ -> error "this should have type (sl-type ...)"
						return (SLR.TypeName name' kind)
			paramTypes <- sequence [
				internType (SLR.NameOfType (name ++ "Type")) type_
				| (TLR.NameOfMetaObject name, type_) <- params]
			valueType <- case TLR.typeOfMetaObject value of
				TLR.MTSLTerm type_ -> internType suggestedName type_
					where suggestedName = SLR.NameOfType (SLS.unNameOfTerm name ++ "Type")
				_ -> error "this should have type (sl-term ...)"
			let wholeType = foldr SLR.TypeFun valueType paramTypes
			return (name, SLC.NameTermInScope (SLR.NameOfTerm (SLS.unNameOfTerm name)) wholeType)
			| (name, TLR.SLTermBinding params value) <- M.toList termBindings']
	let (termScopeAdditions, typeBindings''') = runState termScopeMaker typeBindings''
	let termBindings'' = M.mapKeys (SLR.NameOfTerm . SLS.unNameOfTerm) termBindings'

	term' <- errorContext msgPrefix $
		SLC.compileTerm
			(slObjectsInScope scope) {
				SLC.typesInScope = typeScopeAdditions `M.union` (SLC.typesInScope $ slObjectsInScope scope),
				SLC.termsInScope = termScopeAdditions `M.union` (SLC.termsInScope $ slObjectsInScope scope)
				} 
			term
	-- TODO: Make sure subs are applied appropriately in `term'`

	return (TLR.MOSLTerm term' typeBindings''' termBindings'')

compileMetaObject scope (TLS.MOJSExprLiteral range equiv type_ expr bindings) = do
	let msgPrefix = "in `(js-expr ...)` literal at " ++ formatRange range ++ ": "
	equiv' <- compileMetaObject scope equiv
	slType1 <- case TLR.reduceMetaType (TLR.typeOfMetaObject equiv') of
		TLR.MTSLTerm ty -> return ty
		otherType -> fail (msgPrefix ++ "the SL equivalent given at " ++ formatRange (TLS.tagOfMetaObject equiv) ++
			" has meta-type " ++ formatMTForMessage otherType ++ ", but it should have meta-type `(sl-term ...)`.")
	type_' <- compileMetaObject scope type_
	slType2 <- case TLR.reduceMetaType (TLR.typeOfMetaObject type_') of
		TLR.MTJSExprType ty -> return ty
		otherType -> fail (msgPrefix ++ "the JavaScript type given at " ++ formatRange (TLS.tagOfMetaObject type_) ++
			" has meta-type " ++ formatMTForMessage otherType ++ ", but it should have meta-type \
			\`(js-expr-type ...)`.")
	unless (slType1 `TLR.equivalentMetaObjects` slType2) $
		fail (msgPrefix ++ "the SL equivalent given at " ++ formatRange (TLS.tagOfMetaObject equiv) ++ " has SL \
			\type " ++ formatMOForMessage slType1 ++ ", but the JavaScript type given at " ++
			formatRange (TLS.tagOfMetaObject type_) ++ " has SL equivalent type " ++ formatMOForMessage slType2 ++
			". Those are supposed to be the same, but they aren't.")
	bindings' <- compileJSExprBindings msgPrefix scope bindings
	-- TODO: Make sure subs are applied appropriately in `expr`

	return (TLR.MOJSExprLiteral equiv' type_' (JS.removeAnnotations expr) bindings')

compileMetaObject scope (TLS.MOJSExprConvertEquiv range inEquiv outEquiv content) = do
	let msgPrefix = "in `(js-expr-convert-equiv ...)` construct at " ++ formatRange range ++ ": "
	inEquiv' <- compileMetaObject scope inEquiv
	slType1 <- case TLR.reduceMetaType (TLR.typeOfMetaObject inEquiv') of
		TLR.MTSLTerm ty -> return ty
		otherType -> fail (msgPrefix ++ "the `in-equiv` given at " ++ formatRange (TLS.tagOfMetaObject inEquiv) ++
			" has meta-type " ++ formatMTForMessage otherType ++ ", but it should have meta-type `(sl-term ...)`.")
	outEquiv' <- compileMetaObject scope outEquiv
	slType2 <- case TLR.reduceMetaType (TLR.typeOfMetaObject outEquiv') of
		TLR.MTSLTerm ty -> return ty
		otherType -> fail (msgPrefix ++ "the `out-equiv` given at " ++ formatRange (TLS.tagOfMetaObject outEquiv) ++
			" has meta-type " ++ formatMTForMessage otherType ++ ", but it should have meta-type `(sl-term ...)`.")
	unless (slType1 `TLR.equivalentMetaObjects` slType2) $
		fail (msgPrefix ++ "the `in-equiv` given at " ++ formatRange (TLS.tagOfMetaObject inEquiv) ++ " has SL \
			\type " ++ formatMOForMessage slType1 ++ ", but the `out-equiv` given at " ++
			formatRange (TLS.tagOfMetaObject outEquiv) ++ " has SL type " ++ formatMOForMessage slType2 ++
			". Because they have different types, they can't possibly be equivalent expressions.")
	content' <- compileMetaObject scope content
	case TLR.reduceMetaType (TLR.typeOfMetaObject content') of
		TLR.MTJSExpr _ equiv ->
			unless (equiv `TLR.equivalentMetaObjects` inEquiv') $
				fail (msgPrefix ++ "the `content` given at " ++ formatRange (TLS.tagOfMetaObject content) ++
					" has SL equivalent " ++ formatMOForMessage equiv ++ ", but the `in-equiv` given at " ++
					formatRange (TLS.tagOfMetaObject inEquiv) ++ " is " ++ formatMOForMessage inEquiv' ++ ".")
		otherType -> fail (msgPrefix ++ "the `content` given at " ++ formatRange (TLS.tagOfMetaObject content) ++
			" has meta-type " ++ formatMTForMessage otherType ++ ", but it should have meta-type `(js-expr ...)`.")
	return $ TLR.MOJSExprConvertEquiv outEquiv' content'

compileAbstraction :: Scope
                   -> [(TLS.Name, TLS.MetaType Range)]
                   -> ([(TLR.NameOfMetaObject, TLR.MetaType)] -> Scope -> ErrorMonad a)
                   -> ((TLR.NameOfMetaObject, TLR.MetaType) -> a -> a)
                   -> ErrorMonad a
compileAbstraction scope [] base fun = base [] scope
compileAbstraction scope ((paramName, paramType):params) base fun = do
	let paramName' = TLR.NameOfMetaObject (TLS.unName paramName)
	paramType' <- compileMetaType scope paramType
	let scope' = scope { metaObjectsInScope = M.insert paramName (TLR.MOName paramName' paramType') (metaObjectsInScope scope) }
	let base' = base . ((paramName', paramType'):)
	rest <- compileAbstraction scope' params base' fun
	return (fun (paramName', paramType') rest)

compileBindings :: Ord n
                => Scope
                -> (Scope -> TLS.Binding Range n -> TLS.BindingParam Range -> ErrorMonad (Scope, p))
                -> (Scope -> TLS.Binding Range n -> [p] -> TLS.MetaObject Range -> ErrorMonad a)
                -> [TLS.Binding Range n]
                -> ErrorMonad (M.Map n a)
compileBindings scope paramFun valueFun bindings = do
	bindings' <- sequence [do
		let
			f scope' [] params' =
				valueFun scope' binding params' (TLS.valueOfBinding binding)
			f scope' (param:params) paramsSoFar' = do
				(scope'', param') <- paramFun scope' binding param
				f scope'' params (paramsSoFar' ++ [param'])
		value' <- f scope (TLS.paramsOfBinding binding) []
		return (TLS.nameOfBinding binding, value')
		| binding <- bindings]
	-- TODO: Check for duplicates
	return (M.fromList bindings')

compileSLTypeBindings :: String
                      -> Scope
                      -> [TLS.Binding Range SLS.NameOfType]
                      -> ErrorMonad (M.Map SLS.NameOfType TLR.SLTypeBinding)
compileSLTypeBindings msgPrefix1 scope bindings = compileBindings scope
	(\ subScope binding (TLS.BindingParam paramRange parts) -> do
		let msgPrefix2 = msgPrefix1 ++ "in binding of name `" ++ SLS.unNameOfType (TLS.nameOfBinding binding) ++
			"` (at " ++ formatRange (TLS.tagOfBinding binding) ++ "): "
		fail (msgPrefix2 ++ "type bindings are not allowed to have parameters")
		)
	(\ subScope binding [] value -> do
		value' <- compileMetaObject subScope value
		case TLR.typeOfMetaObject value' of
			TLR.MTSLType _ -> return (TLR.SLTypeBinding value')
			otherType -> fail (msgPrefix1 ++ "in binding of name `" ++ SLS.unNameOfType (TLS.nameOfBinding binding) ++
				"` (at " ++ formatRange (TLS.tagOfBinding binding) ++ ": value at " ++
				formatRange (TLS.tagOfMetaObject value) ++ " has meta-type " ++ formatMTForMessage otherType ++ ", but \
				\it's supposed to have meta-type `(sl-type ...)`.")
		)
	bindings

compileSLTermBindings :: String
                      -> Scope
                      -> [TLS.Binding Range SLS.NameOfTerm]
                      -> ErrorMonad (M.Map SLS.NameOfTerm TLR.SLTermBinding)
compileSLTermBindings msgPrefix1 scope bindings = compileBindings scope
	(\ subScope binding (TLS.BindingParam paramRange parts) -> do
		let msgPrefix2 = msgPrefix1 ++ "in binding of name `" ++ SLS.unNameOfTerm (TLS.nameOfBinding binding) ++
			"` (at " ++ formatRange (TLS.tagOfBinding binding) ++ "): "
		(name, type_) <- case parts of
			[(name, type_)] -> return (name, type_)
			_ -> fail (msgPrefix2 ++ "parameter at " ++ formatRange paramRange ++ " has two or more parts, which is \
				\illegal.")
		let name' = TLR.NameOfMetaObject (TLS.unName name)
		type_' <- compileMetaType scope type_
		slType' <- case TLR.reduceMetaType type_' of
			TLR.MTSLTerm slType' -> return slType'
			otherType -> fail (msgPrefix2 ++ "parameter `" ++ TLS.unName name ++ "` (at " ++ formatRange paramRange ++
				") has meta-type " ++ formatMTForMessage type_' ++ ", but all parameters in a `(sl-term ...)` literal \
				\are supposed to have meta-type `(sl-term ...)`.")
		let subScope' = subScope {
			metaObjectsInScope = M.insert name (TLR.MOName name' type_') (metaObjectsInScope subScope)
			}
		return (subScope', (name', slType'))
		)
	(\ subScope' binding params' value -> do
		let msgPrefix2 = msgPrefix1 ++ "in binding of name `" ++ SLS.unNameOfTerm (TLS.nameOfBinding binding) ++
			"` (at " ++ formatRange (TLS.tagOfBinding binding) ++ "): "
		value' <- compileMetaObject subScope' value
		case TLR.typeOfMetaObject value' of
			TLR.MTSLTerm _ -> return (TLR.SLTermBinding params' value')
			otherType -> fail ("value at " ++ formatRange (TLS.tagOfMetaObject value) ++ " has meta-type " ++
				formatMTForMessage otherType ++ ", but it's supposed to have meta-type `(sl-term ...)`.")
		)
	bindings

compileJSExprBindings :: String
                      -> Scope
                      -> [TLS.Binding Range (JS.Id ())]
                      -> ErrorMonad (M.Map (JS.Id ()) TLR.JSExprBinding)
compileJSExprBindings msgPrefix1 scope bindings = compileBindings scope
	(\ subScope binding (TLS.BindingParam paramRange parts) -> do
		let msgPrefix2 = msgPrefix1 ++ "in binding of name `" ++ JS.unId (TLS.nameOfBinding binding) ++ "` (at " ++
			formatRange (TLS.tagOfBinding binding) ++ "): "
		
		(name1, type1, name2, type2) <- case parts of
			[(name1, type1), (name2, type2)] -> return (name1, type1, name2, type2)
			_ -> fail (msgPrefix2 ++ "JavaScript expression binding parameters are supposed to have two parts, like \
				\`(x :: sl-term ... | y :: js-expr ... x)`.")

		let msgPrefix3 = msgPrefix2 ++ "in parameter pair `" ++ TLS.unName name1 ++ "` and `" ++ TLS.unName name2 ++
			"` (at " ++ formatRange paramRange ++ "): "

		let name1' = TLR.NameOfMetaObject (TLS.unName name1)
		type1' <- compileMetaType scope type1
		slType <- case TLR.reduceMetaType type1' of
			TLR.MTSLTerm ty -> return ty
			otherType -> fail (msgPrefix3 ++ "the meta-type of the first parameter `" ++ TLS.unName name1 ++ "` is \
				\given as " ++ formatMTForMessage otherType ++ " (at " ++ formatRange (TLS.tagOfMetaType type1) ++
				"), but it is supposed to be something of the form `(sl-term ...)`.")

		let scopeForType2 = scope { metaObjectsInScope =
			M.insert name1 (TLR.MOName name1' type1') $
			metaObjectsInScope scope
			}
		let name2' = TLR.NameOfMetaObject (TLS.unName name2)
		type2' <- compileMetaType scopeForType2 type2
		jsType <- case TLR.reduceMetaType type2' of
			TLR.MTJSExpr jsType (TLR.MOName n _) | n == name1' -> return jsType
			otherType -> fail (msgPrefix3 ++ "the meta-type of the second parameter `" ++ TLS.unName name2 ++ "` is \
				\given as " ++ formatMTForMessage otherType ++ " (at " ++ formatRange (TLS.tagOfMetaType type2) ++
				"), but it is supposed to be something of the form `(js-expr ... " ++ TLS.unName name1 ++ ")`.")

		let subScope' = subScope { metaObjectsInScope =
			M.insert name2 (TLR.MOName name2' type2') $
			M.insert name1 (TLR.MOName name1' type1') $
			metaObjectsInScope subScope
			}

		return (subScope', TLR.JSExprBindingParam name1' slType name2' jsType)
		)

	(\ subScope' binding params' value -> do
		let msgPrefix2 = msgPrefix1 ++ "in binding of name `" ++ JS.unId (TLS.nameOfBinding binding) ++ "` (at " ++
			formatRange (TLS.tagOfBinding binding) ++ "): "
		value' <- compileMetaObject subScope' value
		case TLR.typeOfMetaObject value' of
			TLR.MTJSExpr _ _ -> return ()
			otherType -> fail (msgPrefix2 ++ "value at " ++ formatRange (TLS.tagOfMetaObject value) ++ " has \
				\meta-type " ++ formatMTForMessage otherType ++ ", but it's supposed to have meta-type \
				\`(js-expr ...)`.")
		return (TLR.JSExprBinding params' value')
		)

	bindings

data GlobalResults = GlobalResults {
	slDefnsOfGlobalResults :: SLC.Defns,
	tlDefnsOfGlobalResults :: M.Map TLS.Name TLR.MetaObject,
	emitsOfGlobalResults :: [JS.Statement ()]
	}

compileDirectives :: [TLS.Directive Range] -> ErrorMonad GlobalResults
compileDirectives directives = do

	let allSLDirs = concat [content | TLS.DSLCode _ content <- directives]
	slDefns <- SLC.compileSLDirectives allSLDirs

	-- `let`, `js-expr-type`, and `js-expr-global` directives are collectively referred to as "definition directives"
	-- because they introduce new names into scope. They also both may refer to names already in scope; this means that
	-- they have to be top-sorted before processing them.
	let unsortedDefnDirs = concat [case dir of
			TLS.DLet _ name params type_ value -> [((name, dir), name, S.toList freeNames)]
				where freeNames = TLS.freeNamesInAbstraction params $
						maybe S.empty TLS.freeNamesInMetaType type_
						`S.union` TLS.freeNamesInMetaObject value
			TLS.DJSExprType _ name params spec -> [((name, dir), name, S.toList freeNames)]
				where freeNames = TLS.freeNamesInAbstraction params $
						TLS.freeNamesInMetaObject spec
			TLS.DJSExprGlobal _ name params _ type_ spec _ -> [((name, dir), name, S.toList freeNames)]
				where freeNames = TLS.freeNamesInAbstraction params $
						TLS.freeNamesInMetaObject type_
						`S.union` TLS.freeNamesInMetaObject spec
			_ -> []
			| dir <- directives]
	sortedDefnDirs <- sequence [case scc of
		Data.Graph.AcyclicSCC (name, dir) -> return (name, dir)
		Data.Graph.CyclicSCC things -> fail ("the following top-level directive(s) form an illegal recursive loop: " ++ 
			Data.List.intercalate ", "
				["`" ++ TLS.unName name ++ "` (at " ++ formatRange (TLS.tagOfDirective dir) ++ ")"
				| (name, dir) <- things] ++ ".")
		| scc <- Data.Graph.stronglyConnComp unsortedDefnDirs]

	let
		compileDefnDir :: Scope
		               -> TLS.Directive Range
		               -> ErrorMonad (TLR.MetaObject, [Scope -> ErrorMonad [JS.Statement ()]])

		compileDefnDir scope (TLS.DLet range _ params maybeType value) = do
			value <- compileAbstraction scope params (\_ scope' -> do
				value' <- compileMetaObject scope' value
				case maybeType of
					Nothing -> return ()
					Just type_ -> do
						type_' <- compileMetaType scope' type_
						unless (TLR.typeOfMetaObject value' `TLR.equivalentMetaTypes` type_') $
							fail ("in `(let ...)` directive at " ++ formatRange range ++ ": the type signature \
								\given at " ++ formatRange (TLS.tagOfMetaType type_) ++ " says the type should \
								\be " ++ formatMTForMessage type_' ++ ", but the actual value at " ++
								formatRange (TLS.tagOfMetaObject value) ++ " has type " ++
								formatMTForMessage (TLR.typeOfMetaObject value') ++ ".")
				return value'
				) TLR.MOAbs
			return (value, [])

		compileDefnDir scope (TLS.DJSExprType range name params slEquiv) = do
			value <- compileAbstraction scope params (\params' scope' -> do
				slEquiv' <- compileMetaObject scope' slEquiv
				case TLR.typeOfMetaObject slEquiv' of
					TLR.MTSLType SLR.KindType -> return ()
					otherType -> fail ("in `(js-expr-type ...)` directive at " ++ formatRange range ++ ": the SL \
						\equivalent given at " ++ formatRange (TLS.tagOfMetaObject slEquiv) ++ " has meta-type " ++
						formatMTForMessage otherType ++ ", but it's supposed to have meta-type `(sl-type ...)`.")
				let defn = TLR.JSExprTypeDefn {
					TLR.nameOfJSExprTypeDefn = TLR.NameOfMetaObject (TLS.unName name),
					TLR.paramsOfJSExprTypeDefn = map snd params',
					TLR.slEquivOfJSExprTypeDefn = \paramValues -> let
						subs = M.fromList $ zip (map fst params') paramValues
						in TLR.substituteMetaObject subs slEquiv'
					}
				return (TLR.MOJSExprTypeDefn defn [TLR.MOName paramName paramType | (paramName, paramType) <- params'])
				) TLR.MOAbs
			return (value, [])

		compileDefnDir scope dir@(TLS.DJSExprGlobal range globalName params runtimeArgs type_ equiv body) = do
			let msgPrefix = "in `(js-expr-global ...)` directive named `" ++ TLS.unName globalName ++ "` at " ++
					formatRange range ++ ": "
			compileAbstraction scope params (\params' scope' -> do
				equiv' <- compileMetaObject scope' equiv
				slType1 <- case TLR.reduceMetaType (TLR.typeOfMetaObject equiv') of
					TLR.MTSLTerm ty -> return ty
					otherType -> fail (msgPrefix ++ "the SL equivalent given at " ++ formatRange (TLS.tagOfMetaObject equiv) ++
						" has meta-type " ++ formatMTForMessage otherType ++ ", but it should have meta-type `(sl-term ...)`.")
				type_' <- compileMetaObject scope' type_
				slType2 <- case TLR.reduceMetaType (TLR.typeOfMetaObject type_') of
					TLR.MTJSExprType equiv -> return equiv
					otherType -> fail (msgPrefix ++ "the JavaScript type given at " ++ formatRange (TLS.tagOfMetaObject type_) ++
						" has meta-type " ++ formatMTForMessage otherType ++ ", but it should have meta-type \
						\`(js-expr-type ...)`.")
				unless (slType1 `TLR.equivalentMetaObjects` slType2) $
					fail (msgPrefix ++ "the SL equivalent given at " ++ formatRange (TLS.tagOfMetaObject equiv) ++ " has SL \
						\type " ++ formatMOForMessage slType1 ++ ", but the JavaScript type given at " ++
						formatRange (TLS.tagOfMetaObject type_) ++ " has SL equivalent type " ++
						formatMOForMessage slType2 ++ ". Those are supposed to be the same, but they aren't.")
				let paramTypesMap = M.fromList [(name, type_) | ((name, _), (_, type_)) <- zip params params']
				let paramNamesMap = M.fromList [(name, name') | ((name, _), (name', _)) <- zip params params']
				runtimeArgsTypesAndEquivs <- sequence [
					case M.lookup argName paramTypesMap of
						Nothing -> fail ("the name `" ++ TLS.unName argName ++ "` appears in the `(args ...)` clause, \
							\but it is not the name of a parameter.")
						Just (TLR.MTJSExpr t e) -> return (t, e)
						Just otherType -> fail  ("the name `" ++ TLS.unName argName ++ "` appears in the `(args ...)` \
							\clause, but that parameter has type " ++ formatMTForMessage otherType ++ ", and only \
							\JavaScript expressions can be passed to JavaScript functions.")
					| argName <- runtimeArgs]
				let runtimeArgsJSNames = [JS.Id () ("arg_" ++ show i ++ "_" ++ TLS.unName name)
						| (name, i) <- zip runtimeArgs [1..]]
				unless (runtimeArgs == nub runtimeArgs) $
					fail (msgPrefix ++ "the following name(s) appear more than once in the `(args ...)` clause: " ++
						intercalate ", " (map TLS.unName (nub ((Data.List.\\) runtimeArgs (nub runtimeArgs)))))
				let globalJSName = JS.Id () ("global_" ++ TLS.unName globalName)
				let jsEquiv = JS.CallExpr ()
						(JS.VarRef () globalJSName)
						[JS.CallExpr () (JS.VarRef () jsName) [] | jsName <- runtimeArgsJSNames]
				let value = TLR.MOJSExprLiteral
					equiv' type_'
					jsEquiv
					(M.fromList [
						(jsName, TLR.JSExprBinding [] (TLR.MOName ((M.!) paramNamesMap name) ((M.!) paramTypesMap name)))
						| (name, jsName) <- zip runtimeArgs runtimeArgsJSNames
						])
				let globalFunctions = [\ scope'' -> do
					let scope''' = scope'' {
						metaObjectsInScope = M.fromList [
								(name, TLR.MOName ((M.!) paramNamesMap name) ((M.!) paramTypesMap name))
								| (name, _) <- params]
								`M.union` (metaObjectsInScope scope'')
						}
					body' <- compileMetaObject scope''' body
					unless (TLR.typeOfMetaObject body' `TLR.equivalentMetaTypes` TLR.MTJSExpr type_' equiv') $
						fail (msgPrefix ++ "the body has type " ++ formatMTForMessage (TLR.typeOfMetaObject body') ++
							" but the `(spec ...)` and `(type ...)` clauses specify that the type of the body should \
							\be " ++ formatMTForMessage (TLR.MTJSExpr type_' equiv') ++ ".")
					let runtimeArgsJSNames = [JS.Id () ("arg_" ++ show i ++ "_" ++ TLS.unName name)
							| (name, i) <- zip runtimeArgs [1..]]
					let runtimeArgsMetaObjects = [TLR.MOJSExprLiteral e t (JS.VarRef () n) M.empty
							| ((e, t), n) <- zip runtimeArgsTypesAndEquivs runtimeArgsJSNames]
					let body'' = TLR.substituteMetaObject
							(M.fromList (zip
									[(M.!) paramNamesMap name | name <- runtimeArgs]
									runtimeArgsMetaObjects
									))
							body'
					body''' <- case TLR.reduceMetaObject body'' of
						TLR.MOJSExprLiteral _ _ expr subs | M.null subs -> return expr
						_ -> fail (msgPrefix ++ "the body depends in an illegal way on a parameter that is not in \
							\`(args ...)` clause.")
					return [JS.FunctionStmt () globalJSName runtimeArgsJSNames [JS.ReturnStmt () (Just body''')]]
					]
				return (value, globalFunctions)
				)
				(\ (paramName, paramType) (value, globalFunctions) ->
					(TLR.MOAbs (paramName, paramType) value, globalFunctions)
					)

	(tlDefns, globalFunctions) <- foldM (\ (tlDefns, globalFunctions) (name, defnDir) -> do
		let scope = Scope
			(tlDefns `M.union`
				M.fromList [(name, error "loop, not caught by top-sort")
				| (name, _) <- sortedDefnDirs, name `M.notMember` tlDefns])
			(SLC.scopeForDefns slDefns)
		(value, newGlobalFunctions) <- compileDefnDir scope defnDir
		let tlDefns' = M.insert name value tlDefns
		let globalFunctions' = globalFunctions ++ newGlobalFunctions
		return (tlDefns', globalFunctions')
		)
		(M.empty, [])
		sortedDefnDirs

	let scope = Scope tlDefns (SLC.scopeForDefns slDefns)

	emitsFromJSExprGlobalDirs <- liftM concat $ mapM ($ scope) globalFunctions

	emitsFromJSEmitDirs <- liftM concat $ sequence [do
		bindings' <- compileJSExprBindings
			("in `(js-emit ...)` directive at " ++ formatRange range ++ ": ")
			scope bindings
		let subs = M.map (\ (TLR.JSExprBinding params value) -> let
				fun paramValues = let
						innerSubs = M.fromList
							[(n2, TLR.MOJSExprLiteral (TLR.MOName n1 (TLR.MTSLTerm t1)) t2 v M.empty)
							| (TLR.JSExprBindingParam n1 t1 n2 t2, v) <- zip params paramValues]
						value' = TLR.reduceMetaObject (TLR.substituteMetaObject innerSubs value)
						in case value' of
							TLR.MOJSExprLiteral _ _ expr bs | M.null bs -> expr
							_ -> error "not completely reduced for some reason"
				varsIntroduced = JS.freeNamesInExpression (fun [JS.NullLit () | _ <- params])
				in JS.SubstFun fun varsIntroduced) bindings'
		return $ map (JS.substituteStatement subs M.empty . JS.removeAnnotations) code
		| TLS.DJSEmit range code bindings <- directives]

	-- Emits from `js-expr-global` directives must go before emits from `js-emit` directives so that user-supplied
	-- statements can access automatically-generated global definitions
	let allEmits = emitsFromJSExprGlobalDirs ++ emitsFromJSEmitDirs 

	return $ GlobalResults {
		slDefnsOfGlobalResults = slDefns,
		tlDefnsOfGlobalResults = tlDefns,
		emitsOfGlobalResults = allEmits
		}

