module Metacompiler.Compile.FormatTL where

import Control.Monad
import Control.Monad.State
import qualified Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Language.ECMAScript3.Syntax.Annotations as JS
import qualified Metacompiler.Compile.FormatSL as FSL
import qualified Metacompiler.Runtime as R
import qualified Metacompiler.SL.Syntax as SL
import qualified Metacompiler.TL.Syntax as TL
import qualified Metacompiler.TL.ToSExpr as TL

formatMetaTypeAsTL :: R.MetaType -> TL.MetaType ()
formatMetaTypeAsTL type_ =
	topLevelRunTLFMonad
		(R.freeAndGlobalNamesInMetaType type_)
		(formatMetaTypeAsTL' type_)

formatMetaTypeAsString :: R.MetaType -> String
formatMetaTypeAsString = TL.formatTLMetaTypeAsString . formatMetaTypeAsTL

formatMetaObjectAsTL :: R.MetaObject -> TL.MetaObject ()
formatMetaObjectAsTL obj =
	topLevelRunTLFMonad
		(R.freeAndGlobalNamesInMetaObject obj)
		(formatMetaObjectAsTL' obj)

formatMetaObjectAsString :: R.MetaObject -> String
formatMetaObjectAsString = TL.formatTLMetaObjectAsString . formatMetaObjectAsTL

-- `formatMetaTypeAsTL'` and `formatMetaObjectAsTL'` are the heart of the procedure. `TLFMonad` is used for keeping
-- track of embedded bits of SL; it will be defined later.

formatMetaTypeAsTL' :: R.MetaType -> TLFMonad (TL.MetaType ())
formatMetaTypeAsTL' (R.MTFun (pn, pt) rt) = do
	let pn' = TL.Name (R.unNameOfMetaObject pn)
	pt' <- formatMetaTypeAsTL' pt
	rt' <- pushToTLStackInTLFMonad [pn'] $ formatMetaTypeAsTL' rt
	case rt' of
		TL.MTFun () ps' rt2' -> return (TL.MTFun () ((pn', pt'):ps') rt2')
		_ -> return (TL.MTFun () [(pn', pt')] rt')
formatMetaTypeAsTL' (R.MTSLType k) = do
	let k' = FSL.formatSLKindAsSL k
	return (TL.MTSLType () k')
formatMetaTypeAsTL' (R.MTSLTerm t) = do
	t' <- formatMetaObjectAsTL' t
	return (TL.MTSLTerm () t')
formatMetaTypeAsTL' (R.MTJSExprType t) = do
	t' <- formatMetaObjectAsTL' t
	return (TL.MTJSExprType () t')
formatMetaTypeAsTL' (R.MTJSExpr ty te) = do
	ty' <- formatMetaObjectAsTL' ty
	te' <- formatMetaObjectAsTL' te
	return (TL.MTJSExpr () ty' te')

formatMetaObjectAsTL' :: R.MetaObject -> TLFMonad (TL.MetaObject ())
formatMetaObjectAsTL' (R.MOApp f x) = do
	f' <- formatMetaObjectAsTL' f
	x' <- formatMetaObjectAsTL' x
	return (TL.MOApp () f' x')
formatMetaObjectAsTL' (R.MOAbs (pn, pt) b) = do
	let pn' = TL.Name (R.unNameOfMetaObject pn)
	pt' <- formatMetaTypeAsTL' pt
	b' <- pushToTLStackInTLFMonad [pn'] $ formatMetaObjectAsTL' b
	case b' of
		TL.MOAbs () ps' b2' -> return (TL.MOAbs () ((pn', pt'):ps') b2')
		_ -> return (TL.MOAbs () [(pn', pt')] b')
formatMetaObjectAsTL' (R.MOName n _) =
	return $ TL.MOName () (TL.Name (R.unNameOfMetaObject n))
formatMetaObjectAsTL' mo@(R.MOSLTypeDefn _) =
	makeSLTypeLiteral mo
formatMetaObjectAsTL' mo@(R.MOSLTypeName _ _) =
	makeSLTypeLiteral mo
formatMetaObjectAsTL' mo@(R.MOSLTypeApp _ _) =
	makeSLTypeLiteral mo
formatMetaObjectAsTL' mo@(R.MOSLTypeFun _ _) =
	makeSLTypeLiteral mo
formatMetaObjectAsTL' mo@(R.MOSLTypeLazy _) =
	makeSLTypeLiteral mo
formatMetaObjectAsTL' mo@(R.MOSLTermDefn _ _) =
	makeSLTermLiteral mo
formatMetaObjectAsTL' mo@(R.MOSLTermName _ _) =
	makeSLTermLiteral mo
formatMetaObjectAsTL' mo@(R.MOSLTermApp _ _) =
	makeSLTermLiteral mo
formatMetaObjectAsTL' mo@(R.MOSLTermAbs _ _) =
	makeSLTermLiteral mo
formatMetaObjectAsTL' mo@(R.MOSLTermCase _ _) =
	makeSLTermLiteral mo
formatMetaObjectAsTL' mo@(R.MOSLTermData _ _ _) =
	makeSLTermLiteral mo
formatMetaObjectAsTL' mo@(R.MOSLTermWrap _) =
	makeSLTermLiteral mo
formatMetaObjectAsTL' mo@(R.MOSLTermUnwrap _) =
	makeSLTermLiteral mo
formatMetaObjectAsTL' (R.MOJSExprTypeDefn d ps) = do
	let d' = TL.Name (R.unNameOfMetaObject (R.nameOfJSExprTypeDefn d))
	ps' <- mapM formatMetaObjectAsTL' ps
	return (foldl (TL.MOApp ()) (TL.MOName () d') ps')
formatMetaObjectAsTL' (R.MOJSExprLiteral e t c bs) = do
	e' <- formatMetaObjectAsTL' e
	t' <- formatMetaObjectAsTL' t
	let c' = JS.reannotate (const undefined) c
	bs' <- sequence [do
		ps' <- sequence [do
			let n1' = TL.Name (R.unNameOfMetaObject n1)
			t1' <- formatMetaObjectAsTL' t1
			let n2' = TL.Name (R.unNameOfMetaObject n2)
			t2' <- formatMetaObjectAsTL' t2
			return (TL.BindingParam [(n1', TL.MTSLTerm () t1'), (n2', TL.MTJSExpr () t2' (TL.MOName () n1'))])
			| R.JSExprBindingParam n1 t1 n2 t2 <- ps]
		let toForbid = map (TL.Name . R.unNameOfMetaObject) $
				concat [[n1, n2] | R.JSExprBindingParam n1 _ n2 _ <- ps]
		v' <- pushToTLStackInTLFMonad toForbid $ formatMetaObjectAsTL' v
		return (TL.Binding () n ps' v')
		| (n, R.JSExprBinding ps v) <- M.toList bs]
	return (TL.MOJSExprLiteral () e' t' c' bs')

-- This is the tricky part: decompiling SL type and term literals. The general procedure is as follows:
--   * When we encounter a SL type or term in the `R.MetaObject` we are traversing, we call `makeSLTypeLiteral` or
--     `makeSLTermLiteral`.
--   * `makeSLTypeLiteral` and `makeSLTermLiteral` convert it to a `SL.Type ()` or `SL.Term ()` using the functions in
--     the `FormatSL` module.
--   * When we encounter a SL type or term name that wasn't brought into scope by the SL code immediately containing
--     it, `makeSLTypeLiteral` or `makeSLTermLiteral` will pass it up via `TLFMonad`.
--   * When we encounter a TL object embedded in a SL type or term, we record a binding using `SLFMonad`, where the
--     parameters of the binding are any type or term names that were passed to us via `TLFMonad` that are brought into
--     scope by the SL code that the TL object is embedded in.

makeSLTypeLiteral :: R.MetaObject -> TLFMonad (TL.MetaObject ())
makeSLTypeLiteral (R.MOSLTypeName slName kind) = do
	slTypeStack <- liftM (slTypeStackInCommonState . commonStateInTLFState) get
	let slName' = SL.NameOfType (R.unNameOfSLType slName)
	case slName' `elem` slTypeStack of
		True -> do
			tlName <- generateNameTLFMonad
			let kind' = FSL.formatSLKindAsSL kind
			recordTypeTLFMonad tlName (slName', kind')
			return (TL.MOName () tlName)
		False -> do
			return (TL.MOSLTypeLiteral () (SL.TypeName () slName') [])
makeSLTypeLiteral obj = do
	(code, typeBindings, []) <- runSLFMonadInTLFMonad' $
		FSL.formatMetaObjectAsSLType handleMetaObjectEmbeddedInSLType M.empty obj
	return (TL.MOSLTypeLiteral () code typeBindings)

makeSLTermLiteral :: R.MetaObject -> TLFMonad (TL.MetaObject ())
makeSLTermLiteral (R.MOSLTermName slName type_) = do
	slTermStack <- liftM (slTermStackInCommonState . commonStateInTLFState) get
	let slName' = SL.NameOfTerm (R.unNameOfSLTerm slName)
	case slName' `elem` slTermStack of
		True -> do
			tlName <- generateNameTLFMonad
			type_' <- formatMetaObjectAsTL' type_
			recordTermTLFMonad tlName (slName', type_')
			return (TL.MOName () tlName)
		False -> do
			return (TL.MOSLTermLiteral () (SL.TermName () slName' []) [] [])
makeSLTermLiteral obj = do
	(code, typeBindings, termBindings) <- runSLFMonadInTLFMonad' $
		FSL.formatMetaObjectAsSLTerm handleMetaObjectEmbeddedInSLType handleMetaObjectEmbeddedInSLTerm M.empty M.empty obj
	return (TL.MOSLTermLiteral () code typeBindings termBindings)

handleMetaObjectEmbeddedInSLType :: M.Map R.NameOfSLType R.SLKind
                                 -> R.MetaObject
                                 -> SLFMonad (SL.Type ())
handleMetaObjectEmbeddedInSLType typeScope obj =
	pushToSLStacksInSFLMonad typeScope M.empty $ do
		(obj', recordedTypes, recordedTerms) <- runTLFMonadInSLFMonad' (formatMetaObjectAsTL' obj)
		runTLFMonadInSLFMonad $ sequence [
			recordTypeTLFMonad tlName (slName, kind)
			| (tlName, (slName, kind)) <- M.toList recordedTypes, R.NameOfSLType (SL.unNameOfType slName) `M.notMember` typeScope]
		runTLFMonadInSLFMonad $ sequence [
			recordTermTLFMonad tlName (slName, type_)
			| (tlName, (slName, type_)) <- M.toList recordedTerms]
		let typesToBind = [(tlName, slName, kind)
			| (tlName, (slName, kind)) <- M.toList recordedTypes, R.NameOfSLType (SL.unNameOfType slName) `M.member` typeScope]
		let typeParams = [TL.BindingParam [(tlName, TL.MTSLType () kind)] | (tlName, _, kind) <- typesToBind]
		rootName <- generateTypeNameSLFMonad
		recordTypeBindingSLFMonad (TL.Binding () rootName typeParams obj')
		return $ foldl (SL.TypeApp ())
			(SL.TypeName () rootName)
			[SL.TypeName () slName | (_, slName, _) <- typesToBind]

handleMetaObjectEmbeddedInSLTerm :: M.Map R.NameOfSLType R.SLKind
                                 -> M.Map R.NameOfSLTerm R.MetaObject
                                 -> R.MetaObject
                                 -> SLFMonad (SL.Term ())
handleMetaObjectEmbeddedInSLTerm typeScope termScope thing =
	pushToSLStacksInSFLMonad typeScope termScope $ do
		(thing', recordedTypes, recordedTerms) <- runTLFMonadInSLFMonad' (formatMetaObjectAsTL' thing)
		runTLFMonadInSLFMonad $ sequence [
			recordTypeTLFMonad tlName (slName, kind)
			| (tlName, (slName, kind)) <- M.toList recordedTypes, R.NameOfSLType (SL.unNameOfType slName) `M.notMember` typeScope]
		runTLFMonadInSLFMonad $ sequence [
			recordTermTLFMonad tlName (slName, type_)
			| (tlName, (slName, type_)) <- M.toList recordedTerms, R.NameOfSLTerm (SL.unNameOfTerm slName) `M.notMember` termScope]
		let typesToBind = [(tlName, slName, kind)
			| (tlName, (slName, kind)) <- M.toList recordedTypes, R.NameOfSLType (SL.unNameOfType slName) `M.member` typeScope]
		let termsToBind = [(tlName, slName, type_)
			| (tlName, (slName, type_)) <- M.toList recordedTerms, R.NameOfSLTerm (SL.unNameOfTerm slName) `M.member` termScope]
		let typeParams = [TL.BindingParam [(tlName, TL.MTSLType () kind)] | (tlName, _, kind) <- typesToBind]
		let termParams = [TL.BindingParam [(tlName, TL.MTSLTerm () type_)] | (tlName, _, type_) <- termsToBind]
		rootName <- generateTermNameSLFMonad
		recordTermBindingSLFMonad (TL.Binding () rootName (typeParams++termParams) thing')
		return $ foldl (SL.TermApp ())
			(SL.TermName () rootName [SL.TypeName () slName | (_, slName, _) <- typesToBind])
			[SL.TermName () slName [] | (_, slName, _) <- termsToBind]

data CommonState = CommonState {
	forbiddenNamesInCommonState :: S.Set String,
	tlStackInCommonState :: [TL.Name],
	slTypeStackInCommonState :: [SL.NameOfType],
	slTermStackInCommonState :: [SL.NameOfTerm],
	namesUsedInCommonState :: S.Set String
	}

generateNameCommonState :: CommonState -> (String, CommonState)
generateNameCommonState state = let
	candidates = [[c] ++ n | c <- "abcdefghijklmnopqrstuvwxyz", n <- [""] ++ map show [2..]]
	allForbidden = forbiddenNamesInCommonState state
		`S.union` S.fromList (map TL.unName (tlStackInCommonState state))
		`S.union` S.fromList (map SL.unNameOfType (slTypeStackInCommonState state))
		`S.union` S.fromList (map SL.unNameOfTerm (slTermStackInCommonState state))
		`S.union` namesUsedInCommonState state
	Just name = Data.List.find (`S.notMember` allForbidden) candidates
	state' = state { namesUsedInCommonState = S.insert name (namesUsedInCommonState state) }
	in (name, state')

type TLFMonad = State TLFState
data TLFState = TLFState {
	commonStateInTLFState :: CommonState,
	recordedTypesInTLFState :: M.Map TL.Name (SL.NameOfType, SL.Kind ()),
	recordedTermsInTLFState :: M.Map TL.Name (SL.NameOfTerm, TL.MetaObject ())
	}

pushToTLStackInTLFMonad :: [TL.Name] -> TLFMonad a -> TLFMonad a
pushToTLStackInTLFMonad names inner = do
	state <- get
	let cs = commonStateInTLFState state
	put $ state { commonStateInTLFState = cs { tlStackInCommonState = tlStackInCommonState cs ++ names } }
	value <- inner
	state' <- get
	let cs' = commonStateInTLFState state'
	put $ state' { commonStateInTLFState = cs' { tlStackInCommonState = tlStackInCommonState cs } }
	return value

generateNameTLFMonad :: TLFMonad TL.Name
generateNameTLFMonad = do
	state <- get
	let (nameString, cs) = generateNameCommonState (commonStateInTLFState state)
	put $ state { commonStateInTLFState = cs }
	return (TL.Name nameString)

recordTypeTLFMonad :: TL.Name -> (SL.NameOfType, SL.Kind ()) -> TLFMonad ()
recordTypeTLFMonad key value = do
	state <- get
	put $ state { recordedTypesInTLFState = M.insert key value (recordedTypesInTLFState state) }

recordTermTLFMonad :: TL.Name -> (SL.NameOfTerm, TL.MetaObject ()) -> TLFMonad ()
recordTermTLFMonad key value = do
	state <- get
	put $ state { recordedTermsInTLFState = M.insert key value (recordedTermsInTLFState state) }

topLevelRunTLFMonad :: R.Names -> TLFMonad a -> a
topLevelRunTLFMonad (R.Names names1 names2 names3) inner = let
	forbiddenNames = S.map R.unNameOfMetaObject names1
		`S.union` S.map R.unNameOfSLType names2
		`S.union` S.map R.unNameOfSLTerm names3
	state1 = TLFState {
		commonStateInTLFState = CommonState {
			forbiddenNamesInCommonState = forbiddenNames,
			tlStackInCommonState = [],
			slTypeStackInCommonState = [],
			slTermStackInCommonState = [],
			namesUsedInCommonState = S.empty
			},
		recordedTypesInTLFState = M.empty,
		recordedTermsInTLFState = M.empty
		}
	(value, state2) = runState inner state1
	in if (M.null (recordedTypesInTLFState state2) && M.null (recordedTermsInTLFState state2))
		then value
		else error "unbound unhandled top-level variables"

type SLFMonad = State SLFState
data SLFState = SLFState {
	commonStateInSLFState :: CommonState,
	recordedTypeBindingsInSLFState :: [TL.Binding () SL.NameOfType],
	recordedTermBindingsInSLFState :: [TL.Binding () SL.NameOfTerm]
	}

pushToSLStacksInSFLMonad :: M.Map R.NameOfSLType R.SLKind -> M.Map R.NameOfSLTerm R.MetaObject -> SLFMonad a -> SLFMonad a
pushToSLStacksInSFLMonad typeScope termScope inner = do
	state <- get
	let cs = commonStateInSLFState state
	put $ state { commonStateInSLFState = cs {
		slTypeStackInCommonState = slTypeStackInCommonState cs ++ map (SL.NameOfType . R.unNameOfSLType) (M.keys typeScope),
		slTermStackInCommonState = slTermStackInCommonState cs ++ map (SL.NameOfTerm . R.unNameOfSLTerm) (M.keys termScope)
		} }
	value <- inner
	state' <- get
	let cs' = commonStateInSLFState state'
	put $ state' { commonStateInSLFState = cs' {
		slTypeStackInCommonState = slTypeStackInCommonState cs,
		slTermStackInCommonState = slTermStackInCommonState cs
		} }
	return value

generateTypeNameSLFMonad :: SLFMonad SL.NameOfType
generateTypeNameSLFMonad = do
	state <- get
	let (nameString, cs) = generateNameCommonState (commonStateInSLFState state)
	put $ state { commonStateInSLFState = cs }
	return (SL.NameOfType nameString)

generateTermNameSLFMonad :: SLFMonad SL.NameOfTerm
generateTermNameSLFMonad = do
	state <- get
	let (nameString, cs) = generateNameCommonState (commonStateInSLFState state)
	put $ state { commonStateInSLFState = cs }
	return (SL.NameOfTerm nameString)

recordTypeBindingSLFMonad :: TL.Binding () SL.NameOfType -> SLFMonad ()
recordTypeBindingSLFMonad binding = do
	state <- get
	put $ state { recordedTypeBindingsInSLFState = recordedTypeBindingsInSLFState state ++ [binding] }

recordTermBindingSLFMonad :: TL.Binding () SL.NameOfTerm -> SLFMonad ()
recordTermBindingSLFMonad binding = do
	state <- get
	put $ state { recordedTermBindingsInSLFState = recordedTermBindingsInSLFState state ++ [binding] }

runSLFMonadInTLFMonad' :: SLFMonad a -> TLFMonad (a, [TL.Binding () SL.NameOfType], [TL.Binding () SL.NameOfTerm])
runSLFMonadInTLFMonad' inner = do
	state <- get
	let innerState = SLFState {
		commonStateInSLFState = commonStateInTLFState state,
		recordedTypeBindingsInSLFState = [],
		recordedTermBindingsInSLFState = []
		}
	let (value, innerState') = runState inner innerState
	let state' = state {
		commonStateInTLFState = commonStateInSLFState innerState'
		}
	put state'
	return (value, recordedTypeBindingsInSLFState innerState', recordedTermBindingsInSLFState innerState')

runTLFMonadInSLFMonad :: TLFMonad a -> SLFMonad a
runTLFMonadInSLFMonad inner = do
	(value, _, _) <- runTLFMonadInSLFMonad' inner
	return value

runTLFMonadInSLFMonad' :: TLFMonad a
                       -> SLFMonad (a, M.Map TL.Name (SL.NameOfType, SL.Kind ()), M.Map TL.Name (SL.NameOfTerm, TL.MetaObject ()))
runTLFMonadInSLFMonad' inner = do
	state <- get
	let innerState = TLFState {
		commonStateInTLFState = commonStateInSLFState state,
		recordedTypesInTLFState = M.empty,
		recordedTermsInTLFState = M.empty
		}
	let (value, innerState') = runState inner innerState
	let state' = state {
		commonStateInSLFState = commonStateInTLFState innerState'
		}
	put state'
	return (value, recordedTypesInTLFState innerState', recordedTermsInTLFState innerState')

