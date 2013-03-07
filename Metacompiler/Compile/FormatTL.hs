module Metacompiler.Compile.FormatTL where

import qualified Metacompiler.Runtime as R
import qualified Metacompiler.SL.Syntax as SL
import qualified Metacompiler.TL.Syntax as TL

-- `formatMetaTypeAsTL'` and `formatMetaObjectAsTL'` are the heart of the procedure. `TLFMonad` is used for keeping
-- track of embedded bits of SL; it will be defined later.

formatMetaTypeAsTL' :: R.MetaType -> TLFMonad (TL.MetaType ())
formatMetaTypeAsTL' (R.MTFun (pn, pt) rt) = do
	let pn' = TL.Name (R.unNameOfMetaObject pn)
	pt' <- formatMetaTypeAsTL' pt
	rt' <- bindInTLFMonad [pn'] $ formatMetaTypeAsTL' rt
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
	b' <- bindInTLFMonad [pn'] $ formatMetaObjectAsTL' b
	case b' of
		TL.MOAbs () ps' b2' -> return (TL.MOAbs () ((pn', pt'):ps') b2')
		_ -> return (TL.MOAbs () [(pn', pt')] b')
formatMetaObjectAsTL' (R.MOName n _) =
	return TL.MOName () (TL.Name (R.unNameOfMetaObject n))
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
formatMetaObjectAsTL' mo@(R.MOSLTermData _ _) =
	makeSLTermLiteral mo
formatMetaObjectAsTL' mo@(R.MOSLTermWrap _ _) =
	makeSLTermLiteral mo
formatMetaObjectAsTL' mo@(R.MOSLTermUnwrap _ _) =
	makeSLTermLiteral mo
formatMetaObjectAsTL' (R.MOJSExprTypeDefn d ps) = do
	ps' <- mapM formatMetaObjectAsTL' ps
	return (foldl (TL.MOApp ()) (TL.MOName () (R.nameOfJSExprTypeDefn d)) ps')
formatMetaObjectAsTL' (R.MOJSExprLiteral e t c bs) = do
	e' <- formatMetaObjectAsTL' e
	t' <- formatMetaObjectAsTL' t
	bs' <- sequence [do
		let ps' = [TL.BindingParam () [
			(TL.Name (R.unNameOfMetaObject n1), TL.MTSLTerm () t1),
			(TL.Name (R.unNameOfMetaObject n2), TL.MTJSExpr () t2 (TL.MOName () (TL.Name (R.unNameOfMetaObject n1))))
			]
			| R.JSExprBindingParam n1 t1 n2 t2 <- ps]
		let toForbid = map (TL.Name . R.unNameOfMetaObject) $
				concat [[n1, n2] | R.JSExprBindingParam n1 _ n2 _ <- ps]
		v' <- bindInTLFMonad toForbid $ formatMetaObjectAsTL' v
		return (TL.Binding () n ps' v')
		| (n, R.JSExprBinding ps v) <- M.toList bs]
	return (TL.MOJSExprLiteral () e' t' c bs')

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

data CommonState = CommonState {
	forbiddenNamesInCommonState :: S.Set TL.Name,
	stackNamesInCommonState :: [TL.Name],
	namesUsedInCommonState :: S.Set String
	}

generateNameCommonState :: S.Set String -> CommonState -> (String, CommonState)
generateNameCommonState forbidden state = let
	candidates = [[c] ++ n | c <- "abcdefghijklmnopqrstuvwxyz", n <- [""] ++ map show [2..]]
	allForbidden = forbidden
		`S.union` S.map TL.unName (forbiddenNamesInCommonState state)
		`S.union` S.fromList (map TL.unName (stackNamesInCommonState state))
		`S.union` namesUsedInCommonState state
	Just name = Data.List.find (`S.notMember` allForbidden) candidates
	state' = state { namesUsedInCommonState = S.insert name (namesUsedInCommonState state) }
	in (name, state')

type TLFMonad = State TLFState
data TLFState = TLFState {
	commonStateInTLFState :: CommonState,
	recordedTypesInTLFState :: M.Map TL.Name (SL.NameOfType, SL.Kind ()),
	recordedTermsInTLFState :: M.Map TL.Name (SL.NameOfType, SL.Type ())
	}

bindInTLFMonad :: [TL.Name] -> TLFMonad a -> TLFMonad a
bindInTLFMonad names inner = do
	state <- get
	let cs = commonStateInTLFState state
	put $ state { commonStateInTLFState = cs { stackNamesInCommonState = stackNamesInCommonState cs ++ names } }
	value <- inner
	state' <- get
	let cs' = commonStateInTLFState state'
	put $ state' { commonStateInTLFState = cs' { stackNamesInCommonState = stackNamesInCommonState cs } }
	return value

generateNameTLFMonad :: TLFMonad TL.Name
generateNameTLFMonad = do
	state <- get
	let (nameString, cs) = generateNameCommonState S.empty (commonStateInTLFState state)
	put $ state { commonStateInTLFState = cs }
	return (TL.Name nameString)

recordTypeTLFMonad :: TL.Name -> (SL.NameOfType, SL.Kind ()) -> TLFMonad ()
recordTypeTLFMonad key value = do
	state <- get
	put $ state { recordedTypesInTLFState = M.insert key value (recordedTypesInTLFState state) }

recordTermTLFMonad :: TL.Name -> (SL.NameOfTerm, SL.Type ()) -> TLFMonad ()
recordTermTLFMonad key value = do
	state <- get
	put $ state { recordedTermsInTLFState = M.insert key value (recordedTermsInTLFState state) }

data SLFMonad = State SLFState
data SLFState = SLFState {
	commonStateOfSLFState :: CommonState,
	recordedTypeBindingsInSLFState :: [TL.Binding () SL.NameOfType],
	recordedTermBindingsInSLFState :: [TL.Binding () SL.NameOfTerm]
	}

generateTypeNameSLFMonad :: M.Map R.NameOfSLType R.SLKind -> SLFMonad SL.NameOfType
generateTypeNameSLFMonad typeScope = do
	let forbidden = S.fromList (map R.unNameOfSLType (M.keys typeScope))
	state <- get
	let (nameString, cs) = generateNameCommonState forbidden (commonStateInSLFState state)
	put $ state { commonStateInSLFState = cs }
	return (SL.NameOfType nameString)

generateTermNameSLFMonad :: M.Map R.NameOfSLType R.SLKind -> M.Map R.NameOfSLTerm R.MetaObject -> SLFMonad SL.NameOfType
generateTermNameSLFMonad typeScope termScope = do
	let forbidden = S.fromList (map R.unNameOfSLType (M.keys typeScope))
			`S.union` S.fromList (map R.unNameOfSLTerm (M.keys termScope))
	state <- get
	let (nameString, cs) = generateNameCommonState forbidden (commonStateInSLFState state)
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

runTLFMonadInSLFMonad' :: TLFMonad a -> SLFMonad (a, M.Map TL.Name (SL.NameOfType, SL.Kind ()), M.Map TL.Name (SL.NameOfTerm, SL.Type ()))
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
	return (value, reecordedTypesInTLFState innerState', recordedTermsInTLFState innerState')

runTLFMonadInSLFMonad :: TLFMonad a -> SLFMonad a
runTLFMonadInSLFMonad inner = do
	(value, _, _) <- runTLFMonadInSLFMonad inner
	return value

makeSLTypeLiteral :: R.MetaObject -> TLFMonad (TL.MetaObject ())
makeSLTypeLiteral (R.MOSLTypeName slName kind) = do
	tlName <- generateNameTLFMonad
	let kind' = formatSLKindAsSL kind
	recordTypeTLFMonad tlName (slName, kind')
	return (TL.MOName tlName)
makeSLTypeLiteral obj = do
	(code, typeBindings, []) <- runSLFMonadInTLFMonad' $
		FSL.formatMetaObjectAsSLType handleMetaObjectEmbeddedInSLType M.empty obj
	return (TL.MOSLTypeLiteral () code typeBindings)

makeSLTermLiteral :: R.MetaObject -> TLFMonad (TL.MetaObject ())
makeSLTermLiteral (R.MOSLTermName slName type_) = do
	tlName <- generateNameTLFMonad
	type_' <- formatMetaObjectAsTL' type_
	recordTermTLFMonad tlName (slName, type_')
makeSLTermLiteral obj = do
	(code, typeBindings, termBindings) <- runSLFMonadInTLFMonad' $
		FSL.formatMetaObjectAsSLType handleMetaObjectEmbeddedInSLType handleMetaObjectEmbeddedInSLTerm M.empty M.empty obj
	return (TL.MOSLTypeLiteral () code typeBindings termBindings)

handleMetaObjectEmbeddedInSLType :: M.Map R.NameOfSLType R.SLKind
                                 -> R.MetaObject
                                 -> SLFMonad (SL.Type ())
handleMetaObjectEmbeddedInSLType typeScope obj = do
	(obj', recordedTypes, recordedTerms) <- runTLFMonadInSLFMonad (formatMetaObjectAsTL' obj)
	runTLFMonadInSLFMonad $ sequence [
		recordTypeTLFMonad tlName (slName, kind)
		| (tlName, (slName, kind)) <- M.toList recordedTypes, slName `M.notMember` typeScope]
	runTLFMonadInSLFMonad $ sequence [
		recordTermTLFMonad tlName (slName, type_)
		| (tlName, (slName, type_)) <- M.toList recordedTerms]
	let typesToBind = [(tlName, SL.NameOfType (R.unNameOfSLType slName), kind)
		| (tlName, (slName, kind)) <- M.toList recordedTypes, slName `M.member` typeScope]
	let typeParams = [TL.BindingParam [(tlName, TL.MTSLType kind)] | (tlName, _, kind) <- typesToBind]
	rootName <- generateTypeNameSLFMonad typeScope
	recordTypeBindingSLFMonad (TL.Binding () rootName typeParams obj')
	return $ foldl (SL.TypeApp ())
		(SL.TypeName () rootName)
		[SL.TypeName () slName | (_, slName, _) <- typesToBind]

handleMetaObjectEmbeddedInSLTerm :: M.Map R.NameOfSLType R.SLKind
                                 -> M.Map R.NameOfSLTerm R.MetaObject
                                 -> R.MetaObject
                                 -> SLFMonad (SL.Term ())
handleMetaObjectEmbeddedInSLTerm typeScope termScope thing = do
	let (obj', recordedTypes, recordedTerms) <- runTLFMonadInSLFMonad' (formatMetaObjectAsTL' obj)
	runTLFMonadInSLFMonad $ sequence [
		recordTypeTLFMonad tlName (slName, kind)
		| (tlName, (slName, kind)) <- M.toList recordedTypes, slName `M.notMember` typeScope]
	runTLFMonadInSLFMonad $ sequence [
		recordTermTLFMonad tlName (slName, type_)
		| (tlName, (slName, type_)) <- M.toList recordedTerms, slName `M.notMember` termScope]
	let typesToBind = [(tlName, SL.NameOfType (R.unNameOfSLType slName), kind)
		| (tlName, (slName, kind)) <- M.toList recordedTypes, slName `M.member` typeScope]
	let termsToBind = [(tlName, SL.NameOfTerm (R.unNameOfSLTerm slName), type_)
		| (tlName, (slName, type_)) <- M.toList recordedTerms, slName `M.member` termScope]
	let typeParams = [TL.BindingParam [(tlName, TL.MTSLType kind)] | (tlName, _, kind) <- typesToBind]
	let termParams = [TL.BindingParam [(tlName, TL.MTSLTerm type_)] | (tlName, _, type_) <- termsToBind]
	rootName <- generateTermNameSLFMonad typeScope termScope
	recordTermBindingSLFMonad (TL.Binding () rootName (typeParams++termParams) obj')
	return $ foldl (SL.TermApp ())
		(SL.TermName () rootName [SL.TypeName () slName | (_, slName, _) <- typesToBind])
		[SL.TermName () slName [] | (_, slName, _) <- typesToBind]

