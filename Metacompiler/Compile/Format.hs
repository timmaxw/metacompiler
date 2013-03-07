module Metacompiler.Compile.Format where

import qualified Metacompiler.Runtime as R
import qualified Metacompiler.SL.Syntax as SL
import qualified Metacompiler.TL.Syntax as TL

formatSLKindAsSL :: R.SLKind -> SL.Kind ()
formatSLKindAsSL (R.SLKindType) =
	SL.KindType ()
formatSLKindAsSL (R.SLKindFun a r) =
	case formatSLKindAsSL r of
		SL.KindFun () as' r' -> SL.KindFun (a':as') r'
		r' -> SL.KindFun () [a'] r'
	where a' = formatSLKindAsSL a

formatMetaTypeAsTL :: R.MetaType -> TL.MetaType ()
formatMetaTypeAsTL obj =
	case runState (formatMetaTypeAsTL' (metaObjectsInNames (freeAndGlobalNamesInMetaType obj)) obj) (TLFS [] []) of
		(obj', TLFS [] []) -> obj'
		_ -> error "unbound SL type/term variables at top level"

formatMetaObjectAsTL :: R.MetaObject -> TL.MetaObject ()
formatMetaObjectAsTL obj =
	case runState (formatMetaObjectAsTL' (metaObjectsInNames (freeAndGlobalNamesInMetaObject obj)) obj) (TLFS [] []) of
		(obj', TLFS [] []) -> obj'
		_ -> error "unbound SL type/term variables at top level"

-- This next part is a bit complicated:
-- 
-- `formatMetaTypeAsTL'` and `formatMetaObjectAsTL'` run in the `State TLFS` monad. The `typeBindingsInTLFS` and
-- `termBindingsInTLFS` fields will be added to but not read or otherwise modified. `forbiddenNamesInTLFS` will be both
-- read and added to.
-- 
-- Suppose that we run `(formatMetaTypeAsTL' someType)`. Let the result be `someType'`, and let the parts added to
-- `typeBindingsInTLFS` and `termBindingsInTLFS` be `types` and `terms` respectively. Then if `someType'` is evaluated
-- such that each key in `types` is bound to its value and each key in `terms` is bound to its value, then the result
-- of that evaluation will be equivalent to the original `someType`.
--
-- Each value in `typeBindingsInTLFS` will have meta-type `(R.MTSLType ...)`. Each value in `termBindingsInTLFS` will
-- have meta-type `(R.MTSLTerm ...)`. When a key-value pair is added `typeBindingsInTLFS` or `termBindingsInTLFS`, the
-- key will also be added to `forbiddenNamesInTLFS`. Names in `forbiddenNamesInTLFS` will not be used for new key-value
-- pairs.

data TLFS = TLFS {
	forbiddenNamesInTLFS :: S.Set TL.Name,
	typeBindingsInTLFS :: M.Map TL.Name R.MetaObject,
	termBindingsInTLFS :: M.Map TL.Name R.MetaObject
	}

forbidNamesForTLFS :: S.Set TL.Name -> State TLFS a -> State TLFS a
forbidNamesForTLFS newFns inner = do
	state <- get
	let innerState = state { forbiddenNamesInTLFS = names' names `S.union` forbiddenNamesInTLFS state }
	let (result, innerState') = runState inner innerState
	let state' = state { typeBindingsInTLFS = typeBindingsInTLFS innerState', termBindingsInTLFS = termBindingsInTLFS innerState' }
	put state'
	return result

bindTypeForTLFS :: R.MetaObject -> State TLFS TL.Name
bindTypeForTLFS obj = do
	state <- get
	let candidates = [TL.Name ([c] ++ n) | c <- "abcdefghijklmnopqrstuvwxyz", n <- [""] ++ map show [2..]]
	let Just name = find (`S.notMember` forbiddenNamesInTLFS state) candidates
	let state' = state {
			forbiddenNamesInTLFS = S.insert name (forbiddenNamesInTLFS state),
			typeBindingsInTLFS = M.insert name obj (typeBindingsInTLFS state)
			}
	put state'
	return name

bindTermForTLFS :: R.MetaObject -> State TLFS TL.Name
bindTypeForTLFS obj = do
	state <- get
	let candidates = [TL.Name ([c] ++ n) | c <- "abcdefghijklmnopqrstuvwxyz", n <- [""] ++ map show [2..]]
	let Just name = find (`S.notMember` forbiddenNamesInTLFS state) candidates
	let state' = state {
			forbiddenNamesInTLFS = S.insert name (forbiddenNamesInTLFS state),
			termBindingsInTLFS = M.insert name obj (termBindingsInTLFS state)
			}
	put state'
	return name

formatMetaTypeAsTL' :: R.MetaType -> State TLFS (TL.MetaType ())
formatMetaTypeAsTL' (R.MTFun (pn, pt) rt) = do
	let pn' = TL.Name (R.unNameOfMetaObject pn)
	pt' <- formatMetaTypeAsTL pt
	rt' <- forbidNamesForTLFS (S.singleton pn') $ formatMetaTypeAsTL rt
	case rt' of
		TL.MTFun () ps' rt2' -> return (TL.MTFun () ((pn', pt'):ps') rt2')
		_ -> return (TL.MTFun () [(pn', pt')] rt')
formatMetaTypeAsTL (R.MTSLType k) = do
	k' <- formatSLKindAsSL k
	return (TL.MTSLType () k')
formatMetaTypeAsTL (R.MTSLTerm t) = do
	t' <- formatMetaObjectAsTL t
	return (TL.MTSLTerm () t')
formatMetaTypeAsTL (R.MTJSExprType t) = do
	t' <- formatMetaObjectAsTL t
	return (TL.MTJSExprType () t')
formatMetaTypeAsTL (R.MTJSExpr ty te) = do
	ty' <- formatMetaObjectAsTL ty
	te' <- formatMetaObjectAsTL te
	return (TL.MTJSExpr () ty' te')

formatMetaObjectAsTL' :: R.MetaObject -> State TLFS (TL.MetaObject ())
formatMetaObjectAsTL' (R.MOApp f x) = do
	f' <- formatMetaObjectAsTL f
	x' <- formatMetaObjectAsTL x
	return (TL.MOApp () f' x')
formatMetaObjectAsTL (R.MOAbs (pn, pt) b) = do
	let pn' = TL.Name (R.unNameOfMetaObject pn)
	pt' <- formatMetaTypeAsTL pt
	b' <- forbidNamesForTLFS (S.singleton pn') $ formatMetaObjectAsTL b
	case b' of
		TL.MOAbs () ps' b2' -> return (TL.MOAbs () ((pn', pt'):ps') b2')
		_ -> return (TL.MOAbs () [(pn', pt')] b')
formatMetaObjectAsTL (R.MOName n _) =
	return TL.MOName () (TL.Name (R.unNameOfMetaObject n))
formatMetaObjectAsTL mo@(R.MOSLTypeDefn _) =
	formatMetaObjectAsTLSLType mo
formatMetaObjectAsTL mo@(R.MOSLTypeName _ _) =
	formatMetaObjectAsTLSLType mo
formatMetaObjectAsTL mo@(R.MOSLTypeApp _ _) =
	formatMetaObjectAsTLSLType mo
formatMetaObjectAsTL mo@(R.MOSLTypeFun _ _) =
	formatMetaObjectAsTLSLType mo
formatMetaObjectAsTL mo@(R.MOSLTypeLazy _) =
	formatMetaObjectAsTLSLType mo
formatMetaObjectAsTL mo@(R.MOSLTermDefn _ _) =
	formatMetaObjectAsTLSLTerm mo
formatMetaObjectAsTL mo@(R.MOSLTermName _ _) =
	formatMetaObjectAsTLSLTerm mo
formatMetaObjectAsTL mo@(R.MOSLTermApp _ _) =
	formatMetaObjectAsTLSLTerm mo
formatMetaObjectAsTL mo@(R.MOSLTermAbs _ _) =
	formatMetaObjectAsTLSLTerm mo
formatMetaObjectAsTL mo@(R.MOSLTermCase _ _) =
	formatMetaObjectAsTLSLTerm mo
formatMetaObjectAsTL mo@(R.MOSLTermData _ _) =
	formatMetaObjectAsTLSLTerm mo
formatMetaObjectAsTL mo@(R.MOSLTermWrap _ _) =
	formatMetaObjectAsTLSLTerm mo
formatMetaObjectAsTL mo@(R.MOSLTermUnwrap _ _) =
	formatMetaObjectAsTLSLTerm mo
formatMetaObjectAsTL (R.MOJSExprTypeDefn d ps) = do
	ps' <- mapM formatMetaObjectAsTL ps
	return (foldl (TL.MOApp ()) (TL.MOName () (R.nameOfJSExprTypeDefn d)) ps')
formatMetaObjectAsTL (R.MOJSExprLiteral e t c bs) = do
	e' <- formatMetaObjectAsTL e
	t' <- formatMetaObjectAsTL t
	bs' <- sequence [do
		let ps' = [TL.BindingParam () [
			(TL.Name (R.unNameOfMetaObject n1), TL.MTSLTerm () t1),
			(TL.Name (R.unNameOfMetaObject n2), TL.MTJSExpr () t2 (TL.MOName () (TL.Name (R.unNameOfMetaObject n1))))
			]
			| R.JSExprBindingParam n1 t1 n2 t2 <- ps]
		let toForbid = S.fromList $ map (TL.Name . R.unNameOfMetaObject) $
				concat [[n1, n2] | R.JSExprBindingParam n1 _ n2 _ <- ps]
		v' <- forbidNamesForTLFS toForbid $ formatMetaObjectAsTL v
		return (TL.Binding () n ps' v')
		| (n, R.JSExprBinding ps v) <- M.toList bs]
	return (TL.MOJSExprLiteral () e' t' c bs')

-- `formatMetaObjectAsSLType` and `formatMetaObjectAsSLTerm` run in the `SLFS` monad. Like with `TLFS`, the fields
-- `forbiddenTypeNamesInSLFS` and `forbiddenTermNamesInSLFS` are both read and added to, but the fields
-- `typeBindingsInSLFS` and `termBindingsInSLFS` are append-only.
--
-- Suppose that running `(formatMetaObjectAsSLTerm someTerm)` produces the result `someTerm'` while adding `types` and
-- `terms` to the `typeBindingsInSLFS` and `termBindingsInSLFS` fields of the state. Then if `someTerm'` is evaluated
-- with each key in `types` bound to its value and each key in `terms` bound to its value, the result will be
-- equivalent to the original `someTerm`.

data SLFS = SLFS {
	forbiddenTypeNamesInSLFS :: S.Set SL.NameOfType,
	forbiddenTermNamesInSLFS :: S.Set SL.NameOfTerm,
	typeBindingsInSLFS :: M.Map SL.NameOfType R.MetaObject,
	termBindingsInSLFS :: M.Map SL.NameOfType R.MetaObject
	}

forbidTermNamesForSLFS :: S.Set TL.NameOfTerm -> State TLFS a -> State TLFS a
forbidTermNamesForSLFS names inner = do
	state <- get
	let innerState = state { forbiddenTermNamesInSLFS = names' names `S.union` forbiddenTermNamesInSLFS state }
	let (result, innerState') = runState inner innerState
	let state' = state { typeBindingsInSLFS = typeBindingsInSLFS innerState', termBindingsInSLFS = termBindingsInSLFS innerState' }
	put state'
	return result

bindTypeForSLFS :: R.MetaObject -> State SLFS SL.NameOfType
bindTypeForSLFS obj = do
	state <- get
	let candidates = [SL.NameOfType ([c] ++ n) | c <- "ABCDEFGHIJKLMNOPQRSTUVWXYZ", n <- [""] ++ map show [2..]]
	let Just name = find (`S.notMember` forbiddenTypeNamesInSLFS state) candidates
	let state' = state {
			forbiddenTypeNamesInSLFS = S.insert name (forbiddenTypeNamesInSLFS state),
			typeBindingsInSLFS = M.insert name obj (typeBindingsInSLFS state)
			}
	put state'
	return name

bindTermForSLFS :: R.MetaObject -> State SLFS SL.NameOfTerm
bindTermForSLFS obj = do
	state <- get
	let candidates = [SL.NameOfTerm ([c] ++ n) | c <- "abcdefghijklmnopqrstuvwxyz", n <- [""] ++ map show [2..]]
	let Just name = find (`S.notMember` forbiddenTermNamesInSLFS state) candidates
	let state' = state {
			forbiddenTermNamesInSLFS = S.insert name (forbiddenTermNamesInSLFS state),
			termBindingsInSLFS = M.insert name obj (termBindingsInSLFS state)
			}
	put state'
	return name

formatMetaObjectAsTLSLType :: R.MetaObject -> State TLFS (TL.MetaObject ())
formatMetaObjectAsTLSLType obj = do
	(code, typeBindings, termBindings) <- formatMetaObjectAsTLSLThing formatMetaObjectAsSLType obj
	unless (M.null termBindings) (error "how did we get a term binding in a type?")
	return (TL.MOSLTypeLiteral () code typeBindings)

formatMetaObjectAsTLSLTerm :: R.MetaObject -> State TLFS (TL.MetaObject ())
formatMetaObjectAsTLSLTerm obj = do
	(code, typeBindings, termBindings) <- formatMetaObjectAsTLSLThing formatMetaObjectAsSLTerm obj
	return (TL.MOSLTypeLiteral () code typeBindings termBindings)

formatMetaObjectAsTLSLThing :: (R.MetaObject -> State SLFS a)
                            -> R.MetaObject
                            -> (State TLFS a, [TL.Binding () SL.NameOfType], [TL.Binding () SL.NameOfTerm])
formatMetaObjectAsTLSLThing fun obj = let
	FreeNamesAndTypes _ freeSLTypes freeSLTerms = freeNamesInMetaObject obj
	typeSubs = liftM M.fromList $ sequence [do
		name' <- bindTypeForTLFS (R.MOSLTypeName name kind)
		return (name, R.MOName name' (R.MTSLType kind))
		| (name, kind) <- M.toList freeSLTypes]
	termSubs = liftM M.fromList $ sequence [do
		name' <- bindTermForTLFS (R.MOSLTermName name type_)
		return (name, R.MOName name' (R.MTSLTerm type_))
		| (name, type_) <- M.toList freeSLTerms]
	let obj' = R.substituteMetaObject (Substitutions M.empty typeSubs termSubs) obj
	let (code, (SLFS _ _ typeBindings termBindings)) = runState (fun obj') (SLFS S.empty S.empty M.empty M.empty)
	typeParams <- [do
		let (value', (TLFS _ 
		| (name, value) <- typeBindings]

formatMetaObjectAsTLSLTerm :: R.MetaObject -> State TLFS TL.MetaObject () 
formatMetaObjectAsTLSLTerm obj = let
	FreeNames _ ftyns ftens = globalNamesInMetaObject obj
	(obj', SLFS tybs tebs) = runState (formatMetaObjectAsSLTerm ftyns ftens obj) (SLFS [] [])
	in TL.MOSLTermLiteral () obj' tybs tebs

formatMetaObjectAsSLType :: R.MetaObject -> State SLFS (SL.Type ())
formatMetaObjectAsSLType (R.MOSLTypeDefn defn) =
	return (SL.TypeName () (SL.NameOfType (R.unNameOfSLType (R.nameOfSLDataDefn defn))))
formatMetaObjectAsSLType (R.MOSLTypeName n k) =
	return (SL.TypeName () (SL.NameOfType (R.unNameOfSLType n)))
formatMetaObjectAsSLType (R.MOSLTypeApp f x) = do
	f' <- formatMetaObjectAsSLType f
	x' <- formatMetaObjectAsSLType x
	return (SL.TypeApp f' x')
formatMetaObjectAsSLType (R.MOSLTypeFun a r) = do
	a' <- formatMetaObjectAsSLType a
	r' <- formatMetaObjectAsSLType r
	return (SL.TypeFun a' r')
formatMetaObjectAsSLType (R.MOSLTypeLazy x) = do
	x' <- formatMetaObjectAsSLType ftyns x
	return (SL.TypeLazy x')
formatMetaObjectAsSLType other = do
	SLFS tybs tebs <- get
	let candidates = [SL.NameOfType ([c] ++ n)
		| c <- "abcdefghijklmnopqrstuvwxyz", n <- [""] ++ map show [2..]]
	let ftyns' = ftyns `S.union` S.fromList [name | TL.Binding () name _ _ <- tybs]
	let Just name = find (`S.notMember` ftyns') candidates
	(other', TLFS [] []) <-
		runState (formatMetaObjectAsTL (namesInFreeNames (freeAndGlobalNamesInMetaObject other)) other) (TLFS [] [])
	let binding = TL.Binding () name [] other'
	put (SLFS (tybs ++ [binding]) tebs)
	return (SL.TypeName () name)

formatMetaObjectAsSLTerm :: R.MetaObject -> State SLFS (SL.Term ())
formatMetaObjectAsSLTerm (R.MOSLTermDefn d typs) =
	return (SL.TermName () (SL.NameOfTerm (R.unNameOfSLTerm (R.nameOfSLTermDefn d))))
formatMetaObjectAsSLTerm (R.MOSLTermName n _) =
	return (SL.TermName () (SL.NameOfTerm (R.unNameOfSLTerm n)))
formatMetaObjectAsSLTerm (R.MOSLTermApp f x) = do
	f' <- formatMetaObjectAsSLTerm f
	x' <- formatMetaObjectAsSLTerm x
	return (SL.TermApp () f' x')
formatMetaObjectAsSLTerm (R.MOSLTermAbs (a, at) b) = do
	let a' = SL.NameOfTerm (R.unNameOfSLType a)
	at' <- formatMetaObjectAsSLType at
	b' <- formatMetaObjectAsSLTerm (S.insert a' ftens) b
	return (SL.TermAbs [(a', at')] b')
formatMetaObjectAsSLTerm (R.MOSLTermCase s cs) = do
	s' <- formatMetaObjectAsSLTerm s
	cs' <- sequence [do
		let c' = SL.NameOfCtor (R.unNameOfSLCtor (R.nameOfSLCtorDefn c))
		let fns' = [SL.NameOfTerm (R.unNameOfSLTerm n) | n <- fns]
		typs' <- mapM formatMetaObjectAsSLType typs
		v' <- formatMetaObjectAsSLTerm (ftens `S.union` S.fromList fns') v
		return (c', typs', fns', v')
		| (c, typs, fns, v) <- cs]
	return (SL.TermCase s' cs')
formatMetaObjectAsSLTerm (R.MOSLTermData c typs teps) = do
	let c' = SL.NameOfTerm (R.unNameOfSLCtor (R.nameOfSLCtorDefn c))
	typs' <- mapM formatMetaObjectAsSLType typs
	teps' <- mapM formatMetaObjectAsSLType teps
	return (foldl (SL.TermApp ()) (SL.TermName () c' typs') teps)
formatMetaObjectAsSLTerm (R.MOSLTermWrap x) = do
	x' <- formatMetaObjectAsSLTerm x
	return (SL.TermWrap () x')
formatMetaObjectAsSLTerm (R.MOSLTermUnwrap x) = do
	x' <- formatMetaObjectAsSLTerm x
	return (SL.TermUnwrap () x')
formatMetaObjectAsSLTerm other = do
	SLFS tybs tebs <- get
	let candidates = [SL.NameOfType ([c] ++ n)
		| c <- "abcdefghijklmnopqrstuvwxyz", n <- [""] ++ map show [2..]]
	let ftyns' = ftyns `S.union` S.fromList [name | TL.Binding () name _ _ <- tybs]
	let Just name = find (`S.notMember` ftyns') candidates
	(other', TLFS typs teps) <-
		runState (formatMetaObjectAsTL (namesInFreeNames (freeAndGlobalNamesInMetaObject other)) other) (TLFS [] [])
	let binding = TL.Binding () name (map fst typs ++ map fst teps) other'
	put (SLFS tybs (tebs ++ [binding]))
	return (foldl (S.TermApp ()) (S.TermName name (map snd typs)) (map snd teps))

