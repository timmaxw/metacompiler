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

data TLFS = TLFS {
	forbiddenNamesInTLFS :: S.Set TL.Name,
	typeParamsInTLFS :: [(BindingParam (), R.MetaObject)],
	termParamsInTLFS :: [(BindingParam (), R.MetaObject)]
	}

forbidNamesForTLFS :: S.Set TL.Name -> State TLFS a -> State TLFS a
forbidNamesForTLFS newFns inner = do
	TLFS oldFns typs teps <- get
	let fns' = newFns `S.union` oldFns
	let (result, TLFS _ typs' teps') = runState inner (TLFS fns' typs teps)
	put (TLFS oldFns typs' teps')
	return result

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

data SLFS = SLFS {
	forbiddenTypeNamesInSLFS :: S.Set SL.NameOfType,
	forbiddenTermNamesInSLFS :: S.Set SL.NameOfTerm,
	typeBindingsInSLFS :: [TL.Binding () SL.NameOfType],
	termBindingsInSLFS :: [TL.Binding () SL.NameOfTerm]
	}

formatMetaObjectAsTLSLType :: R.MetaObject -> State TLFS TL.MetaObject ()
formatMetaObjectAsTLSLType obj = case freeNamesInMetaObject obj of
	FreeNames _ _ tens | not (S.null a) -> error "how'd we get a free term name in a type?"
	FreeNames _ tyns _ | S.null tyns -> do
		let FreeNames _ ftyns _ = globalNamesInMetaObject obj
		let (obj', SLFS tybs []) = runState (formatMetaObjectAsSLType ftyns obj) (SLFS [] [])
		return (TL.MOSLTypeLiteral () obj' tybs)
	FreeNames ns tyns _ | S.null ns && not (S.null tyns) -> do
		TLFS typs teps <- get
		let candidates = [TL.Name ([c] ++ n)
			| c <- "abcdefghijklmnopqrstuvwxyz", n <- [""] ++ map show [2..]]
		let fns' = fns `S.union` S.fromList [name | TL.BindingParam () [(name, _)] <- tybs]
		let Just name = find (`S.notMember` fns') candidates
		ty' <- formatMetaTypeAsTL (R.typeOfMetaObject obj)
		let p = TL.BindingParam () [(name, ty')]
		put (TLFS (typs ++ [(p, obj)]) teps)
	FreeNames ns tyns _ | not (S.null ns) -> do
		
		

formatMetaObjectAsTLSLTerm :: R.MetaObject -> State TLFS TL.MetaObject () 
formatMetaObjectAsTLSLTerm obj = let
	FreeNames _ ftyns ftens = globalNamesInMetaObject obj
	(obj', SLFS tybs tebs) = runState (formatMetaObjectAsSLTerm ftyns ftens obj) (SLFS [] [])
	in TL.MOSLTermLiteral () obj' tybs tebs

formatMetaObjectAsSLType :: S.Set SL.NameOfType -> R.MetaObject -> State SLFS (SL.Type ())
formatMetaObjectAsSLType ftyns (R.MOSLTypeDefn defn) =
	return (SL.TypeName () (SL.NameOfType (R.unNameOfSLType (R.nameOfSLDataDefn defn))))
formatMetaObjectAsSLType ftyns (R.MOSLTypeName n k) =
	return (SL.TypeName () (SL.NameOfType (R.unNameOfSLType n)))
formatMetaObjectAsSLType ftyns (R.MOSLTypeApp f x) = do
	f' <- formatMetaObjectAsSLType ftyns f
	x' <- formatMetaObjectAsSLType ftyns x
	return (SL.TypeApp f' x')
formatMetaObjectAsSLType ftyns (R.MOSLTypeFun a r) = do
	a' <- formatMetaObjectAsSLType ftyns a
	r' <- formatMetaObjectAsSLType ftyns r
	return (SL.TypeFun a' r')
formatMetaObjectAsSLType ftyns (R.MOSLTypeLazy x) = do
	x' <- formatMetaObjectAsSLType ftyns x
	return (SL.TypeLazy x')
formatMetaObjectAsSLType ftyns other = do
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

formatMetaObjectAsSLTerm :: S.Set SL.NameOfType -> S.Set SL.NameOfTerm -> R.MetaObject -> State SLFS (SL.Term ())
formatMetaObjectAsSLTerm ftyns ftens (R.MOSLTermDefn d typs) =
	return (SL.TermName () (SL.NameOfTerm (R.unNameOfSLTerm (R.nameOfSLTermDefn d))))
formatMetaObjectAsSLTerm ftyns ftens (R.MOSLTermName n _) =
	return (SL.TermName () (SL.NameOfTerm (R.unNameOfSLTerm n)))
formatMetaObjectAsSLTerm ftyns ftens (R.MOSLTermApp f x) = do
	f' <- formatMetaObjectAsSLTerm ftyns ftens f
	x' <- formatMetaObjectAsSLTerm ftyns ftens x
	return (SL.TermApp () f' x')
formatMetaObjectAsSLTerm ftyns ftens (R.MOSLTermAbs (a, at) b) = do
	let a' = SL.NameOfTerm (R.unNameOfSLType a)
	at' <- formatMetaObjectAsSLType ftyns at
	b' <- formatMetaObjectAsSLTerm ftyns (S.insert a' ftens) b
	return (SL.TermAbs [(a', at')] b')
formatMetaObjectAsSLTerm ftyns ftens (R.MOSLTermCase s cs) = do
	s' <- formatMetaObjectAsSLTerm ftyns ftens s
	cs' <- sequence [do
		let c' = SL.NameOfCtor (R.unNameOfSLCtor (R.nameOfSLCtorDefn c))
		let fns' = [SL.NameOfTerm (R.unNameOfSLTerm n) | n <- fns]
		typs' <- mapM (formatMetaObjectAsSLType ftyns) typs
		v' <- formatMetaObjectAsSLTerm ftyns (ftens `S.union` S.fromList fns') v
		return (c', typs', fns', v')
		| (c, typs, fns, v) <- cs]
	return (SL.TermCase s' cs')
formatMetaObjectAsSLTerm ftyns ftens (R.MOSLTermData c typs teps) = do
	let c' = SL.NameOfTerm (R.unNameOfSLCtor (R.nameOfSLCtorDefn c))
	typs' <- mapM (formatMetaObjectAsSLType ftyns) typs
	teps' <- mapM (formatMetaObjectAsSLType ftyns ftens) teps
	return (foldl (SL.TermApp ()) (SL.TermName () c' typs') teps)
formatMetaObjectAsSLTerm ftyns ftens (R.MOSLTermWrap x) = do
	x' <- formatMetaObjectAsSLTerm ftyns ftens x
	return (SL.TermWrap () x')
formatMetaObjectAsSLTerm ftyns ftens (R.MOSLTermUnwrap x) = do
	x' <- formatMetaObjectAsSLTerm ftyns ftens x
	return (SL.TermUnwrap () x')
formatMetaObjectAsSLTerm ftyns ftens other = do
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

