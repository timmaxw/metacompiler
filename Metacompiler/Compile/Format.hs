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
	case runState (formatMetaTypeAsTL' (namesInFreeNames (freeAndGlobalNamesInMetaType obj)) obj) (TLFormatState [] []) of
		(obj', TLFormatState [] []) -> obj'
		_ -> error "unbound SL type/term variables at top level"

formatMetaObjectAsTL :: R.MetaObject -> TL.MetaObject ()
formatMetaObjectAsTL obj =
	case runState (formatMetaObjectAsTL' (namesInFreeNames (freeAndGlobalNamesInMetaObject obj)) obj) (TLFormatState [] []) of
		(obj', TLFormatState [] []) -> obj'
		_ -> error "unbound SL type/term variables at top level"

data TLFormatState = TLFormatState {
	typeParamsInTLFormatState :: [(BindingParam (), R.MetaObject)],
	termParamsInTLFormatState :: [(BindingParam (), R.MetaObject)]
	}

formatMetaTypeAsTL' :: S.Set Name -> R.MetaType -> State TLFormatState (TL.MetaType ())
formatMetaTypeAsTL' fns (R.MTFun (pn, pt) rt) = do
	let pn' = TL.Name (R.unName pn)
	pt' <- formatMetaTypeAsTL fns pt
	rt' <- formatMetaTypeAsTL fns rt
	case rt' of
		TL.MTFun () ps' rt2' -> return (TL.MTFun () ((pn', pt'):ps') rt2')
		_ -> return (TL.MTFun () [(pn', pt')] rt')
formatMetaTypeAsTL fns (R.MTSLType k) = do
	k' <- formatSLKindAsSL fns k
	return (TL.MTSLType () k')
formatMetaTypeAsTL fns (R.MTSLTerm t) = do
	t' <- formatMetaObjectAsTL fns t
	return (TL.MTSLTerm () t')
formatMetaTypeAsTL fns (R.MTJSExprType t) = do
	t' <- formatMetaObjectAsTL fns t
	return (TL.MTJSExprType () t')
formatMetaTypeAsTL fns (R.MTJSExpr ty te) = do
	ty' <- formatMetaObjectAsTL fns ty
	te' <- formatMetaObjectAsTL fns te
	return (TL.MTJSExpr () ty' te')

formatMetaObjectAsTL' :: S.Set Name -> R.MetaObject -> State TLFormatState (TL.MetaObject ())
formatMetaObjectAsTL' fns (R.MOApp f x) = do
	f' <- formatMetaObjectAsTL fns f
	x' <- formatMetaObjectAsTL fns x
	return (TL.MOApp () f' x')
formatMetaObjectAsTL fns (R.MOAbs (pn, pt) b) = do
	let pn' = TL.Name (R.unName pn)
	pt' <- formatMetaTypeAsTL fns pt
	b' <- formatMetaObjectAsTL (S.insert pn' fns) b
	case b' of
		TL.MOAbs () ps' b2' -> return (TL.MOAbs () ((pn', pt'):ps') b2')
		_ -> return (TL.MOAbs () [(pn', pt')] b')
formatMetaObjectAsTL fns (R.MOName (R.Name n) _) =
	return TL.MOName () (TL.Name n)
formatMetaObjectAsTL fns mo@(R.MOSLTypeDefn _) =
	formatMetaObjectAsTLSLType fns mo
formatMetaObjectAsTL fns mo@(R.MOSLTypeName _ _) =
	formatMetaObjectAsTLSLType fns mo
formatMetaObjectAsTL fns mo@(R.MOSLTypeApp _ _) =
	formatMetaObjectAsTLSLType fns mo
formatMetaObjectAsTL fns mo@(R.MOSLTypeFun _ _) =
	formatMetaObjectAsTLSLType fns mo
formatMetaObjectAsTL fns mo@(R.MOSLTypeLazy _) =
	formatMetaObjectAsTLSLType fns mo
formatMetaObjectAsTL fns mo@(R.MOSLTermDefn _ _) =
	formatMetaObjectAsTLSLTerm fns mo
formatMetaObjectAsTL fns mo@(R.MOSLTermName _ _) =
	formatMetaObjectAsTLSLTerm fns mo
formatMetaObjectAsTL fns mo@(R.MOSLTermApp _ _) =
	formatMetaObjectAsTLSLTerm fns mo
formatMetaObjectAsTL fns mo@(R.MOSLTermAbs _ _) =
	formatMetaObjectAsTLSLTerm fns mo
formatMetaObjectAsTL fns mo@(R.MOSLTermCase _ _) =
	formatMetaObjectAsTLSLTerm fns mo
formatMetaObjectAsTL fns mo@(R.MOSLTermData _ _) =
	formatMetaObjectAsTLSLTerm fns mo
formatMetaObjectAsTL fns mo@(R.MOSLTermWrap _ _) =
	formatMetaObjectAsTLSLTerm fns mo
formatMetaObjectAsTL fns mo@(R.MOSLTermUnwrap _ _) =
	formatMetaObjectAsTLSLTerm fns mo
formatMetaObjectAsTL fns (R.MOJSExprTypeDefn d ps) = do
	ps' <- mapM (formatMetaObjectAsTL fns) ps
	return (foldl (TL.MOApp ()) (TL.MOName () (R.nameOfJSExprTypeDefn d)) ps')
formatMetaObjectAsTL fns (R.MOJSExprLiteral e t c bs) = do
	e' <- formatMetaObjectAsTL fns e
	t' <- formatMetaObjectAsTL fns t
	bs' <- sequence [do
		let ps' = [TL.BindingParam () [
			(TL.Name (R.unName n1), TL.MTSLTerm () t1),
			(TL.Name (R.unName n2), TL.MTJSExpr () t2 (TL.MOName () (TL.Name (R.unName n1))))
			]
			| R.JSExprBindingParam n1 t1 n2 t2 <- ps]
		let fns' = S.fromList (map (TL.Name . R.unName) (concat [[n1, n2] | R.JSExprBindingParam n1 _ n2 _ <- ps])) `S.union` fns
		v' <- formatMetaObjectAsTL fns' v
		return (TL.Binding () n ps' v')
		| (n, R.JSExprBinding ps v) <- M.toList bs]
	return (TL.MOJSExprLiteral () e' t' c bs')

data SLFormatState = SLFormatState {
	typeBindingsInSLFormatState :: [TL.Binding () SL.NameOfType],
	termBindingsInSLFormatState :: [TL.Binding () SL.NameOfTerm]
	}

formatMetaObjectAsTLSLType :: S.Set Name -> R.MetaObject -> State TLFormatState TL.MetaObject ()
formatMetaObjectAsTLSLType fns obj = case freeNamesInMetaObject obj of
	FreeNames _ _ tens | not (S.null a) -> error "how'd we get a free term name in a type?"
	FreeNames _ tyns _ | S.null tyns -> do
		let FreeNames _ ftyns _ = globalNamesInMetaObject obj
		let (obj', SLFormatState tybs []) = runState (formatMetaObjectAsSLType ftyns obj) (SLFormatState [] [])
		return (TL.MOSLTypeLiteral () obj' tybs)
	FreeNames ns tyns _ | S.null ns && not (S.null tyns) -> do
		TLFormatState typs teps <- get
		let candidates = [TL.Name ([c] ++ n)
			| c <- "abcdefghijklmnopqrstuvwxyz", n <- [""] ++ map show [2..]]
		let fns' = fns `S.union` S.fromList [name | TL.BindingParam () [(name, _)] <- tybs]
		let Just name = find (`S.notMember` fns') candidates
		ty' <- formatMetaTypeAsTL (R.typeOfMetaObject obj)
		let p = TL.BindingParam () [(name, ty')]
		put (TLFormatState (typs ++ [(p, obj)]) teps)
	FreeNames ns tyns _ | not (S.null ns) -> do
		
		

formatMetaObjectAsTLSLTerm :: R.MetaObject -> State TLFormatState TL.MetaObject () 
formatMetaObjectAsTLSLTerm obj = let
	FreeNames _ ftyns ftens = globalNamesInMetaObject obj
	(obj', SLFormatState tybs tebs) = runState (formatMetaObjectAsSLTerm ftyns ftens obj) (SLFormatState [] [])
	in TL.MOSLTermLiteral () obj' tybs tebs

formatMetaObjectAsSLType :: S.Set SL.NameOfType -> R.MetaObject -> State SLFormatState (SL.Type ())
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
	SLFormatState tybs tebs <- get
	let candidates = [SL.NameOfType ([c] ++ n)
		| c <- "abcdefghijklmnopqrstuvwxyz", n <- [""] ++ map show [2..]]
	let ftyns' = ftyns `S.union` S.fromList [name | TL.Binding () name _ _ <- tybs]
	let Just name = find (`S.notMember` ftyns') candidates
	(other', TLFormatState [] []) <-
		runState (formatMetaObjectAsTL (namesInFreeNames (freeAndGlobalNamesInMetaObject other)) other) (TLFormatState [] [])
	let binding = TL.Binding () name [] other'
	put (SLFormatState (tybs ++ [binding]) tebs)
	return (SL.TypeName () name)

formatMetaObjectAsSLTerm :: S.Set SL.NameOfType -> S.Set SL.NameOfTerm -> R.MetaObject -> State SLFormatState (SL.Term ())
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
	SLFormatState tybs tebs <- get
	let candidates = [SL.NameOfType ([c] ++ n)
		| c <- "abcdefghijklmnopqrstuvwxyz", n <- [""] ++ map show [2..]]
	let ftyns' = ftyns `S.union` S.fromList [name | TL.Binding () name _ _ <- tybs]
	let Just name = find (`S.notMember` ftyns') candidates
	(other', TLFormatState typs teps) <-
		runState (formatMetaObjectAsTL (namesInFreeNames (freeAndGlobalNamesInMetaObject other)) other) (TLFormatState [] [])
	let binding = TL.Binding () name (map fst typs ++ map fst teps) other'
	put (SLFormatState tybs (tebs ++ [binding]))
	return (foldl (S.TermApp ()) (S.TermName name (map snd typs)) (map snd teps))

