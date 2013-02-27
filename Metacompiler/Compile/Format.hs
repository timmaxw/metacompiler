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
formatMetaTypeAsTL (R.MTFun (pn, pt) rt) =
	case formatMetaTypeAsTL rt of
		TL.MTFun () ps' rt' -> TL.MTFun () ((pn', pt'):ps') rt'
		rt' -> TL.MTFun () [(pn', pt')] rt'
	where
		pn' = TL.Name (R.unName pn)
		pt' = formatMetaTypeAsTL pt
formatMetaTypeAsTL (R.MTSLType k) =
	TL.MTSLType () (formatSLKindAsSL k)
formatMetaTypeAsTL (R.MTSLTerm t) =
	TL.MTSLTerm () (formatMetaObjectAsTL t)
formatMetaTypeAsTL (R.MTJSExprType t) =
	TL.MTJSExprType () (formatMetaObjectAsTL t)
formatMetaTypeAsTL (R.MTJSExpr ty te) =
	TL.MTJSExpr () (formatMetaObjectAsTL ty) (formatMetaObjectAsTL te)

formatMetaObjectAsTL :: R.MetaObject -> TL.MetaObject ()
formatMetaObjectAsTL (R.MOApp f x) =
	TL.MOApp () (formatMetaObjectAsTL f) (formatMetaObjectAsTL x)
formatMetaObjectAsTL (R.MOAbs (pn, pt) b) =
	case formatMetaObjectAsTL b of
		TL.MOAbs () ps' b' -> TL.MOAbs () ((pn', pt'):ps') b'
		b' -> TL.MOAbs () [(pn', pt')] b'
	where
		pn' = TL.Name (R.unName pn)
		pt' = formatMetaTypeAsTL pt
formatMetaObjectAsTL (R.MOName (R.Name n) _) =
	TL.MOName () (TL.Name n)
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
formatMetaObjectAsTL (R.MOJSExprTypeDefn d ps) =
	foldl (TL.MOApp ()) (TL.MOName () (R.nameOfJSExprTypeDefn d)) (map formatMetaObjectAsTL ps)
formatMetaObjectAsTL (R.MOJSExprLiteral e t c bs) =
	TL.MOJSExprLiteral ()
		(formatMetaObjectAsTL e)
		(formatMetaObjectAsTL t)
		c
		[TL.Binding () n
			[TL.BindingParam () [
				(TL.Name (R.unName n1), TL.MTSLTerm () t1),
				(TL.Name (R.unName n2), TL.MTJSExpr () t2 (TL.MOName () (TL.Name (R.unName n1))))
				]
				| R.JSExprBindingParam n1 t1 n2 t2 <- p]
			| (n, R.JSExprBinding p v) <- M.toList bs]

data SLFormatState = SLFormatState {
	typeBindingsInSLFormatState :: [TL.Binding () SL.NameOfType],
	termBindingsInSLFormatState :: [TL.Binding () SL.NameOfTerm]
	}

formatMetaObjectAsTLSLType :: R.MetaObject -> TL.MetaObject ()
formatMetaObjectAsTLSLType obj = let
	FreeNames _ forbiddenTypeNames _ = globalNamesInMetaObject obj
	(obj', SLFormatState tybs []) = runState (formatMetaObjectAsSLType forbiddenTypeNames obj) (SLFormatState [] [])
	in TL.MOSLTypeLiteral () obj' tybs

formatMetaObjectAsTLSLTerm :: R.MetaObject -> TL.MetaObject () 
formatMetaObjectAsTLSLTerm obj = let
	FreeNames _ forbiddenTypeNames forbiddenTermNames = globalNamesInMetaObject obj
	(obj', SLFormatState tybs tebs) = runState (formatMetaObjectAsSLTerm forbiddenTypeNames forbiddenTermNames obj) (SLFormatState [] [])
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
	let binding = TL.Binding () name [] (formatMetaObjectAsTL other)
	put (SLFormatState (tybs ++ [binding]) tebs)

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
	


