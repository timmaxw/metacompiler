module Metacompiler.Compile.FormatTL where

import qualified Metacompiler.Runtime as R
import qualified Metacompiler.SL.Syntax as SL
import qualified Metacompiler.TL.Syntax as TL

data EmbeddedSL = EmbeddedSL {
	termsInEmbeddedSL :: M.Map TL.Name (SL.NameOfTerm, SL.Type ())
	}

data TLFMonad a = TLFMonad {
	runTLFMonad :: ([TL.Name], S.Set TL.Name)
	            -> (S.Set TL.Name, EmbeddedSL, a)
	}

instance Monad TLFMonad where
	return x = TLFMonad $ \ (_, forbidden) -> (forbidden, EmbeddedSL M.empty M.empty, x)
	a >>= b = TLFMonad $ \ (stack, forbidden1) ->
		case runTLFMonad a (stack, forbidden1) of
			(forbidden2, EmbeddedSL types1 terms1, x) ->
				case runTLFMonad (b x) (stack, forbidden2) of
					(forbidden3, EmbeddedSL types2 terms2, y) ->
						(forbidden3, EmbeddedSL (M.union types1 types2) (M.union terms1 terms2), y)

addNamesToScopeForTLFMonad :: [TL.Name] -> TLFMonad a -> TLFMonad a
addNamesToScopeForTLFMonad names inner =
	TLFMonad (\ (stack, forbidden) -> runTLFMonad inner (stack ++ names, forbidden))

generateNameForTLFMonad :: TLFMonad TL.Name
generateNameForTLFMonad = TLFMonad $ \ (stack, forbidden) -> let
	candidates = [TL.Name ([c] ++ n) | c <- "abcdefghijklmnopqrstuvwxyz", n <- [""] ++ map show [2..]]
	forbidden' = forbidden `S.union` S.fromList stack
	Just name = Data.List.find (`S.notMember` forbidden) candidates
	in (S.insert name forbidden, EmbeddedSL M.empty M.empty, name)

recordEmbeddedSLForTLFMonad :: EmbeddedSL -> TLFMonad ()
recordEmbeddedSLForTLFMonad esl =
	TLFMonad $ \ (stack, forbidden) -> (forbidden, esl, ())

-- `formatMetaTypeAsTL'` and `formatMetaObjectAsTL'` are the heart of the procedure.

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

-- `formatMetaObjectAsTLSLType` and `formatMetaObjectAsTLSLTerm` take a meta-object which is a SL type or SL term, and
-- return a TL term which is equivalent by wrapping it in a `MOSLTypeLiteral` or `MOSLTermLiteral`.

formatMetaObjectAsTLSLType :: R.MetaObject -> TLFMonad (TL.MetaObject ())
formatMetaObjectAsTLSLType obj = do
	(code, typeBindings, termBindings) <- formatMetaObjectAsTLSLThing formatMetaObjectAsSLType obj
	unless (M.null termBindings) (error "how did we get a term binding in a type?")
	return (TL.MOSLTypeLiteral () code typeBindings)

formatMetaObjectAsTLSLTerm :: R.MetaObject -> TLFMonad (TL.MetaObject ())
formatMetaObjectAsTLSLTerm obj = do
	(code, typeBindings, termBindings) <- formatMetaObjectAsTLSLThing formatMetaObjectAsSLTerm obj
	return (TL.MOSLTypeLiteral () code typeBindings termBindings)

formatMetaObjectAsTLSLThing :: (R.MetaObject -> SLFMonad a)
                            -> R.MetaObject
                            -> TLFMonad (a, [TL.Binding () SL.NameOfType], [TL.Binding () SL.NameOfTerm])
formatMetaObjectAsTLSLThing fun obj = do
	(_, _, embeddedTL, obj') <- runSLFMonad (fun obj) ([], S.empty, S.empty)
	return (obj', (typesInEmbeddedTL embeddedTL), (termsInEmbeddedTL embeddedTL))

