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
formatMetaObjectAsTL' (R.MOSLType type_ bs) = do
	
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
			return (TL.BindingParam () [(n1', TL.MTSLTerm () t1'), (n2', TL.MTJSExpr () t2' (TL.MOName () n1'))])
			| R.JSExprBindingParam n1 t1 n2 t2 <- ps]
		let toForbid = map (TL.Name . R.unNameOfMetaObject) $
				concat [[n1, n2] | R.JSExprBindingParam n1 _ n2 _ <- ps]
		v' <- pushToTLStackInTLFMonad toForbid $ formatMetaObjectAsTL' v
		return (TL.Binding () n ps' v')
		| (n, R.JSExprBinding ps v) <- M.toList bs]
	return (TL.MOJSExprLiteral () e' t' c' bs')
formatMetaObjectAsTL' (R.MOJSExprConvertEquiv outEquiv content) = do
	outEquiv' <- formatMetaObjectAsTL' outEquiv
	inEquiv' <- formatMetaObjectAsTL' (case R.typeOfMetaObject content of R.MTJSExpr _ equiv -> equiv)
	content' <- formatMetaObjectAsTL' content
	return (TL.MOJSExprConvertEquiv () inEquiv' outEquiv' content')


